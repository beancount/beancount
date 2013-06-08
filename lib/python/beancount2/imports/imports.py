"""Driver for the importers.

This is code that guesses the file types, identifies the source of data, selects
a suitable configuration, finds an import module, runs and filters it, and
outputs the imported entries. It can also rename and file documents in a
directory hierarchy. This is the driver program for importing stuff from files.
"""
import textwrap
import itertools
import re
import mimetypes
import logging
import subprocess
import bs4

from beancount2.core import data
from beancount2.core.data import format_entry
from beancount2.core.dups import find_duplicate_entries
from beancount2 import utils
from beancount2.imports import ofx_invest, ofx_bank, oanda, ameritrade, thinkorswim


#
# Getting the file types.
#

EXTRA_FILE_TYPES = [
    (re.compile(regexp, re.I), filetype)
    for regexp, filetype in (
            (r'.*\.qbo$', 'application/vnd.intu.qbo'),
            (r'.*\.(qfx|ofx)$', 'application/x-ofx'),
    )]

try:
    import magic
except ImportError:
    magic = None

def guess_file_type(filename):
    """Attempt to guess the type of the input file.
    Return a suitable mimetype, or None if we don't know."""

    # Try the regular mimetypes association.
    filetype, _ = mimetypes.guess_type(filename, False)

    if filetype is None:
        # Try out some extra ones that we know about.
        for regexp, mimtype in EXTRA_FILE_TYPES:
            if regexp.match(filename):
                filetype = mimtype
                break

    # FIXME: Add python-magic, optionally (if imported).
    if filetype is None:
        if not magic:
            pass # FIXME: issue a warning
        else:
            # Okay, we couldn't figure it out from the filename; use libmagic
            # (if installed).
            bfiletype = magic.from_file(filename, mime=True)
            filetype = bfiletype.decode()

    return filetype


#
# Identification of files to specific accounts.
#

def sliced_match(string):
    """Return a regexp that will match the given string with possibly any number of
    spaces in between. For example, '123' would become '1 *2 * 3'."""
    return '[ -]*'.join(string)


def identify_string(text, account_ids):
    """Given some string 'text', find if any of the account-ids in the 'account_ids'
    map is present in the text and return the corresponding account, or None."""

    for account_id in account_ids:
        mo = re.search(sliced_match(account_id), text)
        if mo:
            return account_id


def identify_pdf(filename, account_ids):
    "Attempt to identify the account for the given PDF file."""

    p = subprocess.Popen(('pdftotext', filename, '-'),
                         shell=False,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    if p.returncode != 0 or stderr:
        logging.error("Error running pdftotext: {}".format(stderr))
        return

    text = stdout.decode()
    return identify_string(text, account_ids)


def identify_csv(filename, account_ids):
    "Attempt to identify the account for the given CSV file."""
    text = open(filename).read()
    return identify_string(text, account_ids)


def identify_ofx(filename, __account_ids):
    "Attempt to identify the account for the given OFX file."""

    soup = bs4.BeautifulSoup(open(filename), 'lxml')
    acctid = ofx_get_account(soup)
    try:
        return acctid
    except KeyError:
        return None


IDENTIFY_HANDLERS = {
    'application/pdf'          : identify_pdf,
    'text/csv'                 : identify_csv,
    'application/x-ofx'        : identify_ofx,
    'application/vnd.intu.qbo' : identify_ofx,
}


def identify_account(filename, entries, account_ids):
    """Given a filename, return the filetype and account that this file corresponds
    to, or None if it could not be identified."""

    # Attempt to find the corresponding file type.
    filetype = guess_file_type(filename)

    identify_fun = IDENTIFY_HANDLERS.get(filetype, None)
    if identify_fun is None:
        # No handler was found; bail out.
        return

    # Attempt to find the corresponding account.
    account_id = identify_fun(filename, account_ids)

    # Attempt to find the corresponding institution.
    institution = None
    contents = open(filename).read()
    header = contents[:4096]
    if filetype == 'application/x-ofx':
        # Test for signature strings... we could go further and look at the
        # bankid/org ofx tags.
        if re.search(r'\bVanguard\b', header):
            institution = 'vanguard'
        elif re.search(r'\b011103093\b', header):
            institution = 'td'
    elif filetype == 'text/csv':
        if re.match(r'Ticket\b.*\bPipettes\b', header):
            institution = 'oanda'
        elif re.search(r'MONEY MARKET PURCHASE \(MMDA1\)', header):
            institution = 'ameritrade'
        elif re.search(r'DATE,TIME,TYPE,REF #,DESCRIPTION,FEES,COMMISSIONS,AMOUNT,BALANCE', header):
            institution = 'thinkorswim'

    return (institution, filetype, account_id)


# FIXME: This can be done via an import and a standard entry point under beancounts.importer.NAME

IMPORTERS = {
    'vanguard'    : ofx_invest,
    'td'          : ofx_bank,
    'oanda'       : oanda,
    'ameritrade'  : ameritrade,
    'thinkorswim' : thinkorswim,
}


def run_importer(importer_config, files_or_directories, output,
                 entries=[], mindate=None):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, identify them, try to find a suitable importer
    and run it on the files. A list of entries for an existing ledger can be
    provided in order to perform de-duplication and a minimum date can be
    provided to filter out old entries.
    """

    # Get the list of account-ids.
    account_ids = [account_id for (_, _, account_id) in importer_config if account_id]

    for filename in utils.walk_files_or_dirs(files_or_directories):

        # Figure out the account's filetype and account-id.
        identification = identify_account(filename, entries, account_ids)
        if not identification:
            continue # Skip file.
        institution, filetype, account_id = identification

        # FIXME: Maybe the identification triple should be (mimetype, institution, account-id)?

        # Get the relevant importer function/module.
        importer = IMPORTERS.get(institution)
        if importer is None:
            logging.warn("No importer available for '{}'; id = {}.".format(filename,
                                                                           identification))
            continue #

        accounts = importer_config[identification]

        # Run the importer.
        new_entries, annotations = importer.import_file(filename, accounts, entries)

        # Filter out entries with dates before 'mindate'.
        if mindate:
            new_entries = list(itertools.dropwhile(lambda x: x.date < opts.mindate,
                                                   new_entries))

        # Find potential matching entries.
        duplicate_entries = find_duplicate_entries(new_entries, entries)

        pr = lambda *args: print(*args, file=output)
        pr(';;')
        pr(';; {}'.format(filename))
        pr(';; ({}, {}, {})'.format(institution, filetype, account_id))
        pr(';;\n')

        # Ensure that the entries are typed correctly.
        for entry in new_entries:
            data.sanity_check_types(entry)

        # Pr out the entries.
        for entry in new_entries:
            entry_string = format_entry(entry)

            # Indicate that this entry may be a duplicate.
            if entry in duplicate_entries:
                pr(';;;; POTENTIAL DUPLICATE ENTRY')
                entry_string = textwrap.indent(entry_string, ';; ')

            pr(entry_string)
