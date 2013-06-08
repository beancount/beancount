"""Driver for the importers.

This is code that guesses the file types, identifies the source of data, selects
a suitable configuration, finds an import module, runs and filters it, and
outputs the imported entries. It can also rename and file documents in a
directory hierarchy. This is the driver program for importing stuff from files.
"""
import textwrap
import itertools
import re
import logging
import subprocess
import bs4
import importlib

from beancount2.core import data
from beancount2.core.data import format_entry
from beancount2.core.dups import find_duplicate_entries
from beancount2 import utils
from beancount2.imports.filetype import guess_file_type
from beancount2.imports import ofx_bank


#
# Identification of files to specific accounts.
#

def sliced_match(string):
    """Return a regexp that will match the given string with possibly any number of
    spaces in between. For example, '123' would become '1 *2 * 3'. This is used
    to grep a file for broken-up ids, such as '123456789' appearing as '123
    45-6789'.
    """
    return '[ \-\­]*'.join(string)


def find_account_ids(contents, filetype, all_account_ids):
    """Given file contents, yield all the account ids found from all_account_ids."""

    # Try to find corresponding account-ids, using the list of those we already
    # know about from the importers config.
    if filetype in ('application/pdf', 'text/csv'):
        for account_id in all_account_ids:
            mo = re.search(sliced_match(account_id), contents)
            if mo:
                yield account_id

    elif filetype in ('application/x-ofx', 'application/vnd.intu.qbo'):
        soup = bs4.BeautifulSoup(contents, 'lxml')
        acctids = soup.find_all('acctid')
        for acctid in acctids:
            # There's some garbage in here sometimes; clean it up.
            yield acctid.text.split('\n')[0]


def guess_institution(contents, filetype, all_sources):
    """Attempt to guess the institution of the file in contents.
    Returns an institution ID."""

    # Read the file contents, grab some of the header.
    for source in all_sources:
        if source.is_matching_file(contents, filetype):
            return source.ID


def read_file(filename):
    """Read the file contents in a format that it can be examined."""

    # Get the filetype.
    filetype = guess_file_type(filename)

    if filetype == 'application/pdf':
        # If the file is a PDF file, convert to text so we can grep through it.
        p = subprocess.Popen(('pdftotext', filename, '-'),
                             shell=False,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        if p.returncode != 0 or stderr:
            logging.error("Error running pdftotext: {}".format(stderr))
            contents = ''
        else:
            contents = stdout.decode()

    else:
        # Otherwise just read it.
        contents = open(filename).read()

    return contents, filetype


def identify(files_or_directories, importer_config):
    """Walk over the list of files or directories, and attempt to identify the
    filetype, institution/source, and list of account-ids for each file that
    we can grok. Yield a list of

      (filename, (institution, filetype, [list-of-account-ids]))

    """

    # Get the list of account-ids.
    all_account_ids = [account_id
                       for (_, _, account_id) in importer_config
                       if account_id]

    # Get the list of sources.
    all_sources = []
    for source, _, _ in importer_config:
        module = importlib.import_module('beancount2.imports.sources.{}'.format(source))
        all_sources.append(module)

    for filename in utils.walk_files_or_dirs(files_or_directories):

        contents, filetype = read_file(filename)

        # Figure out the account's filetype and account-id.
        account_ids = find_account_ids(contents, filetype, all_account_ids)

        # Get the institution / data source.
        institution = guess_institution(contents, filetype, all_sources)

        yield filename, (institution, filetype, list(account_ids))


def run_importer(importer_config, files_or_directories, output,
                 entries=[], mindate=None):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, identify them, try to find a suitable importer
    and run it on the files. A list of entries for an existing ledger can be
    provided in order to perform de-duplication and a minimum date can be
    provided to filter out old entries.
    """

    for filename, (institution, filetype, account_ids) in identify(files_or_directories,
                                                                   importer_config):

        print('---------------------------------------- {}'.format(filename))
        print( (institution, filetype, account_ids) )
        continue














        # institution, filetype, account_id = identification

        # Get the list of accounts for this importer id.
        try:
            account_names = importer_config[identification]
        except KeyError:
            logging.error("Configuration not foudn for id: {}".format(identification))
            continue

        # Convert account names into Account objects.
        if entries:
            # Find the account objects in the list of entries.
            accounts = {}
            all_accounts = data.gather_accounts(entries)
            error = False
            for kind, account_name in account_names.items():
                try:
                    account = all_accounts[account_name]
                    accounts[kind] = account
                except KeyError:
                    logging.error("No account found for '{}'; id = {}.".format(account, identification))
                    error = True
            if error:
                continue
        else:
            # There are no entries provided; don't bail out, just create them by name.
            accounts = {kind: data.account_from_name(account_name)
                        for kind, account_name in account_names.items()}




        # Get the relevant importer function/module.
        importer = IMPORTERS.get(institution)
        if importer is None:
            logging.warn("No importer available for '{}'; id: {}.".format(filename,
                                                                           identification))
            continue

        # Run the importer.
        new_entries, annotations = importer.import_file(filename, accounts, entries)

        # Filter out entries with dates before 'mindate'.
        if mindate:
            new_entries = list(itertools.dropwhile(lambda x: x.date < opts.mindate,
                                                   new_entries))

        # Find potential matching entries.
        duplicate_entries = find_duplicate_entries(new_entries, entries)

        pr = lambda *args: print(*args, file=output)
        # pr(';;')
        # pr(';; {}'.format(filename))
        # pr(';; ({}, {}, {})'.format(institution, filetype, account_id))
        # pr(';;\n')

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



## FIXME: You shoudl be able to identify without importing

## FIXME: One file may contain more than one account

## FIXME: validate that the configuration has all the right accounts before you call the importer.

## FIXME: Move all sources to sources/ subdirectory; add one for td, one for rbc, etc.

## FIXME: Add configuraiton in the sources to describe all required accounts




# FIXME: Figure out how obtain this list automatically...
# IMPORTERS = {
#     'td'          : ofx_bank,
#     'hsbc'        : ofx_bank,
#     'rbc'         : ofx_bank,
#     'vanguard'    : ofx_invest,
#     'oanda'       : oanda,
#     'ameritrade'  : ameritrade,
#     'thinkorswim' : thinkorswim,
# }
