"""Driver for the importers.

This is code that guesses the file types, identifies the source of data, selects
a suitable configuration, finds an import module, runs and filters it, and
outputs the imported entries. It can also rename and file documents in a
directory hierarchy. This is the driver program for importing stuff from files.
"""
import textwrap
import itertools
import tempfile
import re
import logging
import subprocess
import bs4
import importlib
import datetime
from collections import defaultdict
from pprint import pprint, pformat

from beancount2.core import data
from beancount2.core.data import format_entry
from beancount2.core.dups import find_duplicate_entries
from beancount2 import utils
from beancount2.imports.filetype import guess_file_type


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


def guess_source(contents, filetype, all_sources):
    """Attempt to guess the source of the file in contents.
    Returns an source ID."""

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

    elif filetype == 'application/vnd.ms-excel':
        # If the file is an Excel spreadsheet, convert to a CSV file so we can
        # grep through it.
        with tempfile.NamedTemporaryFile(suffix='.csv') as f:
            r = subprocess.call(('ssconvert', '--export-type=Gnumeric_stf:stf_csv',
                                 filename, f.name),
                                stdout=subprocess.PIPE)
            assert r == 0, r
            contents = open(f.name).read()

    else:
        # Otherwise just read it as it is.
        contents = open(filename).read()

    return contents, filetype


def import_source(source_id):
    return importlib.import_module('beancount2.imports.sources.{}'.format(source_id))


def identify(files_or_directories, importer_config):
    """Walk over the list of files or directories, and attempt to identify the
    filetype, source/source, and list of account-ids for each file that
    we can grok. Yield a list of

      (filename, (source, filetype, [list-of-account-ids]))

    """
    # Get the list of account-ids.
    all_account_ids = [account_id
                       for (_, _, account_id) in importer_config
                       if account_id]

    # Get the list of sources.
    all_sources = [import_source(source_id)
                   for (source_id, _, _) in importer_config]

    for filename in utils.walk_files_or_dirs(files_or_directories):

        contents, filetype = read_file(filename)

        # Figure out the account's filetype and account-id.
        account_ids = find_account_ids(contents, filetype, all_account_ids)
        account_ids = list(account_ids)

        # If there are no account-ids, insert a "None" to hold place for the
        # unknown account that this file is for.
        if not account_ids:
            account_ids.append(None)

        # Get the source / data source.
        source = guess_source(contents, filetype, all_sources)

        yield filename, (source, filetype, account_ids)


def get_required_accounts_for_config(config_account_names, all_accounts):
    """Given a dict of account names from the configuration, and a mapping of
    al the accounts, get the accounts required for this configuration."""

    # Find the Account objects in the list of entries, as much as possible;
    # create new ones on-demand, by name.
    accounts = {}
    error = False
    for kind, account_name in config_account_names.items():
        account = all_accounts.get(account_name, None)
        if account is None:
            account = data.account_from_name(account_name)
        accounts[kind] = account

    return accounts


def import_file(filename, identification, importer_config, entries, accounts):
    """Run the importer on 'filename' as identified in 'identification'.
    (This imports only the transactions for the account specified in the
    identification, which is of the form (source, filetype, account-if).
    """

    # Get the list of accounts for this importer id.
    try:
        config_account_names = importer_config[identification]
    except KeyError:
        logging.error("Configuration not found for id: {}".format(identification))
        return

    # Import the source module.
    source_id, filetype, account_id = identification
    source = import_source(source_id)

    # Check the configuration account provided by the user against the accounts
    # required by the source importer. Just to make sure.
    config_types = set(config_account_names)
    source_types = set(source.CONFIG_ACCOUNTS[filetype])
    for account_type in (source_types - config_types):
        logging.error("Missing account from configuration: {}".format(account_type))
    for account_type in (config_types - source_types):
        logging.error("Extra account in configuration: {}".format(account_type))

    # Convert the account names into account objects before passing it into the
    # importer.
    config = get_required_accounts_for_config(config_account_names, accounts)

    # Run the importer.
    return source.import_file(filename, config, entries)


def run_importer(importer_config, files_or_directories, output,
                 entries=[], mindate=None):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, identify them, try to find a suitable importer
    and run it on the files.

    A list of entries for an existing ledger can be provided in order to perform
    de-duplication and a minimum date can be provided to filter out old entries.
    """

    # Compute a mapping of all the available accounts in the entries from the
    # beancount file.
    accounts = data.gather_accounts(entries)

    # Printing function.
    pr = lambda *args: print(*args, file=output)

    # Iterate over all files found; accumulate the entries by identification.
    entries_byid = defaultdict(list)
    all_duplicate_entries = []
    for filename, (source, filetype, account_ids) in identify(files_or_directories,
                                                              importer_config):

        for account_id in account_ids:
            identification = (source, filetype, account_id)

            # Import entries for file for specified account.
            new_entries = import_file(filename, identification, importer_config, entries, accounts)

            if new_entries is None:
                logging.error("Error importing '{}'; no entries produced.".format(filename))
                continue

            new_entries.sort(key=lambda x: x.date)

            # Filter out entries with dates before 'mindate'.
            if mindate:
                new_entries = list(itertools.dropwhile(lambda x: x.date < opts.mindate,
                                                       new_entries))

            # Save entries for printing later.
            entries_byid[identification].extend(new_entries)

            # Find potential matching entries.
            duplicate_entries = find_duplicate_entries(new_entries, entries)
            all_duplicate_entries.extend(duplicate_entries)

            # Ensure that the entries are typed correctly.
            for entry in new_entries:
                data.sanity_check_types(entry)

    # Print out the entries by identification.
    for identification, entries in sorted(entries_byid.items()):
        print('')
        print(';;; IMPORT {} on {}'.format(identification, datetime.date.today()))
        print('')

        # Sort all the source's entries.
        entries.sort(key=data.entry_sortkey)

        # Print out the entries.
        for entry in entries:
            entry_string = format_entry(entry)

            # Indicate that this entry may be a duplicate.
            if entry in all_duplicate_entries:
                pr(';;;; POTENTIAL DUPLICATE')
                entry_string = textwrap.indent(entry_string, ';; ')

            pr(entry_string)




def run_importer2(importer_config, files_or_directories, output,
                  entries=[], mindate=None, debug=False):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them, and if it
    succeeds, run the importer on the file. This is the main import driver loop.

    A list of entries for an existing ledger can be provided in order to perform
    de-duplication and a minimum date can be provided to filter out old entries.
    """

    # # Compute a mapping of all the available accounts in the entries from the
    # # beancount file.
    # accounts = data.gather_accounts(entries)


    # Printing function.
    pr = lambda *args: print(*args, file=output)

    # Iterate over all files found; accumulate the entries by identification.
    entries_byid = defaultdict(list)
    all_duplicate_entries = []
    for filename in utils.walk_files_or_dirs(files_or_directories):
        print(filename)

        # Read the file in a parseable form.
        contents, filetype = read_file(filename)

        # Build up a string to match the configuration signatures against.
        match_text = textwrap.dedent("""\
        Filename: {}
        FileType: {}
        Contents: {}
        """).format(filename, filetype, contents)

        # If in debugging mode, print out the text the signatures have to match against.
        if debug:
            print(',--------------------------------------------------------------------------------')
            print(match_text)
            print('`--------------------------------------------------------------------------------')

        # For each of the sources the user has declared, find the matching
        # signature sets.
        matches = []
        for signatures, module, config in importer_config:
            # Attempt to match all of the signatures against the text.
            if all(re.search(signature, match_text, re.DOTALL)
                   for signature in signatures):
                matches.append( (module, config) )

        if matches:
            print()
        for module, accounts in matches:
            print('  Importer: {}'.format(module.__name__ if module else '-'))
            print(textwrap.indent(pformat(accounts), '    '))
            print()








    # for filename, (source, filetype, account_ids) in identify(files_or_directories,
    #                                                           importer_config):

    #     for account_id in account_ids:
    #         identification = (source, filetype, account_id)

    #         # Import entries for file for specified account.
    #         new_entries = import_file(filename, identification, importer_config, entries, accounts)

    #         if new_entries is None:
    #             logging.error("Error importing '{}'; no entries produced.".format(filename))
    #             continue

    #         new_entries.sort(key=lambda x: x.date)

    #         # Filter out entries with dates before 'mindate'.
    #         if mindate:
    #             new_entries = list(itertools.dropwhile(lambda x: x.date < opts.mindate,
    #                                                    new_entries))

    #         # Save entries for printing later.
    #         entries_byid[identification].extend(new_entries)

    #         # Find potential matching entries.
    #         duplicate_entries = find_duplicate_entries(new_entries, entries)
    #         all_duplicate_entries.extend(duplicate_entries)

    #         # Ensure that the entries are typed correctly.
    #         for entry in new_entries:
    #             data.sanity_check_types(entry)

    # # Print out the entries by identification.
    # for identification, entries in sorted(entries_byid.items()):
    #     print('')
    #     print(';;; IMPORT {} on {}'.format(identification, datetime.date.today()))
    #     print('')

    #     # Sort all the source's entries.
    #     entries.sort(key=data.entry_sortkey)

    #     # Print out the entries.
    #     for entry in entries:
    #         entry_string = format_entry(entry)

    #         # Indicate that this entry may be a duplicate.
    #         if entry in all_duplicate_entries:
    #             pr(';;;; POTENTIAL DUPLICATE')
    #             entry_string = textwrap.indent(entry_string, ';; ')

    #         pr(entry_string)
