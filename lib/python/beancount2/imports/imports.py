"""Driver for the importers.

This is code that guesses the file types, identifies the source of data, selects
a suitable configuration, finds an import module, runs and filters it, and
outputs the imported entries. It can also rename and file documents in a
directory hierarchy. This is the driver program for importing stuff from files.
"""
import codecs
import textwrap
import itertools
import tempfile
import re
import logging
import subprocess
import sys
import importlib
import datetime
from collections import defaultdict
from pprint import pprint, pformat

import bs4

from beancount2.core import data
from beancount2.core.data import format_entry
from beancount2.core.data import is_account_name, account_from_name
from beancount2.core.dups import find_duplicate_entries
from beancount2 import utils
from beancount2.imports.filetype import guess_file_type


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
        # Attempt to guess the encoding of the file (we should use 'chardet'
        # here but we want to minimize dependencies).
        rb = open(filename, 'rb').read(12)
        if rb.startswith(codecs.BOM_UTF16):
            encoding = 'utf-16'
        else:
            encoding = None

        # Otherwise just read it as it is.
        contents = open(filename, encoding=encoding).read()

    return contents, filetype


def find_imports(importer_config, files_or_directories):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them and return a list
    of (filename, matches), where 'matches' is a list of (module, module_config) for
    each importer module that matched the file."""

    match_template = textwrap.dedent("""\
           Filename: {}
           FileType: {}
           Contents: {}
        """)

    # Iterate over all files found; accumulate the entries by identification.
    all_matches = []
    for filename in utils.walk_files_or_dirs(files_or_directories):

        # Read the file in a parseable form.
        contents, filetype = read_file(filename)

        # Build up a string to match the configuration signatures against.
        match_text = match_template.format(filename, filetype, contents)

        # If in debugging mode, print out the text the signatures have to match against.

        # For each of the sources the user has declared, find the matching
        # signature sets.
        matches = []
        for signatures, module, config in importer_config:
            # Attempt to match all of the signatures against the text.
            if all(re.search(signature, match_text, re.DOTALL)
                   for signature in signatures):
                matches.append( (module, config) )

        yield (filename, match_text, matches)

    return all_matches


def verify_config(module, module_config):
    """Check the configuration account provided by the user against the accounts
    required by the source importer. Just to make sure."""

    module_options = set(module.CONFIG)
    user_options = set(module_config)

    success = True
    for option in (module_options - user_options):
        logging.error("Missing value from user configuration for module {}: {}".format(
            module.__name__, option))
        success = False

    for option in (user_options - module_options):
        logging.error("Unknown value in user configuration for module {}: {}".format(
            module.__name__, option))
        success = False

    return success


def module_config_accountify(module_config):
    """Convert module config options that have values that are account names into
    Account instances. This is a convenience designed to be used by the
    importers.
    """
    return {key: account_from_name(value) if is_account_name(value) else value
            for key, value in module_config.items()}


def import_file(filename, matches):
    """Import entries from a single file.
    Matches is a list of (module, module_config) tuples to run on this file."""


    # Import with the various modules.
    new_entries = []
    for module, module_config in matches:
        # Skip if there is no importer for this match (this occurs when we
        # have a configuration used for filing only, e.g. with PDF files).
        if module is None:
            continue

        # Verify that the user configuration has all the required module
        # configuration options.
        if not verify_config(module, module_config):
            # Skip a failing config.
            continue

        # Import the new entries.
        new_entries.extend(
            module.import_file(filename, module_config))

    # Make sure the newly imported entries are sorted; don't trust the importer.
    new_entries.sort(key=data.entry_sortkey)

    # Ensure that the entries are typed correctly.
    for entry in new_entries:
        data.sanity_check_types(entry)

    return new_entries


def import_file_and_process(filename, matches, existing_entries, mindate):
    """Import entries from file 'filename' with the given matches,
    and cross-check against a list of provided 'existing_entries' entries,
    de-duplicating and possibly auto-categorizing.

    Returns a list of new imported entries and a subset of these which have been
    identified as possible duplicates."""

    # Import the entires.
    new_entries = import_file(filename, matches)
    if new_entries is None:
        return None, None

    # Filter out entries with dates before 'mindate'.
    if mindate:
        new_entries = list(itertools.dropwhile(lambda x: x.date < mindate,
                                               new_entries))

    # Find potential matching entries.
    if existing_entries:
        duplicate_entries = find_duplicate_entries(new_entries, existing_entries)

    ## FIXME: Auto-categorize here.

    return new_entries, duplicate_entries


def run_importer_loop(importer_config,
                      files_or_directories,
                      output,
                      entries=None, mindate=None, dry_run=False):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them, and if it
    succeeds, run the importer on the file. This is the main import driver loop.

    A list of entries for an existing ledger can be provided in order to perform
    de-duplication and a minimum date can be provided to filter out old entries.
    """
    if isinstance(files_or_directories, str):
        files_or_directories = [files_or_directories]

    debug = False
    trace = lambda *args: print(*args, file=sys.stdout)
    for filename, match_text, matches in find_imports(importer_config, files_or_directories):
        # Print the filename and which modules matched.
        trace('=== {}'.format(filename))
        if matches:
            trace('')
        for module, module_config in matches:
            trace('  Importer: {}'.format(module.__name__ if module else '-'))
            trace(textwrap.indent(pformat(module_config), '    '))
            trace('')

        # If we're debugging, print out the match text.
        # This option is useful when we're building our importer configuration,
        # to figure out which patterns to create as unique signatures.
        if debug:
            trace(',--------------------------------------------------------------------------------')
            trace(match_text)
            trace('`--------------------------------------------------------------------------------')

        if dry_run:
            continue

        # Import and process the file.
        new_entries, duplicate_entries = import_file_and_process(filename,
                                                                 matches,
                                                                 entries,
                                                                 mindate)
        if new_entries is None:
            logging.error("Error importing '{}'; no entries produced.".format(filename))
            continue

        # Print out the entries.
        pr = lambda *args: print(*args, file=output)
        if new_entries:
            pr()
            pr(';; {}'.format(filename))
            pr()

        for entry in new_entries:
            entry_string = format_entry(entry)

            # Check if this entry is a dup, and if so, comment it out.
            if entries and entry in duplicate_entries:
                pr(';;;; POTENTIAL DUPLICATE')
                entry_string = textwrap.indent(entry_string, ';; ')

            pr(entry_string)
