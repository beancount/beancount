"""Driver for the importers.

This is code that guesses the file types, identifies the source of data, selects
a suitable configuration, finds an import module, runs and filters it, and
outputs the imported entries. It can also rename and file documents in a
directory hierarchy. This is the driver program for importing stuff from files.
"""
from pprint import pformat
import sys
import codecs
import itertools
import logging
import os
import re
import subprocess
import tempfile
import textwrap

from beancount.core import data
from beancount.core.data import format_entry
from beancount.ops.dups import find_duplicate_entries
from beancount import utils
from beancount.imports.filetype import guess_file_type
from beancount.imports import pdfconvert


FILE_TOO_LARGE_THRESHOLD = 8*1024*1024


def read_file(filename):
    """Read the file contents in a format that it can be examined."""

    # Get the filetype.
    filetype = guess_file_type(filename)

    if filetype == 'application/pdf':
        # If the file is a PDF file, convert to text so we can grep through it.
        #
        # We need to try various methods, because many tools fail to produce
        # usable output.
        for converter in pdfconvert.PDF_CONVERTERS:
            contents = converter(filename)
            if contents is not None and re.search('[a-zA-Z0-9]+', contents):
                break
        else:
            contents = None

    elif filetype == 'application/vnd.ms-excel':
        # If the file is an Excel spreadsheet, convert to a CSV file so we can
        # grep through it.
        excel_filename = filename
        while 1:
            with tempfile.NamedTemporaryFile(suffix='.csv') as f:
                command = ('ssconvert', '--export-type=Gnumeric_stf:stf_csv',
                           excel_filename, f.name)
                p = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                output, errors = p.communicate()
                if p.returncode == 0:
                    contents = open(f.name).read()
                    break
                elif re.search('XML document not well formed', errors.decode()):
                    # Try again, after fixing up some well-known erronerous strings.
                    # This manifests itself in RBC Direct Investing Excel files.
                    tidy_file = tempfile.NamedTemporaryFile(suffix='.xls', mode='w', delete=False)
                    new_contents = re.sub('S&P 500', 'S&amp;P 500', open(excel_filename).read())
                    tidy_file.write(new_contents)
                    tidy_file.flush()
                    excel_filename = tidy_file.name
                else:
                    raise IOError("Could not convert Excel file '{}': {}".format(filename, errors))

    elif filetype.startswith('image/') or filetype == 'application/zip':
        # Skip these file types.
        contents = ''

    else:
        # Attempt to guess the encoding of the file (we should use 'chardet'
        # here but we want to minimize dependencies).
        rb = open(filename, 'rb').read(12)
        if rb.startswith(codecs.BOM_UTF16):
            encoding = 'utf-16'
        else:
            encoding = None

        # Otherwise just read it as it is.
        try:
            contents = open(filename, encoding=encoding).read()
        except UnicodeDecodeError as e:
            logging.error("Error decoding '{}'.".format(filename))
            contents = ''

    return contents, filetype


def find_imports(importer_config, files_or_directories):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them and return a list
    of (filename, importers), where 'importers' is a list of importers that matched
    the file.

    Args:
      importer_config: a list of (regexps, importer) tuples that define the config.
      files_or_directories: a list of files of directories to walk recursively and
                            hunt for files to import.
    Yields:
      Triples of filename found, textified contents of the file, and list of
      importers matching this file.
    """

    match_template = textwrap.dedent("""\
           Filename: {}
           FileType: {}
           Contents: {}
        """)

    # Iterate over all files found; accumulate the entries by identification.
    for filename in utils.walk_files_or_dirs(files_or_directories):

        # Skip files that are simply too large.
        size = os.path.getsize(filename)
        if size > FILE_TOO_LARGE_THRESHOLD:
            logging.warning("File too large: '{}' ({} bytes); skipping.".format(
                filename, size))
            continue

        # Read the file in a parseable form.
        contents, filetype = read_file(filename)

        # Build up a string to match the configuration signatures against.
        match_text = match_template.format(filename, filetype, contents)

        # If in debugging mode, print out the text the signatures have to match against.

        # For each of the sources the user has declared, find the matching
        # signature sets.
        matching_importers = []
        for signatures, importer in importer_config:
            # Attempt to match all of the signatures against the text.
            if all(re.search(signature, match_text, re.DOTALL)
                   for signature in signatures):
                matching_importers.append(importer)

        yield (filename, match_text, matching_importers)


def import_file(filename, importers):
    """Import entries from a single file.

    Args:
      filename: the name of the file to import
      importers: a list of importer instances to run on this file.
    Returns:
      A list of new entries imported from the file.
    """
    # Import with each importer. This is important because different importers
    # may process distinct parts of a single file.
    new_entries = []
    for importer in importers:
        # Import the new entries.
        try:
            imported_entries = importer.import_file(filename)
            if imported_entries:
                new_entries.extend(imported_entries)
        except NotImplementedError:
            # If the module does not implement entry extraction, skip.
            pass


    # Make sure the newly imported entries are sorted; don't trust the importer.
    new_entries.sort(key=data.entry_sortkey)

    # Ensure that the entries are typed correctly.
    for entry in new_entries:
        data.sanity_check_types(entry)

    return new_entries


def import_file_and_process(filename, importers, existing_entries, mindate):
    """Import entries from file 'filename' with the given matches,
    and cross-check against a list of provided 'existing_entries' entries,
    de-duplicating and possibly auto-categorizing.

    Args:
      filename: the name of the file to import
      importers: a list of importer objects that matched the file
      existing_entries: a list of existing entries parsed from a ledger,
                        used to detect duplicates and automatically complete
                        or categorize transactions.
      mindate: a date before which entries should be ignored. This is useful
               when an account has a valid check/assert; we could just ignore
               whatever comes before, if desired.
    Returns:
      A list of new imported entries and a subset of these which have been
      identified as possible duplicates.
    """

    # Import the entries.
    new_entries = import_file(filename, importers)
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
                      entries=None, mindate=None, dry_run=False, debug=False):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them, and if it
    succeeds, run the importer on the file. This is the main import driver loop.

    A list of entries for an existing ledger can be provided in order to perform
    de-duplication and a minimum date can be provided to filter out old entries.
    """
    if isinstance(files_or_directories, str):
        files_or_directories = [files_or_directories]

    trace = lambda arg: sys.stdout.write(arg + '\n')
    for filename, match_text, importers in find_imports(importer_config, files_or_directories):
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
                                                                 importers,
                                                                 entries,
                                                                 mindate)
        if new_entries is None:
            logging.error("Error importing '{}'; no entries produced.".format(filename))
            continue

        # Print the filename and which modules matched.
        trace('\n\n**** {}'.format(filename))
        if importers:
            trace('')
        for importer in importers:
            trace('  Importer: {}'.format(importer.__class__.__name__ if importer else '-'))
            trace(textwrap.indent(pformat(importer.config), '    '))
            trace('')

        # Print out the entries.
        pr = lambda arg: output.write(arg + '\n')
        for entry in new_entries:
            entry_string = format_entry(entry)

            # Check if this entry is a dup, and if so, comment it out.
            if entries and entry in duplicate_entries:
                pr(';;;; POTENTIAL DUPLICATE')
                entry_string = textwrap.indent(entry_string, ';; ')

            pr(entry_string)
