"""Extract script.

Read an import script and a list of downloaded filenames or directories of
downloaded files, and for each of those files, extract transactions from it.
"""
__copyright__ = "Copyright (C) 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import itertools
import inspect
import logging
import sys
import textwrap

from beancount.core import data
from beancount.parser import printer
from beancount.ingest import similar
from beancount.ingest import identify
from beancount.ingest import cache
from beancount import loader


# The format for the header in the extracted output.
# You may override this value from your .import script.
HEADER = ';; -*- mode: beancount -*-\n'


# Name of metadata field to be set to indicate that the entry is a likely duplicate.
DUPLICATE_META = '__duplicate__'


def extract_from_file(filename, importer,
                      existing_entries=None,
                      min_date=None,
                      allow_none_for_tags_and_links=False):
    """Import entries from file 'filename' with the given matches,

    Also cross-check against a list of provided 'existing_entries' entries,
    de-duplicating and possibly auto-categorizing.

    Args:
      filename: The name of the file to import.
      importer: An importer object that matched the file.
      existing_entries: A list of existing entries parsed from a ledger, used to
        detect duplicates and automatically complete or categorize transactions.
      min_date: A date before which entries should be ignored. This is useful
        when an account has a valid check/assert; we could just ignore whatever
        comes before, if desired.
      allow_none_for_tags_and_links: A boolean, whether to allow plugins to
        generate Transaction objects with None as value for the 'tags' or 'links'
        attributes.
    Returns:
      A list of new imported entries.
    Raises:
      Exception: If there is an error in the importer's extract() method.
    """
    # Extract the entries.
    file = cache.get_file(filename)

    # Note: Let the exception through on purpose. This makes developing
    # importers much easier by rendering the details of the exceptions.
    #
    # Note: For legacy support, support calling without the existing entries.
    kwargs = {}
    if 'existing_entries' in inspect.signature(importer.extract).parameters:
        kwargs['existing_entries'] = existing_entries
    new_entries = importer.extract(file, **kwargs)
    if not new_entries:
        return []

    # Make sure the newly imported entries are sorted; don't trust the importer.
    new_entries.sort(key=data.entry_sortkey)

    # Ensure that the entries are typed correctly.
    for entry in new_entries:
        data.sanity_check_types(entry, allow_none_for_tags_and_links)

    # Filter out entries with dates before 'min_date'.
    if min_date:
        new_entries = list(itertools.dropwhile(lambda x: x.date < min_date,
                                               new_entries))

    return new_entries


def find_duplicate_entries(new_entries_list, existing_entries):
    """Flag potentially duplicate entries.

    Args:
      new_entries_list: A list of pairs of (key, lists of imported entries), one
        for each importer. The key identifies the filename and/or importer that
        yielded those new entries.
      existing_entries: A list of previously existing entries from the target
        ledger.
    Returns:
      A list of lists of modified new entries (like new_entries_list),
      potentially with modified metadata to indicate those which are duplicated.
    """
    mod_entries_list = []
    for key, new_entries in new_entries_list:
        # Find similar entries against the existing ledger only.
        duplicate_pairs = similar.find_similar_entries(new_entries, existing_entries)

        # Add a metadata marker to the extracted entries for duplicates.
        duplicate_set = set(id(entry) for entry, _ in duplicate_pairs)
        mod_entries = []
        for entry in new_entries:
            if id(entry) in duplicate_set:
                marked_meta = entry.meta.copy()
                marked_meta[DUPLICATE_META] = True
                entry = entry._replace(meta=marked_meta)
            mod_entries.append(entry)
        mod_entries_list.append((key, mod_entries))
    return mod_entries_list


def print_extracted_entries(entries, file):
    """Print a list of entries.

    Args:
      entries: A list of extracted entries.
      file: A file object to write to.
    """
    # Print the filename and which modules matched.
    pr = lambda *args: print(*args, file=file)
    pr('')

    # Print out the entries.
    for entry in entries:
        # Check if this entry is a dup, and if so, comment it out.
        if DUPLICATE_META in entry.meta:
            meta = entry.meta.copy()
            meta.pop(DUPLICATE_META)
            entry = entry._replace(meta=meta)
            entry_string = textwrap.indent(printer.format_entry(entry), '; ')
        else:
            entry_string = printer.format_entry(entry)
        pr(entry_string)

    pr('')


def extract(importer_config,
            files_or_directories,
            output,
            entries=None,
            options_map=None,
            mindate=None,
            ascending=True,
            hooks=None):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them, and if it
    succeeds, run the importer on the file.

    A list of entries for an existing ledger can be provided in order to perform
    de-duplication and a minimum date can be provided to filter out old entries.

    Args:
      importer_config: A list of (regexps, importer) pairs, the configuration.
      files_or_directories: A list of strings, filenames or directories to be processed.
      output: A file object, to be written to.
      entries: A list of directives loaded from the existing file for the newly
        extracted entries to be merged in.
      options_map: The options parsed from existing file.
      mindate: Optional minimum date to output transactions for.
      ascending: A boolean, true to print entries in ascending order, false if
        descending is desired.
      hooks: An optional list of hook functions to apply to the list of extract
        (filename, entries) pairs, in order. If not specified, find_duplicate_entries()
        is used, automatically.
    """
    allow_none_for_tags_and_links = (
        options_map and options_map["allow_deprecated_none_for_tags_and_links"])

    # Run all the importers and gather their result sets.
    new_entries_list = []
    for filename, importers in identify.find_imports(importer_config,
                                                     files_or_directories):
        for importer in importers:
            # Import and process the file.
            try:
                new_entries = extract_from_file(
                    filename,
                    importer,
                    existing_entries=entries,
                    min_date=mindate,
                    allow_none_for_tags_and_links=allow_none_for_tags_and_links)
                new_entries_list.append((filename, new_entries))
            except Exception as exc:
                logging.exception("Importer %s.extract() raised an unexpected error: %s",
                                  importer.name(), exc)
                continue

    # Find potential duplicate entries in the result sets, either against the
    # list of existing ones, or against each other. A single call to this
    # function is made on purpose, so that the function be able to merge
    # entries.
    if hooks is None:
        hooks = [find_duplicate_entries]
    for hook_fn in hooks:
        new_entries_list = hook_fn(new_entries_list, entries)
    assert isinstance(new_entries_list, list)
    assert all(isinstance(new_entries, tuple) for new_entries in new_entries_list)
    assert all(isinstance(new_entries[0], str) for new_entries in new_entries_list)
    assert all(isinstance(new_entries[1], list) for new_entries in new_entries_list)

    # Print out the results.
    output.write(HEADER)
    for key, new_entries in new_entries_list:
        output.write(identify.SECTION.format(key))
        output.write('\n')
        if not ascending:
            new_entries.reverse()
        print_extracted_entries(new_entries, output)


DESCRIPTION = "Extract transactions from downloads"


def add_arguments(parser):
    """Add arguments for the extract command."""

    parser.add_argument('-e', '-f', '--existing', '--previous', metavar='BEANCOUNT_FILE',
                        default=None,
                        help=('Beancount file or existing entries for de-duplication '
                              '(optional)'))

    parser.add_argument('-r', '--reverse', '--descending',
                        action='store_const', dest='ascending',
                        default=True, const=False,
                        help='Write out the entries in descending order')


def run(args, _, importers_list, files_or_directories, hooks=None):
    """Run the subcommand."""

    # Load the ledger, if one is specified.
    if args.existing:
        entries, _, options_map = loader.load_file(args.existing)
    else:
        entries, options_map = None, None

    extract(importers_list, files_or_directories, sys.stdout,
            entries=entries,
            options_map=options_map,
            mindate=None,
            ascending=args.ascending,
            hooks=hooks)
    return 0
