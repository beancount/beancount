"""Identify script.

Read an import script and a list of downloaded filenames or directories of
2downloaded files, and for each of those files, identify which importer it should
be associated with.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import sys
from os import path

from beancount.utils import file_utils
from beancount.ingest import cache


# The format for the section titles in the extracted output.
# You may override this value from your .import script.
SECTION = '**** {}'


# A file size beyond which we will simply ignore the file. This is used to skip
# large files that are commonly co-present in a Downloads directory.
FILE_TOO_LARGE_THRESHOLD = 8*1024*1024


def find_imports(importer_config, files_or_directories, logfile=None):
    """Given an importer configuration, search for files that can be imported in the
    list of files or directories, run the signature checks on them and return a list
    of (filename, importers), where 'importers' is a list of importers that matched
    the file.

    Args:
      importer_config: a list of importer instances that define the config.
      files_or_directories: a list of files of directories to walk recursively and
                            hunt for files to import.
      logfile: A file object to write log entries to, or None, in which case no log is
        written out.
    Yields:
      Triples of filename found, textified contents of the file, and list of
      importers matching this file.
    """
    # Iterate over all files found; accumulate the entries by identification.
    for filename in file_utils.find_files(files_or_directories):
        if logfile is not None:
            logfile.write(SECTION.format(filename))
            logfile.write('\n')

        # Skip files that are simply too large.
        size = path.getsize(filename)
        if size > FILE_TOO_LARGE_THRESHOLD:
            logging.warning("File too large: '{}' ({} bytes); skipping.".format(
                filename, size))
            continue

        # For each of the sources the user has declared, identify which
        # match the text.
        file = cache.get_file(filename)
        matching_importers = []
        for importer in importer_config:
            try:
                matched = importer.identify(file)
                if matched:
                    matching_importers.append(importer)
            except Exception as exc:
                logging.exception("Importer %s.identify() raised an unexpected error: %s",
                                  importer.name(), exc)

        yield (filename, matching_importers)


def identify(importers_list, files_or_directories):
    """Run the identification loop.

    Args:
      importers_list: A list of importer instances.
      files_or_directories: A list of strings, files or directories.
    """
    logfile = sys.stdout
    for filename, importers in find_imports(importers_list, files_or_directories,
                                            logfile=logfile):
        file = cache.get_file(filename)
        for importer in importers:
            logfile.write('Importer:    {}\n'.format(importer.name() if importer else '-'))
            logfile.write('Account:     {}\n'.format(importer.file_account(file)))
            logfile.write('\n')


DESCRIPTION = "Identify files for import"


def add_arguments(parser):
    """Add arguments for the identify command."""


def run(_, __, importers_list, files_or_directories, hooks=None):
    """Run the subcommand."""
    return identify(importers_list, files_or_directories)
