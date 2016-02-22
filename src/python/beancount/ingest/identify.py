"""Identify script.

Read an import script and a list of downloaded filenames or directories of
downloaded files, and for each of those files, identify which importer it should
be associated with.
"""
__author__ = "Martin Blais <blais@furius.ca>"

from os import path
import argparse
import contextlib
import logging
import os
import pprint
import re
import runpy
import sys
import textwrap

from beancount.utils import file_utils
from beancount.ingest import scripts_utils
from beancount.ingest import cache


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
            logfile.write('**** {}\n'.format(filename))

        # Skip files that are simply too large.
        size = os.path.getsize(filename)
        if size > FILE_TOO_LARGE_THRESHOLD:
            logging.warning("File too large: '{}' ({} bytes); skipping.".format(
                filename, size))
            continue

        # For each of the sources the user has declared, identify which
        # match the text.
        file = cache.FileMemo(filename)
        matching_importers = [importer
                              for importer in importer_config
                              if importer.identify(file)]

        yield (filename, matching_importers)

        if logfile is not None:
            logfile.write('\n\n')


def identify(importer_config, files_or_directories):
    """Run the identification loop.

    Args:
      importer_config: A list of importer instances.
      files_or_directories: A list of strings, files or directories.
    """
    for filename, importers in find_imports(importer_config, files_or_directories,
                                            logfile=sys.stdout):
        for importer in importers:
            print('')
            print('  {}'.format(importer.name()))


def main():
    parser = scripts_utils.create_arguments_parser("Identify files for import")
    _, config, downloads_directories = scripts_utils.parse_arguments(parser)
    identify(config, downloads_directories)
