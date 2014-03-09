"""Driver for the code that files away imported files under a directory
hierarchy mirroring the chart of accounts.
"""
from os import path
import datetime
import logging
import os
import re
import sys
import shutil

from beancount.imports import imports
from beancount.core import account


def trace(arg):
    sys.stdout.write(arg)
    sys.stdout.write('\n')
    sys.stdout.flush()


def run_filer_loop(importer_config,
                   files_or_directories,
                   destination,
                   dry_run=False,
                   debug=False,
                   mkdirs=False,
                   overwrite=False):
    """File importable files under a destination directory.

    Given an importer configuration object, search for files that can be
    imported under the given list of files or directories and moved them under
    the given destination directory with the date computed by the module
    prepended to the filename. If the date cannot be extracted, use a reasonable
    default for the date (e.g. the last modified time of the file itself).

    If 'mkdirs' is True, create the destination directories before moving the
    files.
    """
    if isinstance(files_or_directories, str):
        files_or_directories = [files_or_directories]

    jobs = []
    nerrors = 0
    for filename, match_text, importers in imports.find_imports(importer_config, files_or_directories):
        # If we're debugging, print out the match text.
        # This option is useful when we're building our importer configuration,
        # to figure out which patterns to create as unique signatures.
        if debug:
            trace(',--------------------------------------------------------------------------------')
            trace(match_text)
            trace('`--------------------------------------------------------------------------------')

        if not importers:
            continue

        importer = importers[0]

        # FIXME: Not sure what we're going to do about this.
        # if len(importers) > 1:
        #     logging.error("Ambiguous match. Using the first matching module ({}).".format(
        #         module.__name__))

        # Get the account corresponding to the file.
        file_account = importer.get_filing_account()

        # Compute the date from the last modified time.
        mtime = path.getmtime(filename)
        mtime_date = datetime.datetime.fromtimestamp(mtime).date()

        # Try to get the file's date by calling a module support function. The
        # module may be able to extract the date from the filename, from the
        # contents of the file itself (e.g. scraping some text from the PDF
        # contents, or grabbing the last line of a CSV file).
        file_date = None
        try:
            file_date = importer.import_date(filename, match_text)
            if file_date is None:
                raise ValueError("Invalid return date from importer {}".format(importer))
        except NotImplementedError:
            # Fallback on the last modified time of the file.
            file_date = mtime_date

        # Find out where the file needs to go.
        new_basename = '{0:%Y-%m-%d}.{1}'.format(file_date,
                                                 path.basename(idify(filename)))

        # Apply filename renaming, if implemented.
        new_basename = importer.file_rename(new_basename)

        # Prepend destination directory.
        new_filename = path.normpath(path.join(destination,
                                               file_account.replace(account.sep, os.sep),
                                               new_basename))

        # Print the filename and which modules matched.
        trace('=== {}'.format(filename))
        if importers:
            trace('')
        trace('  Account:     {}'.format(file_account))
        trace('  Importer:    {}'.format(importer.__class__.__name__ if importer else '-'))
        trace('  Mtime Date:  {}'.format(mtime_date))
        trace('  Date:        {}'.format(file_date))
        trace('  Destination: {}'.format(new_filename))
        trace('')

        # Check if the destination directory exists.
        new_dirname = path.dirname(new_filename)
        if not path.exists(new_dirname) and not mkdirs:
            logging.error("Destination directory '{}' does not exists.".format(new_dirname))
            nerrors += 1
            continue

        # Check if the destination file already exists; we don't want to clobber
        # it by accident.
        if not overwrite and path.exists(new_filename):
            logging.error("Destination file '{}' already exists.".format(new_filename))
            nerrors += 1
            continue

        jobs.append( (filename, new_filename) )

    # If there are any errors, just don't do anything at all. This is a nicer
    # behaviour than moving just *some* files.
    if dry_run or nerrors:
        return

    # Actually carry out the moving job.
    for filename, new_filename in jobs:

        # Create missing directory if required.
        new_dirname = path.dirname(new_filename)
        if mkdirs:
            if not path.exists(new_dirname):
                os.makedirs(new_dirname)
        else:
            assert path.exists(new_dirname)

        # Copy the file to its new name.
        shutil.copyfile(filename, new_filename)

        # Remove the old file.
        # (Note: we copy and remove to support cross-device moves, because it's
        # sensible that the destination will be on an encrypted device.)
        os.remove(filename)

    return jobs


def idify(string):
    """Return characters objectionable for a filename."""
    for from_, to in [(r'[ \(\)]+', '_'),
                      (r'_*\._*', '.')]:
        string = re.sub(from_, to, string)
    return string
