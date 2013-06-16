"""Driver for the code that files away imported files under a directory
hierarchy mirroring the chart of accounts.
"""
from os import path
import datetime
import logging
import os
import sys
import shutil

from beancount2.imports import imports


def run_filer_loop(importer_config,
                   files_or_directories,
                   destination,
                   dry_run=False,
                   mkdirs=False):
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

    trace = lambda *args: print(*args, file=sys.stdout)
    jobs = []
    nerrors = 0
    for filename, match_text, matches in imports.find_imports(importer_config, files_or_directories):
        if not matches:
            continue

        module, module_config = matches[0]
        # if len(matches) > 1:
        #     logging.error("Ambiguous match. Using the first matching module ({}).".format(
        #         module.__name__))

        # Get the account corresponding to the file.
        file_account = module_config['FILE']

        # Compute the date from the last modified time.
        mtime = path.getmtime(filename)
        mtime_date = datetime.datetime.fromtimestamp(mtime).date()

        # Try to get the file's date by calling a module support function. The
        # module may be able to extract the date from the filename, from the
        # contents of the file itself (e.g. scraping some text from the PDF
        # contents, or grabbing the last line of a CSV file).
        file_date = None
        if hasattr(module, 'import_date'):
            file_date = module.import_date(filename, match_text)
        if file_date is None:
            # Fallback on the last modified time of the file.
            file_date = mtime_date

        # Find out where the file needs to go.
        new_filename = path.normpath(path.join(
            destination,
            file_account.replace(':', os.sep),
            '{0:%Y-%m-%d}.{1}'.format(file_date,
                                      path.basename(filename))))

        # Print the filename and which modules matched.
        trace('=== {}'.format(filename))
        if matches:
            trace('')
        for _, _module_config in matches:
            trace('  Account:     {}'.format(_module_config['FILE']))
        trace('  Importer:    {}'.format(module.__name__ if module else '-'))
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
        if path.exists(new_filename):
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
        assert not path.exists(new_filename)
        shutil.copyfile(filename, new_filename)

        # Remove the old file.
        # (Note: we copy and remove to support cross-device moves, because it's
        # sensible that the destination will be on an encrypted device.)
        os.remove(filename)

    return jobs
