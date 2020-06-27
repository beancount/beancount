"""File utilities.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import contextlib
import logging
import os
import time


def find_files(fords,
               ignore_dirs=('.hg', '.svn', '.git'),
               ignore_files=('.DS_Store',)):
    """Enumerate the files under the given directories, stably.

    Invalid file or directory names will be logged to the error log.

    Args:
      fords: A list of strings, file or directory names.
      ignore_dirs: A list of strings, filenames or directories to be ignored.
    Yields:
      Strings, full filenames from the given roots.
    """
    if isinstance(fords, str):
        fords = [fords]
    assert isinstance(fords, (list, tuple))
    for ford in fords:
        if path.isdir(ford):
            for root, dirs, filenames in os.walk(ford):
                dirs[:] = sorted(dirname for dirname in dirs if dirname not in ignore_dirs)
                for filename in sorted(filenames):
                    if filename in ignore_files:
                        continue
                    yield path.join(root, filename)
        elif path.isfile(ford) or path.islink(ford):
            yield ford
        elif not path.exists(ford):
            logging.error("File or directory '{}' does not exist.".format(ford))


def guess_file_format(filename, default=None):
    """Guess the file format from the filename.

    Args:
      filename: A string, the name of the file. This can be None.
    Returns:
      A string, the extension of the format, without a leading period.
    """
    if filename:
        if filename.endswith('.txt') or filename.endswith('.text'):
            format = 'text'
        elif filename.endswith('.csv'):
            format = 'csv'
        elif filename.endswith('.html') or filename.endswith('.xhtml'):
            format = 'html'
        else:
            format = default
    else:
        format = default
    return format


def path_greedy_split(filename):
    """Split a path, returning the longest possible extension.

    Args:
      filename: A string, the filename to split.
    Returns:
      A pair of basename, extension (which includes the leading period).
    """
    basename = path.basename(filename)
    index = basename.find('.')
    if index == -1:
        extension = None
    else:
        extension = basename[index:]
        basename = basename[:index]
    return (path.join(path.dirname(filename), basename), extension)


def touch_file(filename, *otherfiles):
    """Touch a file and wait until its timestamp has been changed.

    Args:
      filename: A string path, the name of the file to touch.
      otherfiles: A list of other files to ensure the timestamp is beyond of.
    """
    # Note: You could set os.stat_float_times() but then the main function would
    # have to set that up as well. It doesn't help so much, however, since
    # filesystems tend to have low resolutions, e.g. one second.
    orig_mtime_ns = max(os.stat(minfile).st_mtime_ns
                        for minfile in (filename,) + otherfiles)
    delay_secs = 0.05
    while True:
        with open(filename, 'a'):
            os.utime(filename)
        time.sleep(delay_secs)
        new_stat = os.stat(filename)
        if new_stat.st_mtime_ns > orig_mtime_ns:
            break


@contextlib.contextmanager
def chdir(directory):
    """Temporarily chdir to the given directory.

    Args:
      directory: The directory to switch do.
    Returns:
      A context manager which restores the cwd after running.
    """
    cwd = os.getcwd()
    os.chdir(directory)
    try:
        yield cwd
    finally:
        os.chdir(cwd)
