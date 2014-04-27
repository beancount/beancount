"""
Generic utility packages and functions.
"""
import re
import datetime
import logging
from time import time
import contextlib
from collections import defaultdict
import os
import logging
from os import path


def walk_files_or_dirs(fords, ignore_dirs=['.hg', '.svn', '.git']):
    """Enumerate the files under the given directories."""
    for ford in fords:
        if path.isdir(ford):
            for root, dirs, filenames in os.walk(ford):
                dirs[:] = [dirname for dirname in dirs if dirname not in ignore_dirs]
                for filename in filenames:
                    yield path.join(root, filename)
        elif path.isfile(ford) or path.islink(ford):
            yield ford
        elif not path.exists(ford):
            logging.error("File or directory '{}' does not exist.".format(ford))


class DateIntervalTicker:
    """An object that will tick when the dates cross specific intervals."""

    def __init__(self, compute_value):
        self.last_value = None

        # Compute the new tick value from the date; default implementation
        # returns the date itself, thus ticking every time the date changes.
        if compute_value is None:
            compute_value = lambda new_date: new_date
        self.compute_value = compute_value

    def __call__(self, new_date):
        """Return True if the interval has been crossed; False otherwise."""
        new_value = self.compute_value(new_date)
        if new_value != self.last_value:
            self.last_value = new_value
            return True
        else:
            return False


