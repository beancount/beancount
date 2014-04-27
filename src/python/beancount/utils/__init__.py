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


def compute_ids(strings):
    """Given a sequence of strings, reduce them to corresponding ids without any
    funny characters and insure that the list of ids is unique. Yields pairs
    of (id, string) for the result."""

    string_set = set(strings)

    # Try multiple methods until we get one that has no collisions.
    for regexp, replacement in [('[^A-Za-z0-9-.]', '_'),
                                ('[^A-Za-z0-9]', ''),]:

        # Map ids to strings.
        idmap = defaultdict(list)
        for string in string_set:
            id = re.sub(regexp, replacement, string)
            idmap[id].append(string)

        # Check for collisions.
        if all(len(stringlist) == 1 for stringlist in idmap.values()):
            break
    else:
        raise RuntimeError("Could not find a unique mapping for {}".format(string_set))

    return sorted((id, stringlist[0]) for id, stringlist in idmap.items())
