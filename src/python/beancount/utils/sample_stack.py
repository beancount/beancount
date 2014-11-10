"""Tools for sampling stacks.

This is used to figure out how to pass in low-level dependencies from options.
Let's say I have a variable used deep down in the low-level classes, that I'd
like to have passed from a high-level function which has access to the list of
parsed options. It would be nice to know all the invocation points that trigger
the low-level functions. With the utilities in this file, I can just pepper
those locations with stack samplers which tell me where those low-level functions
are invoked.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import shelve
import sys
import traceback

from beancount.utils import test_utils

REPO_ROOT = test_utils.find_repository_root(__file__)

def sample(filename, limit=5):
    """Store the current stack and increment a counter.

    Args:
      filename: The name of the database to store stack samples to.
      limit: The number of stack frames to cull to.
    """
    stack = traceback.extract_stack(limit=limit+1)
    for index, frame in enumerate(stack):
        frame_filename, _, _, _ = frame
        if (frame_filename.startswith(REPO_ROOT) and
            not frame_filename.endswith('_test.py')):
            break
    small_stack = stack[index:-1]
    stack_str = ''.join(traceback.format_list(small_stack))
    with shelve.open(filename, 'c') as database:
        if stack_str not in database:
            database[stack_str] = 0
        database[stack_str] += 1

def print_samples(filename, file=None):
    """Dump the contents of a sample stack.

    Args:
      filename: The name of the database the stack samples are stored to.
      file: A file object, where to output the report. If None, sys.stdout.
    """
    outfile = file or sys.stdout
    print(file=outfile)
    with shelve.open(filename, 'r') as database:
        for stack, count in database.items():
            print("Seen {} times:".format(count), file=outfile)
            print(stack, file=outfile)
            print(file=outfile)


def main():
    import argparse, logging
    parser = argparse.ArgumentParser()
    parser.add_argument('filenames', nargs='+', help='Sample databases to print.')
    opts = parser.parse_args()

    for filename in opts.filenames:
        print_samples(filename)


if __name__ == '__main__':
    main()
