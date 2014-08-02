"""Tools for sampling stacks.

This is used to figure out how to pass in low-level dependencies from options.
Let's say I have a variable used deep down in the low-level classes, that I'd
like to have passed from a high-level function which has access to the list of
parsed options. It would be nice to know all the invocation points that trigger
the low-level functions. With the utilities in this file, I can just pepper
those locations with stack samplers which tell me where those low-level functions
are invoked.
"""
import shelve
import sys
import traceback

from beancount.utils import test_utils

repo_root = test_utils.find_repository_root(__file__)

def sample(filename, limit=5):
    """Store the current stack and increment a counter.

    Args:
      filename: The name of the database to store stack samples to.
      limit: The number of stack frames to cull to.
    """
    stack = traceback.extract_stack(limit=limit+1)
    for index, frame in enumerate(stack):
        frame_filename, _, _, _ = frame
        if (frame_filename.startswith(repo_root) and
            not frame_filename.endswith('_test.py')):
            break
    small_stack = stack[index:-1]
    stack_str = ''.join(traceback.format_list(small_stack))
    with shelve.open(filename, 'c') as db:
        if stack_str not in db:
            db[stack_str] = 0
        db[stack_str] += 1

def print_samples(filename, file=None):
    """Dump the contents of a sample stack.

    Args:
      filename: The name of the database the stack samples are stored to.
      file: A file object, where to output the report. If None, sys.stdout.
    """
    f = file or sys.stdout
    print(file=f)
    with shelve.open(filename, 'r') as db:
        for stack, count in db.items():
            print("Seen {} times:".format(count), file=f)
            print(stack, file=f)
            print(file=f)


def main():
    import argparse, logging
    parser = argparse.ArgumentParser()
    parser.add_argument('filenames', nargs='+', help='Sample databases to print.')
    opts = parser.parse_args()

    for filename in opts.filenames:
        print_samples(filename)


if __name__ == '__main__':
    main()
