"""Parse, check and realize a beancount input file.

This also measures the time it takes to run all these steps.
"""
import argparse
import logging

from beancount import load
from beancount.utils import misc_utils


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('filename', help='Beancount input filename.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print timings.')
    opts = parser.parse_args()

    if opts.verbose:
        logging.basicConfig(level=logging.INFO,
                            format='%(levelname)-8s: %(message)s')

    with misc_utils.print_time('load'):
        # Load up the file, print errors, checking and validation are invoked
        # automatically.
        entries, errors, _ = load(opts.filename, do_print_errors=True)


if __name__ == '__main__':
    main()
