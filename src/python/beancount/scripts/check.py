"""Parse, check and realize a beancount input file.

This also measures the time it takes to run all these steps.
"""
import argparse
import logging

from beancount import load
from beancount.core import realization
from beancount import utils


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('filename', help='Beancount input filename.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print timings.')

    opts = parser.parse_args()

    if opts.verbose:
        logging.basicConfig(level=logging.INFO,
                            format='%(levelname)-8s: %(message)s')

    with utils.print_time('total'):

        # Load up the file, print errors.
        entries, errors, options = load(opts.filename, do_print_errors=True)

        # Realize the entries.
        with utils.print_time('realize'):
            real_accounts = realization.realize(entries)


if __name__ == '__main__':
    main()
