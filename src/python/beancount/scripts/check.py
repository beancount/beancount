"""Parse, check and realize a beancount input file.

This also measures the time it takes to run all these steps.
"""
import argparse
import logging
import sys

from beancount import load
from beancount.utils import misc_utils
from beancount.parser import printer
from beancount.ops import validation


def main():
    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument('filename',
                        help='Beancount input filename.')

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Print timings.')

    opts = parser.parse_args()

    if opts.verbose:
        logging.basicConfig(level=logging.INFO,
                            format='%(levelname)-8s: %(message)s')

    # Force hardcore validations, just for check.
    validation.VALIDATIONS.extend(validation.HARDCORE_VALIDATIONS)

    with misc_utils.log_time('beancount.loader (total)', logging.info):
        # Load up the file, print errors, checking and validation are invoked
        # automatically.
        entries, errors, _ = load(opts.filename,
                                  log_timings=logging.info,
                                  log_errors=sys.stderr)


if __name__ == '__main__':
    main()
