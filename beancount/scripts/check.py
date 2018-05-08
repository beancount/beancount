"""Parse, check and realize a beancount input file.

This also measures the time it takes to run all these steps.
"""
__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import sys

from beancount import loader
from beancount.ops import validation
from beancount.utils import misc_utils
from beancount.utils import version


def main():
    parser = version.ArgumentParser(description=__doc__)

    parser.add_argument('filename',
                        help='Beancount input filename.')

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Print timings.')

    opts = parser.parse_args()

    if opts.verbose:
        logging.basicConfig(level=logging.INFO,
                            format='%(levelname)-8s: %(message)s')

    with misc_utils.log_time('beancount.loader (total)', logging.info):
        # Load up the file, print errors, checking and validation are invoked
        # automatically.
        entries, errors, _ = loader.load_file(
            opts.filename,
            log_timings=logging.info,
            log_errors=sys.stderr,
            # Force slow and hardcore validations, just for check.
            extra_validations=validation.HARDCORE_VALIDATIONS)

    # Exit with an error code if there were any errors, so this can be used in a
    # shell conditional.
    return 1 if errors else 0


if __name__ == '__main__':
    sys.exit(main())
