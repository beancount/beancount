__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import sys

import click

from beancount import loader
from beancount.ops import validation
from beancount.utils import misc_utils
from beancount.parser import printer
from beancount.parser.version import VERSION

@click.command()
@click.argument('filename', type=click.Path())
@click.option('--verbose', '-v', is_flag=True, help='Print timings.')
@click.option('--no-cache', '-C', is_flag=True, help='Disable the cache.')
@click.option('--cache-filename', type=click.Path(), help='Override the cache filename.')
@click.version_option(message=VERSION)
def main(filename, verbose, no_cache, cache_filename):
    """Parse, check and realize a beancount ledger.

    This also measures the time it takes to run all these steps.

    """
    use_cache = not no_cache

    if verbose:
        logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    # Override loader caching setup.
    if not use_cache or cache_filename:
        loader.initialize(use_cache, cache_filename)

    with misc_utils.log_time('beancount.loader (total)', logging.info):
        # Load up the file, print errors, checking and validation are invoked
        # automatically.
        entries, errors, _ = loader.load_file(
            filename,
            log_timings=logging.info,
            # Force slow and hardcore validations, just for check.
            extra_validations=validation.HARDCORE_VALIDATIONS)
        printer.print_errors(errors, file=sys.stderr)

    # Exit with an error code if there were any errors.
    sys.exit(1 if errors else 0)


if __name__ == '__main__':
    main()
