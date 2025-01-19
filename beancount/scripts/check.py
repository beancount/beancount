__copyright__ = "Copyright (C) 2013-2014, 2016-2018, 2020-2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import sys

import click

from beancount import loader
from beancount.ops import validation
from beancount.parser.version import VERSION
from beancount.utils import misc_utils


@click.command()
@click.argument("filename", type=click.Path())
@click.option("--verbose", "-v", is_flag=True, help="Print timings.")
@click.option("--no-cache", "-C", is_flag=True, help="Disable the cache.")
@click.option("--cache-filename", type=click.Path(), help="Override the cache filename.")
@click.option("--auto", "-a", is_flag=True, help="Implicitly enable auto-plugins.")
@click.version_option(message=VERSION)
def main(filename: str, verbose: bool, no_cache: bool, cache_filename: str, auto: bool):
    """Parse, check and realize a beancount ledger.

    This also measures the time it takes to run all these steps.

    """
    use_cache = not no_cache

    old_plugins_auto = loader.PLUGINS_AUTO[:]
    try:
        if auto:
            # Insert auto plugins. This is convenient for importers
            # because when generating a subset of transactions
            # oftentimes we don't have the contextual account and
            # commodity creation routines. See {4ec6a3205b6c}.
            loader.PLUGINS_AUTO.extend(loader.DEFAULT_PLUGINS_AUTO)

        if verbose:
            logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")

        # Override loader caching setup.
        if not use_cache or cache_filename:
            loader.initialize(use_cache, cache_filename)

        with misc_utils.log_time("beancount.loader (total)", logging.info):
            # Load up the file, print errors, checking and validation
            # are invoked automatically.
            entries, errors, _ = loader.load_file(
                filename,
                log_timings=logging.info,
                log_errors=sys.stderr,
                # Force slow and hardcore validations, just for check.
                extra_validations=validation.HARDCORE_VALIDATIONS,
            )
    finally:
        if auto:
            # Remove auto plugins. This is not necessary when this
            # code is run as script but it is needed when run as part
            # of the test suite (which does not span a new Python
            # interpreter for each script invocation test).
            loader.PLUGINS_AUTO[:] = old_plugins_auto

    # Exit with an error code if there were any errors.
    sys.exit(1 if errors else 0)


if __name__ == "__main__":
    main()
