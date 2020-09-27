#!/usr/bin/env python3
"""Calculate my true returns, including dividends and real costs.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import argparse
import datetime
import logging
import os

from beancount import loader
from beancount.core import getters
from beancount.core import prices

import investments
import reports
import config as configlib


def main():
    """Top-level function."""
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('ledger',
                        help="Beancount ledger file")
    parser.add_argument('config', action='store',
                        help='Configuration for accounts and reports.')
    parser.add_argument('output',
                        help="Output directory to write all output files to.")

    parser.add_argument('filter_reports', nargs='*',
                        help="Optional names of specific subset of reports to analyze.")

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Verbose mode')

    parser.add_argument('-d', '--days-price-threshold', action='store', type=int,
                        default=5,
                        help="The number of days to tolerate price latency.")

    parser.add_argument('-e', '--end-date', action='store',
                        type=datetime.date.fromisoformat,
                        help="The end date to compute returns up to.")

    parser.add_argument('--pdf', '--pdfs', action='store_true',
                        help="Render as PDFs. Default is HTML directories.")

    parser.add_argument('-j', '--parallel', action='store_true',
                        help="Run report generation concurrently.")

    parser.add_argument('-E', '--check-explicit-flows', action='store_true',
                        help=("Enables comparison of the general categorization method "
                              "with the explicit one with specialized explicit  handlers "
                              "per signature."))

    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
        logging.getLogger('matplotlib.font_manager').disabled = True

    # Figure out end date.
    end_date = args.end_date or datetime.date.today()

    # Load the example file.
    logging.info("Reading ledger: %s", args.ledger)
    entries, _, options_map = loader.load_file(args.ledger)
    accounts = getters.get_accounts(entries)
    dcontext = options_map['dcontext']

    # Load, filter and expand the configuration.
    config = configlib.read_config(args.config, args.filter_reports, accounts)
    os.makedirs(args.output, exist_ok=True)
    with open(path.join(args.output, "config.pbtxt"), "w") as efile:
        print(config, file=efile)

    # Extract data from the ledger.
    account_data_map = investments.extract(
        entries, dcontext, config, end_date, args.check_explicit_flows,
        path.join(args.output, "investments"))

    # Generate output reports.
    output_reports = path.join(args.output, "groups")
    pricer = reports.generate_reports(account_data_map, config,
                                      prices.build_price_map(entries),
                                      end_date,
                                      output_reports,
                                      args.parallel, args.pdf)

    # Generate price reports.
    output_prices = path.join(args.output, "prices")
    reports.generate_price_pages(account_data_map,
                                 prices.build_price_map(entries),
                                 output_prices)

    # Output required price directives (to be filled in the source ledger by
    # fetching prices).
    reports.write_price_directives(path.join(output_prices, "prices.beancount"),
                                   pricer, args.days_price_threshold)


if __name__ == '__main__':
    main()
