#!/usr/bin/env python3
"""An experiment plotting net worth values over time in all operating currencies.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import collections
import csv
import datetime
import logging
import time
import pprint
from typing import Any, List, Set, Tuple

from dateutil import rrule
from dateutil.parser import parse
import matplotlib; matplotlib.use("Qt5Agg")
from matplotlib import pyplot
import numpy

from beancount import loader
from beancount.core import account_types
from beancount.core import convert
from beancount.core import data
from beancount.core import inventory
from beancount.core import prices
from beancount.core.data import Currency
from beancount.core.number import ZERO
from beancount.parser import options


EXTRAPOLATE_WORTHS = 1000000, 1500000, 2000000, 2500000, 3000000, 4000000, 5000000, 6000000


def extrapolate(
        net_worths_dict: [data.Currency, Any],
        days_interp: int,
        period: str)-> List[Tuple[datetime.date, data.Amount]]:
    """Extrapolate milestones in various currencies."""

    if period == 'daily':
        num_points = 365
    elif period == 'weekly':
        num_points = int(days_interp / 7)
    elif period == 'monthly':
        num_points = int(days_interp / 30)
    elif period == 'quarterly':
        num_points = int(days_interp / (30 * 3))
    elif period == 'annually':
        num_points = 2

    lines = []
    today = datetime.date.today()
    for currency, currency_data in net_worths_dict.items():
        recent_data = currency_data[-num_points:]
        dates = [time.mktime(date.timetuple()) for date, _ in recent_data]
        values = [float(value) for _, value in recent_data]
        poly = numpy.poly1d(numpy.polyfit(dates, values, 1))

        logging.info("Extrapolations based on the last %s data points for %s:",
                     num_points, currency)
        for amount in EXTRAPOLATE_WORTHS:
            try:
                date_reach = datetime.date.fromtimestamp((amount - poly.c[1]) / poly.c[0])
                if date_reach < today:
                    continue
                time_until = (date_reach - today).days / 365.
                logging.info("%10d %s: %s (%.1f years)",
                             amount, currency, date_reach, time_until)
            except OverflowError:
                pass
        logging.info("Time to save 1M %s: %.1f years",
                     currency, (1000000 / poly.c[0]) / (365*24*60*60))

        last_date = dates[-1]
        one_month_ago = last_date - (30 * 24 * 60 * 60)
        one_week_ago = last_date - (7 * 24 * 60 * 60)
        logging.info("Increase in net-worth per month: %.2f %s  ; per week: %.2f %s",
                     poly(last_date) - poly(one_month_ago), currency,
                     poly(last_date) - poly(one_week_ago), currency)

        dates = [today - datetime.timedelta(days=days_interp), today]
        amounts = [time.mktime(date.timetuple()) * poly.c[0] + poly.c[1] for date in dates]
        lines.append((dates, amounts))

    return lines


def project_missing_currencies(price_map: prices.PriceMap,
                               date: datetime.date,
                               currencies: Set[Currency],
                               target_currency: Currency) -> prices.PriceMap:
    """Project missing currencies to convert `commodities` to `target_currency`.

    We want to convert `currencies` to the currency `target_currency`, but the
    price_map may not contain all the necessary prices. For example, TWD has
    prices in USD, but we want to convert to CAD. TWD being a currency, it
    doesn't have a cost, so routines that convert from cost (e.g.,
    convert.get_value()) will not do their job. What we need to do is convert
    TWD to CAD via USD. The way we do this is by "projecting" the TWD/USD prices
    to TWD/CAD via USD/CAD. This is done iva prices.project().

    We don't want to project any more prices than we need to, howerver. This
    routine is very conservative and automatically finds the subset of
    commodities which will require projection, and automatically infers through
    which currencies it has the opportunity to project.

    Note that this function does not use date/time; if no rates are available to
    at the date required for conversion, the projection will have no effect.
    Therefore, projecting in this way does not guarantee that projection will
    completely success using the returned price map (you should assert). If it
    fails to do so, the solution is to ensure that the ledger contains all
    necessary prices.

    Args:
      price_map: The original price map to probe.
      date: The date at which to convert.
      currencies: The set of currencies you'd like to eventually convert, the
        subject of conversion.
      target_currency: The target currency to which you'd like to convert them.
    Returns:
      An updated price map containing projections to make this possible.

    """
    logging.debug("MISSING %s", currencies)

    # Get a dictionary of which currency is priced in which other.
    priced_currencies = collections.defaultdict(set)
    for base, quote in price_map.keys():
        priced_currencies[base].add(quote)

    # Get a list of the currencies which have prices to the target currency.
    # This works partly because price maps are symmetrical (e.g., "USD" will
    # have all the currencies that are converted to it).
    available_currencies = priced_currencies[target_currency]
    logging.debug("  AVAIL %s", sorted(available_currencies))

    # For all those remaining commodities, ensure that rates exist to
    # convert by value to the target currency, by projecting through
    # an available price conversion.
    projections = collections.defaultdict(list)
    for pos_currency in currencies:
        logging.debug("  POS %s", pos_currency)
        rate_date, rate = prices.get_price(price_map, (pos_currency, target_currency), date)
        if rate is None:
            # Find the available prices for this position.
            quote_currencies = priced_currencies[pos_currency]
            inter_currencies = available_currencies & quote_currencies
            logging.debug("    QUOTE %s %s", quote_currencies, inter_currencies)
            for inter_currency in inter_currencies:
                projections[inter_currency].append(pos_currency)

    logging.debug("  PROJ %s", projections)

    # Apply the projections.
    proj_price_map = price_map
    for inter_currency, commodities in projections.items():
        proj_price_map = prices.project(proj_price_map, inter_currency, target_currency,
                                        commodities)

    return proj_price_map


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('--min-date', action='store',
                        type=lambda string: parse(string).date(),
                        help="Minimum date")

    parser.add_argument('--days-interp', action='store', type=int, default=365,
                        help="Number of days to interpolate the future")

    parser.add_argument('-o', '--output', action='store',
                        help="Save the figure to the given file")

    parser.add_argument('--output-csv', action='store',
                        help="Save the CSV time series to the given file")

    parser.add_argument('--hide', action='store_true',
                        help="Mask out the vertical axis")

    parser.add_argument('--period', choices=['daily', 'weekly', 'monthly',
                                             'quarterly', 'annually'],
                        default='weekly',
                        help="Period of aggregation")

    parser.add_argument('filename', help='Beancount input filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)
    acctypes = options.get_account_types(options_map)
    price_map = prices.build_price_map(entries)
    operating_currencies = options_map['operating_currency']

    if args.min_date:
        dtstart = args.min_date
    else:
        for entry in entries:
            if isinstance(entry, data.Transaction):
                dtstart = entry.date
                break

    net_worths_dict = collections.defaultdict(list)
    index = 0

    dtend = datetime.date.today()
    kw = dict(dtstart=dtstart, until=dtend)
    if args.period == 'daily':
        period = rrule.rrule(rrule.DAILY, **kw)
    elif args.period == 'weekly':
        period = rrule.rrule(rrule.WEEKLY, byweekday=rrule.FR, **kw)
    elif args.period == 'monthly':
        period = rrule.rrule(rrule.MONTHLY, bymonthday=1, **kw)
    elif args.period == 'quarterly':
        period = rrule.rrule(rrule.MONTHLY, interval=3, bymonthday=1, **kw)
    elif args.period == 'annually':
        period = rrule.rrule(rrule.MONTHLY, interval=12, bymonthday=1, **kw)

    balance = inventory.Inventory()
    for dtime in period:
        date = dtime.date()
        logging.info("===== {}".format(date))

        # Append new entries until the given date.
        new_entries = []
        while index < len(entries):
            entry = entries[index]
            if entry.date >= date:
                break
            new_entries.append(entry)
            index += 1

        # Simple global aggregation of all intervening postings to a single
        # inventory.
        for entry in data.filter_txns(new_entries):
            for posting in entry.postings:
                acctype = account_types.get_account_type(posting.account)
                if acctype in (acctypes.assets, acctypes.liabilities):
                    balance.add_position(posting)

        for i, currency in enumerate(operating_currencies):
            logging.debug("------------------------- %s", currency)

            # Compute balance at market price values. This will convert all
            # commodities held at cost to their cost value, and others to the
            # priced value, if relevant prices exist. Only commodities which
            # aren't held at cost or which have no price conversion information
            # providing a conversion currency will remain.
            value_balance = balance.reduce(convert.get_value, price_map, date)
            logging.debug("BAL %s", value_balance.to_string())

            # Convert all contents to destination currency.
            proj_price_map = project_missing_currencies(
                price_map, date, {pos.units.currency for pos in value_balance}, currency)
            converted_balance = balance.reduce(convert.convert_position,
                                               currency, proj_price_map, date)

            # Collect result.
            per_currency_dict = converted_balance.split()
            pos = per_currency_dict.pop(currency).get_only_position()
            assert pos.cost is None

            # If some conversions failed, log an error.
            if per_currency_dict:
                logging.error("Could not convert all positions at date %s, to %s: %s",
                              date, currency, per_currency_dict)
            net_worths_dict[currency].append((date, pos.units.number))

    # Extrapolate milestones in various currencies.
    lines = extrapolate(net_worths_dict, args.days_interp, args.period)

    # Plot each operating currency as a separate curve.
    for currency, currency_data in net_worths_dict.items():
        dates = [date for date, _ in currency_data]
        values = [float(value) for _, value in currency_data]
        pyplot.plot(dates, values, '.-', label=currency)

    pyplot.tight_layout()
    pyplot.title("Net Worth")
    pyplot.legend(loc=2)
    if args.hide:
        pyplot.yticks([])

    for dates, amounts in lines:
        pyplot.plot(dates, amounts, 'k--')

    # Output the CSV file.
    if args.output_csv:
        main_currency = operating_currencies[0]
        time_series = net_worths_dict[main_currency]
        with open(args.output_csv, "w") as outfile:
            wr = csv.writer(outfile)
            wr.writerows(time_series)

    # Output the plot.
    if args.output:
        pyplot.savefig(args.output, figsize=(11,8), dpi=600)
    logging.info("Showing graph")
    pyplot.show()


if __name__ == '__main__':
    main()
