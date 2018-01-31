#!/usr/bin/env python3
"""An experiment plotting net worth values over time in all operating currencies.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import logging
import time

from dateutil import rrule
from dateutil.parser import parse
import matplotlib; matplotlib.use("Qt5Agg")
from matplotlib import pyplot
import numpy

from beancount.core import data
from beancount.ops import holdings
from beancount import loader
from beancount.reports import holdings_reports


EXTRAPOLATE_WORTHS = 1000000, 1500000, 2000000, 2500000, 3000000, 4000000, 5000000, 6000000


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('--min-date', action='store',
                        type=lambda string: parse(string).date(),
                        help="Minimum date")

    parser.add_argument('--days-interp', action='store', type=int, default=365,
                        help="Number of days to interpolate the future")

    parser.add_argument('-o', '--output', action='store',
                        help="Save the figure to the given file")

    parser.add_argument('--hide', action='store_true',
                        help="Mask out the vertical axis")

    parser.add_argument('--period', choices=['weekly', 'monthly', 'daily'],
                        default='weekly',
                        help="Period of aggregation")

    parser.add_argument('filename', help='Beancount input filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    if args.min_date:
        dtstart = args.min_date
    else:
        for entry in entries:
            if isinstance(entry, data.Transaction):
                dtstart = entry.date
                break

    net_worths_dict = collections.defaultdict(list)
    index = 0
    current_entries = []

    dtend = datetime.date.today()
    if args.period == 'weekly':
        period = rrule.rrule(rrule.WEEKLY, byweekday=rrule.FR, dtstart=dtstart, until=dtend)
    elif args.period == 'monthly':
        period = rrule.rrule(rrule.MONTHLY, bymonthday=1, dtstart=dtstart, until=dtend)
    elif args.period == 'daily':
        period = rrule.rrule(rrule.DAILY, dtstart=dtstart, until=dtend)

    for dtime in period:
        date = dtime.date()
        logging.info(date)

        # Append new entries until the given date.
        while True:
            entry = entries[index]
            if entry.date >= date:
                break
            current_entries.append(entry)
            index += 1

        # Get the list of holdings.
        raw_holdings_list, price_map = holdings_reports.get_assets_holdings(current_entries,
                                                                            options_map)

        # Convert the currencies.
        for currency in options_map['operating_currency']:
            holdings_list = holdings.convert_to_currency(price_map,
                                                         currency,
                                                         raw_holdings_list)

            holdings_list = holdings.aggregate_holdings_by(
                holdings_list, lambda holding: holding.cost_currency)

            holdings_list = [holding
                             for holding in holdings_list
                             if holding.currency and holding.cost_currency]

            # If after conversion there are no valid holdings, skip the currency
            # altogether.
            if not holdings_list:
                continue

            net_worths_dict[currency].append((date, holdings_list[0].market_value))

    # Extrapolate milestones in various currencies.
    days_interp = args.days_interp
    if args.period == 'weekly':
        num_points = int(days_interp / 7)
    elif args.period == 'monthly':
        num_points = int(days_interp / 30)
    elif args.period == 'daily':
        num_points = 365

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
                date_reach = date.fromtimestamp((amount - poly.c[1]) / poly.c[0])
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

    # Plot each operating currency as a separate curve.
    for currency, currency_data in net_worths_dict.items():
        dates = [date for date, _ in currency_data]
        values = [float(value) for _, value in currency_data]
        pyplot.plot(dates, values, '-', label=currency)
    pyplot.tight_layout()
    pyplot.title("Net Worth")
    pyplot.legend(loc=2)
    if args.hide:
        pyplot.yticks([])

    for dates, amounts in lines:
        pyplot.plot(dates, amounts, 'k--')

    # Output the plot.
    if args.output:
        pyplot.savefig(args.output, figsize=(11,8), dpi=600)
    logging.info("Showing graph")
    pyplot.show()


if __name__ == '__main__':
    main()
