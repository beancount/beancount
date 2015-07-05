"""An experiment plotting net worth values over time in all operating currencies.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import datetime
import logging
import time

from dateutil import rrule
from dateutil.parser import parse
from matplotlib import pyplot
import numpy

from beancount.core import data
from beancount.ops import holdings
from beancount import loader
from beancount.reports import holdings_reports


EXTRAPOLATE_WORTHS = 1000000, 1500000, 2000000, 3000000, 5000000


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('--min-date', action='store',
                        type=lambda string: parse(string).date(),
                        help="Minimum date")

    parser.add_argument('-o', '--output', action='store',
                        help="Save the figure to the given file")

    parser.add_argument('--hide', action='store_true',
                        help="Mask out the vertical axis")

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

    monthly = rrule.rrule(rrule.MONTHLY, bymonthday=1,
                         dtstart=dtstart,
                         until=entries[-1].date)
    weekly = rrule.rrule(rrule.WEEKLY, byweekday=rrule.FR,
                         dtstart=dtstart,
                         until=entries[-1].date)
    num_points = 4

    for dtime in monthly:
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
    for currency, currency_data in net_worths_dict.items():
        recent_data = currency_data[-num_points:]
        dates = [time.mktime(date.timetuple()) for date, _ in recent_data]
        values = [float(value) for _, value in recent_data]
        poly = numpy.poly1d(numpy.polyfit(dates, values, 1))

        logging.info("Extrapolations based on the last %s data points for %s:",
                     num_points, currency)
        for amount in EXTRAPOLATE_WORTHS:
            try:
                date_1m = date.fromtimestamp((amount - poly.c[1]) / poly.c[0])
                logging.info("%10d %s: %s", amount, currency, date_1m)
            except OverflowError:
                pass

        #print(poly(time.mktime(datetime.date.today().timetuple())))

    # Plot each operating currency as a separate curve.
    for currency, currency_data in net_worths_dict.items():
        dates = [date for date, _ in currency_data]
        values = [float(value) for _, value in currency_data]
        pyplot.plot(dates, values, '-', label=currency)
    pyplot.tight_layout()
    pyplot.title("Net Worth")
    pyplot.legend(loc=2)
    pyplot.yticks([])

    # Output the plot.
    if args.output:
        pyplot.savefig(args.output, figsize=(11,8), dpi=600)
    pyplot.show()


if __name__ == '__main__':
    main()
