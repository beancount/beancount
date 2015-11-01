"""Fetch prices from Google Finance.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import re
import datetime
from urllib import request
from urllib import parse

from beancount.core.number import D


__source_name__ = 'google'


# How to query it:
# http://trading.cheno.net/downloading-google-intraday-historical-data-with-python/
#
# http://www.google.com/finance/getprices?q=AAPL&x=NASD&i=120&sessions=ext_hours&p=5d&f=d,c,v,o,h,l&df=cpct&auto=1&ts=1324323553905
# The base url is http://www.google.com/finance/getprices
# q is the symbol (AAPL)
# x is the exchange (NASD)
# i is the interval in seconds (120 = seconds = 2 minutes)
# sessions is the session requested (ext_hours)
# p is the time period (5d = 5 days)
# f is the requested fields (d,c,v,o,h,l)
# df ?? (cpct)
# auto ?? (1)
# ts is potentially a time stamp (1324323553 905)


# class PriceSource:

#     def __init__(self, symbol, base, quote, exchange=None):
#         """Create a price fetcher for Google Finance.

#         Args:
#           symbol: A string, the name of the instrument, in the Google symbology.
#           base: A string, the asset we're actually pricing.
#           quote: A string, the currency units of the price quote.
#           exchange: A string, the name of the exchange to list, in the Google symbology.
#         """
#         self._symbol = symbol
#         self._exchange = exchange

#         # These are intended to be public.
#         self.base = base
#         self.quote = quote

def get_latest_price(ticker):
    """Return the latest price found for the symbol.

    Args:
      ticker: An 'ExchangeCode:Symbol' string that is the unambiguous ticker
        for the particular financial instrument to query.
    Returns:
      A pair of a price (a Decimal object) and the actual date of that price
      (a datetime.datetime instance).
    """
    if ':' in ticker:
        exchange, symbol = ticker.split(':')
    else:
        exchange = None
        symbol = ticker

    # Build the query.
    params_dict = {
        'q': symbol,
        'f': 'd,c', # Date,Close
    }
    if exchange:
        params_dict['x'] = exchange

    # Always reach back 5 days in time because of long weekends.
    if exchange in ('MUTF', 'MUTF_CA'):
        params_dict['p'] = '5d'
    else:
        params_dict['p'] = '5d'
        params_dict['i'] = 300 # secs, to get the most recent.

    url = 'http://www.google.com/finance/getprices?{}'.format(
        parse.urlencode(sorted(params_dict.items())))

    # Fetch the data.
    data = request.urlopen(url).read().decode('utf-8')
    data = parse.unquote(data).strip()

    # Process the meta-data.
    metadata = {}
    lines = data.splitlines()
    for index, line in enumerate(lines):
        mo = re.match('([A-Z_+]+)=(.*)$', line)
        if not mo:
            break
        metadata[mo.group(1)] = mo.group(2)
    else:
        # No data was found.
        return None, None

    interval = int(metadata['INTERVAL'])
    data_lines = lines[index:]
    for line in data_lines:
        if re.match('TIMEZONE_OFFSET', line):
            continue
        time_str, price_str = line.split(',')

        mo = re.match('a(\d+)', time_str)
        if mo:
            time_marker = datetime.datetime.fromtimestamp(int(mo.group(1)))
            time = time_marker
        else:
            seconds = int(time_str) * interval
            time = time_marker + datetime.timedelta(seconds=seconds)

        price = D(price_str)

    return (price, time)


def get_historical_price(ticker, date):
    """Return the historical price found for the symbol at the given date.

    This should work even if queryign for a date that is on a weekend or a
    market holiday.

    Args:
      date: A datetime.date instance.
    Returns:
      A pair of a price (a Decimal object) and the actual date of that price
      (a datetime.date instance).
    """

    # Look back some number of days in the past in order to make sure we hop
    # over national holidays.
    begin_date = date - datetime.timedelta(days=5)
    end_date = date

    # Make the query.
    params = parse.urlencode(sorted({
        'q': ticker,
        'startdate': begin_date.strftime('%b+%d,%Y'),
        'enddate': end_date.strftime('%b+%d,%Y'),
        'num': 5,
        'output': 'csv',
    }.items()))
    url = 'http://www.google.com/finance/historical?{}'.format(params)
    # Note: utf-8-sig automatically skips the BOM here.
    data = request.urlopen(url).read().decode('utf-8-sig').strip()

    lines = data.splitlines()
    assert len(lines) >= 2, "Too few lines in returned data: {}".format(len(lines))

    # Parse the header, find the column for the adjusted close.
    columns = lines[0].split(',')
    index_price = columns.index('Close')
    assert index_price >= 0, "Could not find 'Adj Close' data column."
    index_date = columns.index('Date')
    assert index_date >= 0, "Could not find 'Date' data column."

    # Get the latest data returned.
    most_recent_data = lines[1].split(',')
    close_price = D(most_recent_data[index_price])
    date = datetime.datetime.strptime(most_recent_data[index_date], '%d-%b-%y').date()

    return (close_price, date)
