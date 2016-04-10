"""A source fetching currency prices from OANDA.

Here is the API documentation:
http://developer.oanda.com/rest-live/rates/

For example:
https://api-fxtrade.oanda.com/v1/candles?instrument=EUR_USD&granularity=D&start=2016-03-27T00%3A00%3A00Z&end=2016-04-04T00%3A00%3A00Z&candleFormat=midpoint
"""
__author__ = "Martin Blais <blais@furius.ca>"

import re
import sys
import datetime
import json
import logging
from urllib import parse
from urllib import error

from dateutil.parser import parse as parse_date
from dateutil import tz

from beancount.core.number import D
from beancount.core.number import Decimal
from beancount.prices import source
from beancount.utils import net_utils


class Source(source.Source):
    "OANDA price source extractor."


    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        params_dict = {
            'instrument': ticker,
            'count': '1',
            'candleFormat': 'midpoint',
        }
        return fetch_candles(params_dict, None)

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        # Build the query.
        time = datetime.datetime(date.year, date.month, date.day, 0, 0, 0,
                                 tzinfo=tz.tzutc())
        start = time - datetime.timedelta(days=5)
        end = time + datetime.timedelta(days=10)
        params_dict = {
            'instrument': ticker,
            'granularity': 'D',
            'candleFormat': 'midpoint',
            'start': start.isoformat('T'),
            'end': end.isoformat('T'),
        }
        return fetch_candles(params_dict, date)


def fetch_candles(params, date=None):
    """Fetch the given URL from OANDA and parse the first price returned.

    Args:
      params: A dict of URL params values.
      date: Desired date. If None, use the latest.
    Returns:
      A SourcePrice instance, or None on failure.
    """
    if date:
        search_time = datetime.datetime(date.year, date.month, date.day, 0, 0, 0,
                                        tzinfo=tz.tzutc())
    else:
        search_time = datetime.datetime.now(tz.tzlocal()).astimezone(tz.tzutc())

        # FIXME: Read the docs and deal with the timezones correctly,
        # translating found times back to the local datetime.

        # FIXME: Override this for the test.
        print(search_time)

    URL = "https://api-fxtrade.oanda.com/v1/candles"
    url = '?'.join((URL, parse.urlencode(sorted(params.items()))))
    logging.info("Fetching '%s'", url)

    # Parse the base and quote currencies from the ticker, which is always
    # representative for this price source.
    match = re.match("([A-Z]+)_([A-Z]+)$", params['instrument'])
    if not match:
        logging.error("Invalid price source ticker '%s'; must be like 'EUR_USD'",
                      params['instrument'])
        return None
    base_currency, quote_currency = match.groups()

    # Fetch the data.
    response = net_utils.retrying_urlopen(url)
    if response is None:
        return None
    data_string = response.read().decode('utf-8')

    # Parse it.
    data = json.loads(data_string, parse_float=D)
    try:
        # Find the candle with the latest time before the given time we're searching
        # for.
        candles = sorted(data['candles'], key=lambda candle: candle['time'])
        latest_candle = None
        for candle in candles:
            print('Z', candle['time'])
            candle_time = parse_date(candle['time'])
            print('>', candle_time, search_time)
            if candle_time > search_time:
                break
            latest_candle = candle

        # Get the price out of the chosen candle.
        if latest_candle is None:
            logging.error("Could not find a valid candle for %s: %s", date, data)
            return None
        candle_price = D(latest_candle[0]['openMid'])

    except KeyError:
        logging.error("Unexpected response data: %s", data)
        return None

    return source.SourcePrice(price, candle_time, quote_currency)
