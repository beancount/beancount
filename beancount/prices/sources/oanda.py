"""A source fetching currency prices from OANDA.

Valid tickers are in the form "XXX_YYY", such as "EUR_USD".

Here is the API documentation:
http://developer.oanda.com/rest-live/rates/

For example:
https://api-fxtrade.oanda.com/v1/candles?instrument=EUR_USD&granularity=D&start=2016-03-27T00%3A00%3A00Z&end=2016-04-04T00%3A00%3A00Z&candleFormat=midpoint
"""
__author__ = "Martin Blais <blais@furius.ca>"

import re
import datetime
import json
import logging
from urllib import parse

from dateutil import tz

from beancount.core.number import D
from beancount.prices import source
from beancount.utils import net_utils


URL = "https://api-fxtrade.oanda.com/v1/candles"


def _get_currencies(ticker):
    """Parse the base and quote currencies from the ticker.

    Args:
      ticker: A string, the symbol in XXX_YYY format.
    Returns:
      A pair of (base, quote) currencies.
    """
    match = re.match("([A-Z]+)_([A-Z]+)$", ticker)
    if not match:
        return None, None
    return match.groups()


def _fetch_candles(params):
    """Fetch the given URL from OANDA and return a list of (utc-time, price).

    Args:
      params: A dict of URL params values.
    Returns:
      A list of (time, price) points.
    """

    url = '?'.join((URL, parse.urlencode(sorted(params.items()))))
    logging.info("Fetching '%s'", url)

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
        time_prices = []
        candles = sorted(data['candles'], key=lambda candle: candle['time'])
        for candle in candles:
            candle_dt_utc = datetime.datetime.strptime(
                candle['time'], r"%Y-%m-%dT%H:%M:%S.%fZ").replace(tzinfo=tz.tzutc())
            candle_price = D(candle['openMid'])
            time_prices.append((candle_dt_utc, candle_price))
    except KeyError:
        logging.error("Unexpected response data: %s", data)
        return None
    return time_prices


class Source(source.Source):
    "OANDA price source extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        _, quote_currency = _get_currencies(ticker)
        if quote_currency is None:
            logging.error("Invalid price source ticker '%s'; must be like 'EUR_USD'",
                          ticker)
            return

        # Query at the current (latest) time.
        params_dict = {
            'instrument': ticker,
            'granularity': 'S5',  # Every two hours.
            'count': '10',
            'candleFormat': 'midpoint',
        }
        time_prices = _fetch_candles(params_dict)
        if time_prices is None:
            logging.error("No prices returned")
            return

        # Get the latest price point available.
        time, price = sorted(time_prices)[-1]

        # Use current local date as the date.
        current_date = datetime.datetime.now()

        return source.SourcePrice(price, current_date, quote_currency)

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        _, quote_currency = _get_currencies(ticker)
        if quote_currency is None:
            logging.error("Invalid price source ticker '%s'; must be like 'EUR_USD'",
                          ticker)
            return

        # Find the boundary dates to query in UTC timezone.
        start_utc = datetime.datetime(date.year, date.month, date.day, 0, 0, 0,
                                      tzinfo=tz.tzlocal()).astimezone(tz.tzutc())
        end_utc = start_utc + datetime.timedelta(days=1)

        interval_begin_utc = (start_utc - datetime.timedelta(days=5))
        interval_end_utc = (end_utc + datetime.timedelta(days=5))

        # Build the query.
        params_dict = {
            'instrument': ticker,
            'granularity': 'H2',  # Every two hours.
            'candleFormat': 'midpoint',
            'start': interval_begin_utc.isoformat('T'),
            'end': interval_end_utc.isoformat('T'),
        }
        time_prices = _fetch_candles(params_dict)
        if time_prices is None:
            logging.error("No prices returned")
            return

        # Get all the prices with the same date.
        same_date = [item
                     for item in time_prices
                     if start_utc <= item[0] < end_utc]
        if same_date:
            # Find the min/max and return the median of all prices.
            sorted_prices = sorted(same_date, key=lambda item: item[1])
            time, price = sorted_prices[len(sorted_prices)//2]
        else:
            # No price matching the date were found; use the midpoint of the
            # last price before the day interval and the first price after the
            # day interval.
            before_time, before_price = min(item
                                            for item in time_prices
                                            if item[0] < start_utc)
            after_time, after_price = max(item
                                          for item in time_prices
                                          if item[0] >= end_utc)
            price = (after_price + before_price) / 2
            time = before_time + (after_time - before_time)/2

        return source.SourcePrice(price, time, quote_currency)
