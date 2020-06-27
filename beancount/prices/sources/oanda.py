"""A source fetching currency prices from OANDA.

Valid tickers are in the form "XXX_YYY", such as "EUR_USD".

Here is the API documentation:
https://developer.oanda.com/rest-live/rates/

For example:
https://api-fxtrade.oanda.com/v1/candles?instrument=EUR_USD&granularity=D&start=2016-03-27T00%3A00%3A00Z&end=2016-04-04T00%3A00%3A00Z&candleFormat=midpoint

Timezone information: Input and output datetimes are specified via UTC
timestamps.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

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
      A sorted list of (time, price) points.
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
    return sorted(time_prices)


def _fetch_price(params_dict, time):
    """Fetch a price from OANDA using the given parameters."""
    ticker = params_dict['instrument']
    _, quote_currency = _get_currencies(ticker)
    if quote_currency is None:
        logging.error("Invalid price source ticker '%s'; must be like 'EUR_USD'",
                      ticker)
        return

    time_prices = _fetch_candles(params_dict)
    if not time_prices:
        logging.error("No prices returned.")
        return

    # Get all the prices before and on the same date and find the latest.
    sorted_prices = [item for item in time_prices if item[0] <= time]
    if not sorted_prices:
        logging.error("No prices matched.")
        return

    time, price = sorted_prices[-1]
    return source.SourcePrice(price, time, quote_currency)


class Source(source.Source):
    "OANDA price source extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""
        time = datetime.datetime.now(tz.tzutc())
        params_dict = {
            'instrument': ticker,
            'granularity': 'S5',  # Every two hours.
            'count': '10',
            'candleFormat': 'midpoint',
        }
        return _fetch_price(params_dict, time)

    def get_historical_price(self, ticker, time):
        """See contract in beancount.prices.source.Source."""
        time = time.astimezone(tz.tzutc())
        query_interval_begin = (time - datetime.timedelta(days=5))
        query_interval_end = (time + datetime.timedelta(days=1))
        params_dict = {
            'instrument': ticker,
            'granularity': 'H2',  # Every two hours.
            'candleFormat': 'midpoint',
            'start': query_interval_begin.isoformat('T'),
            'end': query_interval_end.isoformat('T'),
        }
        return _fetch_price(params_dict, time)
