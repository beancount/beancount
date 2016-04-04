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
from urllib import parse
from urllib import error

from dateutil.parser import parse as parse_date

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
        return fetch_candles(params_dict)

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        # Build the query.
        start = datetime.datetime(date.year, date.month, date.day)
        end = start + datetime.timedelta(days=1)
        params_dict = {
            'instrument': ticker,
            'granularity': 'D',
            'candleFormat': 'midpoint',
            'start': start.isoformat('T') + 'Z',
            'end': end.isoformat('T') + 'Z',
        }
        return fetch_candles(params_dict)


def fetch_candles(params):
    """Fetch the given URL from OANDA and parse the first price returned.

    Args:
      params: A dict of URL params values.
    Returns:
      A SourcePrice instance, or None on failure.
    """
    URL = "https://api-fxtrade.oanda.com/v1/candles"
    url = '?'.join((URL, parse.urlencode(sorted(params.items()))))

    # Parse the base and quote currencies from the ticker, which is always
    # representative for this price source.
    match = re.match("([A-Z]+)_([A-Z]+)", params['instrument'])
    if not match:
        logging.error("Invalid price source ticker '%s'; must be like 'EUR_USD'",
                      params['instrument'])
    base_currency, quote_currency = match.groups()

    # Fetch the data.
    response = net_utils.retrying_urlopen(url)
    if response is None:
        return None
    data_string = response.read().decode('utf-8')

    # Parse it.
    data = json.loads(data_string, parse_float=D)
    try:
        num_string = data['candles'][0]['openMid']
        time_string = data['candles'][0]['time']
    except KeyError:
        logging.error("Unexpected response data: %s", data)
        return None
    price = D(num_string)
    time = parse_date(time_string)

    return source.SourcePrice(price, time, quote_currency)
