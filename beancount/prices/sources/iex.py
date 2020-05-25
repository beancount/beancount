"""Fetch prices from the IEX 1.0 public API.

This is a really fantastic exchange API with a lot of relevant information.

Timezone information: There is currency no support for historical prices. The
output datetime is provided as a UNIX timestamp.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import datetime

from dateutil import tz
import requests

from beancount.core.number import D
from beancount.prices import source


class IEXError(ValueError):
    "An error from the IEX API."


def fetch_quote(ticker):
    """Fetch the latest price for the given ticker."""

    url = "https://api.iextrading.com/1.0/tops/last?symbols={}".format(ticker.upper())
    response = requests.get(url)
    if response.status_code != requests.codes.ok:
        raise IEXError("Invalid response ({}): {}".format(
            response.status_code, response.text))

    results = response.json()
    if len(results) != 1:
        raise IEXError("Invalid number of responses from IEX: {}".format(
            response.text))
    result = results[0]

    price = D(result['price']).quantize(D('0.01'))

    # IEX is American markets.
    us_timezone = tz.gettz("America/New_York")
    time = datetime.datetime.fromtimestamp(result['time'] / 1000)
    time = time.astimezone(us_timezone)

    # As far as can tell, all the instruments on IEX are priced in USD.
    return source.SourcePrice(price, time, 'USD')


class Source(source.Source):
    "IEX API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""
        return fetch_quote(ticker)

    def get_historical_price(self, ticker, time):
        """See contract in beancount.prices.source.Source."""
        raise NotImplementedError(
            "This is now implemented at https://iextrading.com/developers/docs/#hist and "
            "needs to be added here.")
