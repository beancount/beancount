"""A source fetching cryptocurrency prices from Coinbase.

Valid tickers are in the form "XXX-YYY", such as "BTC-USD".

Here is the API documentation:
https://developers.coinbase.com/api/v2

For example:
https://api.coinbase.com/v2/prices/BTC-GBP/spot

Timezone information: Input and output datetimes are specified via UTC
timestamps.
"""

import datetime

import requests
from dateutil.tz import tz

from beancount.core.number import D
from beancount.prices import source


class CoinbaseError(ValueError):
    "An error from the Coinbase API."


def fetch_quote(ticker, time=None):
    """Fetch a quote from Coinbase."""
    url = "https://api.coinbase.com/v2/prices/{}/spot".format(ticker.lower())
    options = {}
    if time is not None:
        options['date'] = time.astimezone(tz.tzutc()).date().isoformat()

    response = requests.get(url, options)
    if response.status_code != requests.codes.ok:
        raise CoinbaseError("Invalid response ({}): {}".format(response.status_code,
                                                               response.text))
    result = response.json()

    price = D(result['data']['amount'])
    if time is None:
        time = datetime.datetime.now(tz.tzutc())
    currency = result['data']['currency']

    return source.SourcePrice(price, time, currency)


class Source(source.Source):
    "Coinbase API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""
        return fetch_quote(ticker)

    def get_historical_price(self, ticker, time):
        """See contract in beancount.prices.source.Source."""
        return fetch_quote(ticker, time)
