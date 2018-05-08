"""Fetch prices from Yahoo Finance's CSV API.

As of lated 2017, the older Yahoo finance API deprecated. In particular, the
ichart endpoint is gone, and the download endpoint requires a cookie (which
could be gotten - here's some documentation for that
http://blog.bradlucas.com/posts/2017-06-02-new-yahoo-finance-quote-download-url/).

We're using both the v7 and v8 APIs here, both of which are, as far as I can
tell, undocumented:

https://query1.finance.yahoo.com/v7/finance/quote
https://query1.finance.yahoo.com/v8/finance/chart/SYMBOL

Timezone information: Input and output datetimes are specified via UNIX
timestamps, but the timezone of the particular market is included in the output.
"""
__copyright__ = "Copyright (C) 2015-2018  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
from typing import Dict, Any

import requests

from beancount.core.number import D
from beancount.prices import source


class YahooError(ValueError):
    "An error from the Yahoo API."


def parse_response(response: requests.models.Response) -> Dict:
    """Process as response from Yahoo.

    Raises:
      YahooError: If there is an error in the response.
    """
    json = response.json()
    content = next(iter(json.values()))
    if response.status_code != requests.codes.ok:
        raise YahooError("Status {}: {}".format(response.status_code, content['error']))
    if len(json) != 1:
        raise YahooError("Invalid format in response from Yahoo; many keys: {}".format(
            ','.join(json.keys())))
    if content['error'] is not None:
        raise YahooError("Error fetching Yahoo data: {}".format(content['error']))
    return content['result'][0]


# Note: Feel free to suggest more here via a PR.
_MARKETS = {
    'us_market': 'USD',
    'ca_market': 'CAD',
}


def parse_currency(result: Dict[str, Any]) -> str:
    """Infer the currency from the result."""
    if 'market' not in result:
        return None
    return _MARKETS.get(result['market'], None)


_DEFAULT_PARAMS = {
    'lang': 'en-US',
    'corsDomain': 'finance.yahoo.com',
    '.tsrc': 'finance',
}


class Source(source.Source):
    "Yahoo Finance CSV API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        url = "https://query1.finance.yahoo.com/v7/finance/quote"
        fields = ['symbol', 'regularMarketPrice', 'regularMarketTime']
        payload = {
            'symbols': ticker,
            'fields': ','.join(fields),
            'exchange': 'NYSE',
        }
        payload.update(_DEFAULT_PARAMS)
        response = requests.get(url, params=payload)
        result = parse_response(response)
        try:
            price = D(result['regularMarketPrice'])

            timezone = datetime.timezone(
                datetime.timedelta(hours=result['gmtOffSetMilliseconds'] / 3600000),
                result['exchangeTimezoneName'])
            trade_time = datetime.datetime.fromtimestamp(result['regularMarketTime'],
                                                         tz=timezone)
        except KeyError:
            raise YahooError("Invalid response from Yahoo: {}".format(repr(result)))

        currency = parse_currency(result)

        return source.SourcePrice(price, trade_time, currency)

    def get_historical_price(self, ticker, time):
        """See contract in beancount.prices.source.Source."""
        if requests is None:
            raise YahooError("You must install the 'requests' library.")
        url = "https://query1.finance.yahoo.com/v8/finance/chart/{}".format(ticker)
        dt_start = time - datetime.timedelta(days=5)
        dt_end = time
        payload = {
            'period1': int(dt_start.timestamp()),
            'period2': int(dt_end.timestamp()),
            'interval': '1d',
        }
        payload.update(_DEFAULT_PARAMS)
        response = requests.get(url, params=payload)
        result = parse_response(response)

        meta = result['meta']
        timezone = datetime.timezone(datetime.timedelta(hours=meta['gmtoffset'] / 3600),
                                     meta['exchangeTimezoneName'])

        timestamp_array = result['timestamp']
        close_array = result['indicators']['quote'][0]['close']
        series = [(datetime.datetime.fromtimestamp(timestamp, tz=timezone), D(price))
                  for timestamp, price in zip(timestamp_array, close_array)]

        # Get the latest data returned.
        latest = None
        for data_dt, price in sorted(series):
            if data_dt >= time:
                break
            latest = data_dt, price
        if latest is None:
            raise YahooError("Could not find price before {} in {}".format(time, series))

        currency = result['meta']['currency']
        return source.SourcePrice(price, data_dt, currency)
