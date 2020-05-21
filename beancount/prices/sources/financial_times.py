"""A source fetching fund prices from Financial Times, from the fund's ISIN.

Valid tickers are in the form "ISIN:CURRENCY", such as "GB00B41XG308:GBP".

For example:
https://markets.ft.com/data/funds/tearsheet/summary?s=GB00B41XG308:GBP

This source parses the website's HTML to retrieve the price.
"""

import datetime
import urllib.request
from html.parser import HTMLParser

from dateutil.tz import tz

from beancount.core.number import D
from beancount.prices import source


class FinancialTimesError(ValueError):
    "An error from the Financial Times API"


def ft_find_price(url, currency):
    # Copied with permission from
    # https://github.com/barrucadu/hledger-scripts/blob/master/market-prices/market-prices.py

    class FTPriceFinder(HTMLParser):
        def __init__(self):
            HTMLParser.__init__(self)
            self.found = None
            self.isnext = False

        def handle_data(self, data):
            if data == 'Price ({})'.format(currency):
                self.isnext = True
            elif self.isnext:
                self.found = data
                self.isnext = False

    req = urllib.request.Request(url)
    with urllib.request.urlopen(req) as response:
        html = response.read().decode('utf-8')
        finder = FTPriceFinder()
        finder.feed(html)
        if finder.found is None:
            raise FinancialTimesError("Could not find price in URL")
        else:
            return finder.found


def fetch_quote(ticker):
    """Fetch"""
    url = "https://markets.ft.com/data/funds/tearsheet/summary?s={}".format(ticker.lower())

    currency = ticker.split(":")[1]
    price = D(ft_find_price(url, currency)).quantize(D('0.01'))
    time = datetime.datetime.now(tz.tzutc())
    return source.SourcePrice(price, time, currency)



class Source(source.Source):
    "Financial Times API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""
        return fetch_quote(ticker)

    def get_historical_price(self, ticker, time):
        """See contract in beancount.prices.source.Source."""
        raise NotImplementedError()
