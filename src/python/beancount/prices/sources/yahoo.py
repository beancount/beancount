"""Fetch prices from Yahoo Finance.

There is also a web service XML/JSON API:
http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
from urllib import request
from urllib import parse
from urllib import error

from beancount.core.number import D
from beancount.prices import source
from beancount.utils import net_utils


class Source(source.Source):
    "Yahoo Finance price source extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        url = 'http://finance.yahoo.com/d/quotes.csv?s={}&f={}'.format(ticker, 'b3b2')
        data = net_utils.retrying_urlopen(url).read().decode('utf-8').strip()
        if not data:
            return None
        bid_str, ask_str = data.split(',')
        if bid_str == 'N/A' or ask_str == 'N/A':
            return None
        bid, ask = D(bid_str), D(ask_str)
        return source.SourcePrice((bid + ask)/2, datetime.datetime.now(), None)

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        # Look back some number of days in the past in order to make sure we hop
        # over national holidays.
        begin_date = date - datetime.timedelta(days=5)
        end_date = date

        # Make the query.
        params = parse.urlencode(sorted({
            's': ticker,
            'a': begin_date.month - 1,
            'b': begin_date.day,
            'c': begin_date.year,
            'd': end_date.month - 1,
            'e': end_date.day,
            'f': end_date.year,
            'g': 'd',
            'ignore': '.csv',
        }.items()))
        url = 'http://ichart.yahoo.com/table.csv?{}'.format(params)
        try:
            data = net_utils.retrying_urlopen(url).read()
        except error.HTTPError:
            return None
        data = data.decode('utf-8').strip()

        lines = data.splitlines()
        assert len(lines) >= 2, "Too few lines in returned data: {}".format(len(lines))

        # Parse the header, find the column for the adjusted close.
        columns = lines[0].split(',')
        index_price = columns.index('Adj Close')
        assert index_price >= 0, "Could not find 'Adj Close' data column."
        index_date = columns.index('Date')
        assert index_date >= 0, "Could not find 'Date' data column."

        # Get the latest data returned.
        most_recent_data = lines[1].split(',')
        close_price = D(most_recent_data[index_price])
        date = datetime.datetime.strptime(most_recent_data[index_date], '%Y-%m-%d')

        return source.SourcePrice(close_price, date, None)
