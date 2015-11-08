"""Fetch prices from Yahoo Finance's CSV API.

Fields:
https://code.google.com/p/yahoo-finance-managed/wiki/enumQuoteProperty

Yahoo also has a web service with a query language (YQL) that outputs XML or
JSON but it requires a key, so that's why we're using the CSV API here, for
simplicity's sake. In any case, the YQL API docs is located here:
http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import logging
import re
from urllib import request
from urllib import parse
from urllib import error

from beancount.core.number import D
from beancount.prices import source
from beancount.utils import net_utils


class Source(source.Source):
    "Yahoo Finance CSV API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        # Try "realtime" and just regular bid/ask pairs.
        for fields, num_prices in [('l1d1', 1),
                                   ('b3b2d2', 2),
                                   ('b0a0d2', 2),
                                   ('p0d2', 2)]:
            url = 'http://finance.yahoo.com/d/quotes.csv?s={}&f=c4{}'.format(ticker, fields)
            data = net_utils.retrying_urlopen(url).read().decode('utf-8').strip()
            if data and not re.match('N/A', data):
                break
        else:
            return None
        components = data.split(',')

        # Get the currency.
        currency = components[0].strip('"')

        # Get the
        if num_prices == 1:
            # Process just a price.
            price = D(components[1])
        else:
            # Process separate bid/offer.
            bid = D(components[1])
            ask = D(components[2])
            price = (bid + ask)/2

        # Get the trade date for that price.
        trade_date = datetime.datetime.strptime(components[-1], '"%m/%d/%Y"')

        return source.SourcePrice(price, trade_date, currency)

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
