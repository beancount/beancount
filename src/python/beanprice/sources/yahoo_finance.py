"""Fetch prices from Yahoo Finance.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
from urllib import request
from urllib import parse

from beancount.core.number import D


__source_name__ = 'yahoo'


# There is also a web service XML/JSON API:
# http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote


class Source:

    def get_latest_price(self, ticker):
        """Return the latest price found for the symbol.

        Returns:
          A Decimal object.
        """
        url = 'http://finance.yahoo.com/d/quotes.csv?s={}&f={}'.format(ticker, 'b3b2')
        data = request.urlopen(url).read().decode('utf-8')
        bid_str, ask_str = data.split(',')
        if bid_str == 'N/A' or ask_str == 'N/A':
            return None, None
        bid, ask = D(bid_str), D(ask_str)
        return ((bid + ask)/2, datetime.datetime.now())

    def get_historical_price(self, ticker, date):
        """Return the historical price found for the symbol at the given date.

        This should work even if querying for a date that is on a weekend or a
        market holiday.

        Args:
          date: A datetime.date instance.
        Returns:
          A pair of a price (a Decimal object) and the actual date of that price
          (a datetime.date instance).
        """
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
        data = request.urlopen(url).read().decode('utf-8').strip()

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
        date = datetime.datetime.strptime(most_recent_data[index_date], '%Y-%m-%d').date()

        return (close_price, date)
