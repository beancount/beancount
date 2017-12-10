"""A source fetching prices from the NASDAQ electronic market.

http://www.nasdaq.com/symbol/vti

This code implements the beancount.prices.source.Source.
"""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re
import datetime
import logging
import socket
from urllib import request
from urllib import parse
from urllib import error

from beancount.core.number import D
from beancount.prices import source
from beancount.utils import net_utils

from dateutil import tz

import bs4


_URL = "http://www.nasdaq.com/symbol/{ticker}"
_HISTORICAL_URL = "http://www.nasdaq.com/symbol/{ticker}/historical"


HistoricalRow = collections.namedtuple(
    "HistoricalRow",
    ["time", "open", "high", "low", "close", "volume"])


class Source(source.Source):
    "NASDAQ price source extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        # Fetch the last trade price.
        req = request.urlopen(_URL.format(ticker=ticker.lower()))
        soup = bs4.BeautifulSoup(req, "lxml")
        last_sale = soup.find("span", class_="last-sale")
        if last_sale is None:
            return None
        price = D(last_sale.text.strip().lstrip("$"))

        # Note: We don't have the time of the last trade on the page.
        time = datetime.datetime.now()

        # Also note that all products on NASDAQ are quoted in US dollars, so
        # hardcoding this here.
        return source.SourcePrice(price, time, "USD")

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        # Fetch the table of recent prices.
        req = request.urlopen(_HISTORICAL_URL.format(ticker=ticker.lower()))
        soup = bs4.BeautifulSoup(req, "lxml")
        # with open("/tmp/out.html", "w") as f:
        #     print(soup.prettify(), file=f)

        div = soup.find(class_="genTable", id="historicalContainer")
        table = div.find("table")
        price_map = {}
        for tr in table.findAll("tr"):
            fields = list(filter(None, [td.text.strip() for td in tr.findAll("td")]))
            if not fields:
                continue
            row = HistoricalRow(*fields)
            row = row._replace(time=datetime.datetime.strptime(row.time, "%m/%d/%Y"))
            price_map[row.time.date()] = row

        row = price_map.get(date, None)
        if row is None:
            return None
        else:
            return source.SourcePrice(D(row.close), row.time, "USD")
