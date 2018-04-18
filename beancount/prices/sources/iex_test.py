__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
from unittest import mock

from dateutil import tz
import requests

from beancount.prices import source
from beancount.prices.sources import iex
from beancount.core.number import D
from beancount.utils import date_utils


def response(contents, status_code=requests.codes.ok):
    """Produce a context manager to patch a JSON response."""
    response = mock.Mock()
    response.status_code = status_code
    response.text = ""
    response.json.return_value = contents
    return mock.patch('requests.get', return_value=response)


class IEXPriceFetcher(unittest.TestCase):

    def test_error_network(self):
        with response(None, 404):
            with self.assertRaises(ValueError) as exc:
                iex.fetch_quote('AAPL')
                self.assertRegex(exc.message, 'premium')

    def _test_valid_response(self):
        contents = {'avgTotalVolume': 34740512,
                    'calculationPrice': 'close',
                    'change': 1.71,
                    'changePercent': 0.01026,
                    'close': 168.39,
                    'closeTime': 1522785600477,
                    'companyName': 'Apple Inc.',
                    'delayedPrice': 168.4,
                    'delayedPriceTime': 1522789153448,
                    'high': 168.745,
                    'iexAskPrice': 0,
                    'iexAskSize': 0,
                    'iexBidPrice': 0,
                    'iexBidSize': 0,
                    'iexLastUpdated': 1522785599992,
                    'iexMarketPercent': 0.03176,
                    'iexRealtimePrice': 168.39,
                    'iexRealtimeSize': 100,
                    'iexVolume': 958606,
                    'latestPrice': 168.39,
                    'latestSource': 'Close',
                    'latestTime': 'April 3, 2018',
                    'latestUpdate': 1522785600477,
                    'latestVolume': 30182809,
                    'low': 164.88,
                    'marketCap': 854413049070,
                    'open': 167.58,
                    'openTime': 1522762200485,
                    'peRatio': 17.31,
                    'previousClose': 166.68,
                    'primaryExchange': 'Nasdaq Global Select',
                    'sector': 'Technology',
                    'symbol': 'AAPL',
                    'week52High': 183.5,
                    'week52Low': 140.06,
                    'ytdChange': -0.018480361155394188}
        with response(contents):
            srcprice = iex.fetch_quote('FB')
            self.assertIsInstance(srcprice, source.SourcePrice)
            self.assertEqual(D('168.39'), srcprice.price)
            self.assertEqual(datetime.datetime(2018, 4, 3, 20, 0, 0, 477000,
                                               tzinfo=tz.tzutc()),
                             srcprice.time.astimezone(tz.tzutc()))
            self.assertEqual('USD', srcprice.quote_currency)

    def test_valid_response(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._test_valid_response()
