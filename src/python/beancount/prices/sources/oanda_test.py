"""Test for price extractor of OANDA.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import datetime
import unittest
import time
from unittest import mock
from urllib import request
from urllib import error

import dateutil.tz

from beancount.prices.sources import oanda
from beancount.prices import source
from beancount.core.number import D
from beancount.core.number import Decimal


class TestOANDASource(unittest.TestCase):

    maxDiff = 8192

    def setUp(self):
        self.fetcher = oanda.Source()
        self.response = mock.MagicMock()
        self.response.read = mock.MagicMock()
        self.response.getcode = mock.MagicMock(return_value=200)
        mock.patch.object(request, 'urlopen', return_value=self.response).start()

    def tearDown(self):
        mock.patch.stopall()

    def test_fetch_candles__invalid_ticker(self):
        srcprice = self.fetcher.get_latest_price('NOTATICKER')
        self.assertIsNone(srcprice)

    def test_fetch_candles__invalid_response(self):
        self.response.read.return_value = ""
        self.response.getcode = mock.MagicMock(return_value=401)
        srcprice = self.fetcher.get_latest_price('USD_FFF')
        self.assertIsNone(srcprice)

    def test_fetch_candles__error_response(self):
        self.response.read.return_value = textwrap.dedent("""\
            {
                    "code" : 46,
                    "message" : "Invalid instrument: CAD_FFF",
                    "moreInfo" : "http:\/\/developer.oanda.com\/docs\/v1\/troubleshooting\/#errors"
            }
        """).encode('utf-8')
        srcprice = self.fetcher.get_latest_price('USD_FFF')
        self.assertIsNone(srcprice)

    def test_get_latest_price(self):
        self.response.read.return_value = textwrap.dedent("""\
            {
                    "instrument" : "USD_CAD",
                    "granularity" : "S5",
                    "candles" : [
                            {
                                    "time" : "2016-04-04T21:19:17.000000Z",
                                    "openMid" : 1.308475,
                                    "highMid" : 1.308475,
                                    "lowMid" : 1.308475,
                                    "closeMid" : 1.308475,
                                    "volume" : 1,
                                    "complete" : true
                            }
                    ]
            }
        """).encode('utf-8')
        srcprice = self.fetcher.get_latest_price('USD_CAD')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        expected = source.SourcePrice(D('1.308475'),
                                      datetime.datetime(2016, 4, 4, 21, 19, 17,
                                                        tzinfo=dateutil.tz.tzutc()),
                                      'CAD')
        self.assertEqual(expected, srcprice)

    # def test_get_historical_price(self):
    # FIXME: TODO
