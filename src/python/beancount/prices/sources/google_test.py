"""Test for price extractor of Google Finance.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import datetime
import unittest
import time
from unittest import mock
from urllib import request
from urllib import error

from beancount.prices.sources import google
from beancount.core.number import D, Decimal

from dateutil import tz


class _TestTimezone(datetime.tzinfo):
    def utcoffset(self, dt):
        return datetime.timedelta(hours=-4) + self.dst(dt)
    def dst(self, dt):
        return datetime.timedelta(0)
    def tzname(self, dt):
        return "Test"


class TestGoogleFinanceSource(unittest.TestCase):

    def setUp(self):
        self.fetcher = google.Source()
        self.url_object = mock.MagicMock()
        self.url_object.read = mock.MagicMock()
        self.url_object.getcode = mock.MagicMock(return_value=200)
        self.saved_urlopen = request.urlopen
        request.urlopen = mock.MagicMock(return_value=self.url_object)

    def tearDown(self):
        request.urlopen = self.saved_urlopen

    def test_get_latest_price(self):
        self.url_object.read.return_value = textwrap.dedent("""\
            EXCHANGE%3DNASDAQ
            MARKET_OPEN_MINUTE=570
            MARKET_CLOSE_MINUTE=960
            INTERVAL=60
            COLUMNS=DATE,CLOSE
            DATA=
            TIMEZONE_OFFSET=-240
            a1402061400,558.06
            1,555.67
            2,555.512
            389,555.89
            390,556.33
        """).encode('utf-8')
        srcprice = self.fetcher.get_latest_price('NASD:HOOL')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('556.33'), srcprice.price)

        NYC = tz.gettz("America/New York")
        self.assertEqual(srcprice.time,
                         datetime.datetime(2014, 6, 6, 16, 0, 0, tzinfo=NYC))

    def test_get_latest_price__invalid(self):
        self.url_object.read.return_value = textwrap.dedent("""\
            EXCHANGE%3DCURRENCY
            MARKET_OPEN_MINUTE=0
            MARKET_CLOSE_MINUTE=1438
            INTERVAL=300
            COLUMNS=DATE,CLOSE
            DATA=
        """).encode('utf-8')
        srcprice = self.fetcher.get_latest_price('CURRENCY:INVALID')
        self.assertIsNone(srcprice)

    def test_get_historical_price(self):
        # That first character before date is the BOM.
        self.url_object.read.return_value = textwrap.dedent("""\
            ﻿Date,Open,High,Low,Close,Volume
            6-May-14,525.23,526.81,515.06,515.14,1684381
            5-May-14,524.82,528.90,521.32,527.81,1021408
            2-May-14,533.76,534.00,525.61,527.93,1685042
        """).encode('utf-8')
        request_date = datetime.date(2014, 5, 7)
        expected_datetime = datetime.datetime(2014, 5, 6)
        srcprice = self.fetcher.get_historical_price('NASD:HOOL', request_date)
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('515.14'), srcprice.price)
        self.assertEqual(expected_datetime, srcprice.time)

    def test_get_historical_price__invalid(self):
        self.url_object.read.side_effect = error.HTTPError('url', 'code', '404', {}, None)
        srcprice = self.fetcher.get_historical_price('CURRENCY:INVALID',
                                                     datetime.date(2014, 5, 7))
        self.assertIsNone(srcprice)
