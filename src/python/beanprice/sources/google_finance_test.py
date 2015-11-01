__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import datetime
import unittest
from unittest import mock
import urllib.request


from beanprice.sources import google_finance
from beancount.core.amount import D, Decimal


class GoogleFinancePriceFetcher(unittest.TestCase):

    def setUp(self):
        self.fetcher = google_finance.GoogleFinancePriceFetcher('GOOG', 'GOOG', 'USD', exchange='NASD')
        self.url_object = mock.MagicMock()
        self.url_object.read = mock.MagicMock()
        urllib.request.urlopen = mock.MagicMock(return_value=self.url_object)

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
        price, time = self.fetcher.get_latest_price()
        self.assertTrue(isinstance(price, Decimal))
        self.assertEqual(D('556.33'), price)
        self.assertEqual(time, datetime.datetime(2014, 6, 6, 16, 0, 0))

    def test_get_historical_price(self):
        # That first character before date is the BOM.
        self.url_object.read.return_value = textwrap.dedent("""\
            ﻿Date,Open,High,Low,Close,Volume
            6-May-14,525.23,526.81,515.06,515.14,1684381
            5-May-14,524.82,528.90,521.32,527.81,1021408
            2-May-14,533.76,534.00,525.61,527.93,1685042
        """).encode('utf-8')
        request_date = datetime.date(2014, 5, 7)
        expected_date = datetime.date(2014, 5, 6)
        price, date = self.fetcher.get_historical_price(request_date)
        self.assertTrue(isinstance(price, Decimal))
        self.assertEqual(D('515.14'), price)
        self.assertEqual(expected_date, date)
