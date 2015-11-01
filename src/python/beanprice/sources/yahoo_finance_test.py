__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import datetime
import unittest
from unittest import mock
import urllib.request


from beanprice.sources import yahoo_finance
from beancount.core.amount import D, Decimal


class YahooFinancePriceFetcher(unittest.TestCase):

    def setUp(self):
        self.fetcher = yahoo_finance.YahooFinancePriceFetcher('GOOG', 'GOOG', 'USD')
        self.url_object = mock.MagicMock()
        self.url_object.read = mock.MagicMock()
        urllib.request.urlopen = mock.MagicMock(return_value=self.url_object)

    def test_get_latest_price(self):
        self.url_object.read.return_value = b'550.74,556.00\r\n'
        price, unused_time = self.fetcher.get_latest_price()
        self.assertTrue(isinstance(price, Decimal))
        self.assertEqual(D('553.37'), price)

    def test_get_historical_price(self):
        self.url_object.read.return_value = textwrap.dedent("""
           Date,Open,High,Low,Close,Volume,Adj Close
           2014-05-06,525.23,526.81,515.06,515.14,1684400,515.14
           2014-05-05,524.82,528.90,521.32,527.81,1021300,527.81
           2014-05-02,533.76,534.00,525.61,527.93,1683900,527.93
        """).encode('utf-8')
        request_date = datetime.date(2014, 5, 7)
        expected_date = datetime.date(2014, 5, 6)
        price, date = self.fetcher.get_historical_price(request_date)
        self.assertTrue(isinstance(price, Decimal))
        self.assertEqual(D('515.14'), price)
        self.assertEqual(expected_date, date)
