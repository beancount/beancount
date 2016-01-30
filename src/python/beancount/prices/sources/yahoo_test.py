__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import datetime
import unittest
from unittest import mock
from urllib import error


from beancount.prices.sources import yahoo
from beancount.core.number import D
from beancount.core.number import Decimal


class YahooFinancePriceFetcher(unittest.TestCase):

    def setUp(self):
        self.fetcher = yahoo.Source()
        self.url_object = mock.MagicMock()
        self.url_object.read = mock.MagicMock()
        self.url_object.getcode = mock.MagicMock(return_value=200)
        mock.patch('urllib.request.urlopen', return_value=self.url_object).start()
        self.addCleanup(mock.patch.stopall)

    def test_get_latest_price(self):
        # Test all four possible URL fetches.

        # c4l1d1
        self.url_object.read.side_effect = [b'USD,553.37,"12/7/2015"\r\n']
        srcprice = self.fetcher.get_latest_price('HOOL')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('553.37'), srcprice.price)

        # c4b3b2d2
        self.url_object.read.side_effect = [b'N/A,N/A\r\n',
                                            b'USD,553.37,556.70,"12/7/2015"\r\n']
        srcprice = self.fetcher.get_latest_price('HOOL')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('555.035'), srcprice.price)

        # c4b0a0d2
        self.url_object.read.side_effect = [b'N/A,N/A\r\n',
                                            b'N/A,N/A\r\n',
                                            b'USD,553.37,556.70,"12/7/2015"\r\n']
        srcprice = self.fetcher.get_latest_price('HOOL')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('555.035'), srcprice.price)

        # c4p0d2
        self.url_object.read.side_effect = [b'N/A,N/A\r\n',
                                            b'N/A,N/A\r\n',
                                            b'N/A,N/A\r\n',
                                            b'USD,553.37,"12/7/2015"\r\n']
        srcprice = self.fetcher.get_latest_price('HOOL')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('553.37'), srcprice.price)

        # None is valid.
        self.url_object.read.side_effect = [b'N/A,N/A\r\n',
                                            b'N/A,N/A\r\n',
                                            b'N/A,N/A\r\n',
                                            b'N/A,N/A\r\n']
        srcprice = self.fetcher.get_latest_price('HOOL')
        self.assertIsNone(srcprice)

    def test_get_latest_price__invalid(self):
        self.url_object.read.return_value = b'N/A,N/A\r\n'
        srcprice = self.fetcher.get_latest_price('INVALID')
        self.assertIsNone(srcprice)

    def test_get_historical_price(self):
        self.url_object.read.return_value = textwrap.dedent("""
           Date,Open,High,Low,Close,Volume,Adj Close
           2014-05-06,525.23,526.81,515.06,515.14,1684400,515.14
           2014-05-05,524.82,528.90,521.32,527.81,1021300,527.81
           2014-05-02,533.76,534.00,525.61,527.93,1683900,527.93
        """).encode('utf-8')
        request_date = datetime.date(2014, 5, 7)
        expected_date = datetime.date(2014, 5, 6)
        srcprice = self.fetcher.get_historical_price('HOOL', request_date)
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('515.14'), srcprice.price)
        self.assertEqual(expected_date, srcprice.time.date())

    def test_get_historical_price__invalid(self):
        self.url_object.read.side_effect = error.HTTPError('url', 'code', '404', {}, None)
        srcprice = self.fetcher.get_historical_price('INVALID', datetime.date(2014, 5, 7))
        self.assertIsNone(srcprice)
