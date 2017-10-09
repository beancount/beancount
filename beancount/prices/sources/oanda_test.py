"""Test for price extractor of OANDA.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest
from unittest import mock

from dateutil import tz

from beancount.prices.sources import oanda
from beancount.prices import source
from beancount.core.number import D
from beancount.utils import net_utils


def response(code, contents=None):
    urlopen = mock.MagicMock(return_value=None)
    if isinstance(contents, str):
        response = mock.MagicMock()
        response.read = mock.MagicMock(return_value=contents.encode('utf-8'))
        response.getcode = mock.MagicMock(return_value=200)
        urlopen.return_value = response
    return mock.patch.object(net_utils, 'retrying_urlopen', urlopen)


class TestOandaMisc(unittest.TestCase):

    def test_get_currencies(self):
        self.assertEqual(('USD', 'CAD'), oanda._get_currencies('USD_CAD'))

    def test_get_currencies_invalid(self):
        self.assertEqual((None, None), oanda._get_currencies('USDCAD'))


class TestOandaFetchCandles(unittest.TestCase):

    @response(404)
    def test_null_response(self):
        self.assertIs(None, oanda._fetch_candles({}))

    @response(200, '''
        {
                "instrument" : "USD_CAD",
                "granularity" : "S5"
        }
    ''')
    def test_key_error(self):
        self.assertIs(None, oanda._fetch_candles({}))

    @response(200, '''
        {
                "instrument" : "USD_CAD",
                "granularity" : "S5",
                "candles" : [
                        {
                                "time" : "2017-01-23T00:45:15.000000Z",
                                "openMid" : 1.330115,
                                "highMid" : 1.33012,
                                "lowMid" : 1.33009,
                                "closeMid" : 1.33009,
                                "volume" : 9,
                                "complete" : true
                        },
                        {
                                "time" : "2017-01-23T00:45:20.000000Z",
                                "openMid" : 1.330065,
                                "highMid" : 1.330065,
                                "lowMid" : 1.330065,
                                "closeMid" : 1.330065,
                                "volume" : 1,
                                "complete" : true
                        }
                ]
        }
    ''')
    def test_valid(self):
        self.assertEqual([
            (datetime.datetime(2017, 1, 23, 0, 45, 15, tzinfo=tz.tzutc()), D('1.330115')),
            (datetime.datetime(2017, 1, 23, 0, 45, 20, tzinfo=tz.tzutc()), D('1.330065'))
        ], oanda._fetch_candles({}))


class TestOandaGetLatest(unittest.TestCase):

    def setUp(self):
        self.fetcher = oanda.Source()

    def test_invalid_ticker(self):
        srcprice = self.fetcher.get_latest_price('NOTATICKER')
        self.assertIsNone(srcprice)

    def test_no_candles(self):
        with mock.patch.object(oanda, '_fetch_candles', return_value=None):
            self.assertEqual(None, self.fetcher.get_latest_price('USD_CAD'))


    def test_valid(self):
        candles = [
            (datetime.datetime(2017, 1, 21, 0, 45, 15, tzinfo=tz.tzutc()), D('1.330115')),
            (datetime.datetime(2017, 1, 21, 0, 45, 20, tzinfo=tz.tzutc()), D('1.330065'))
        ]
        now = datetime.datetime(2017, 1, 22, 19, 55, 53, 235947)
        with mock.patch.object(oanda, '_fetch_candles', return_value=candles),\
             mock.patch('datetime.datetime') as mock_dt:
            mock_dt.now.return_value = now
            srcprice = self.fetcher.get_latest_price('USD_CAD')
            # Latest price, with current time as time.
            self.assertEqual(source.SourcePrice(D('1.330065'), now, 'CAD'), srcprice)


class TestOandaGetHistorical(unittest.TestCase):

    def setUp(self):
        self.fetcher = oanda.Source()

    def test_invalid_ticker(self):
        srcprice = self.fetcher.get_latest_price('NOTATICKER')
        self.assertIsNone(srcprice)

    def test_no_candles(self):
        with mock.patch.object(oanda, '_fetch_candles', return_value=None):
            self.assertEqual(None, self.fetcher.get_latest_price('USD_CAD'))

    def _check_valid(self, query_date, out_time, out_price):
        # pylint: disable=bad-whitespace
        candles = [
            (datetime.datetime(2017, 1, 21,  0, 0, 0, tzinfo=tz.tzutc()), D('1.3100')),
            (datetime.datetime(2017, 1, 21,  8, 0, 0, tzinfo=tz.tzutc()), D('1.3300')),
            (datetime.datetime(2017, 1, 21, 16, 0, 0, tzinfo=tz.tzutc()), D('1.3500')),
            (datetime.datetime(2017, 1, 22,  0, 0, 0, tzinfo=tz.tzutc()), D('1.3700')),
            (datetime.datetime(2017, 1, 22,  8, 0, 0, tzinfo=tz.tzutc()), D('1.3900')),
            (datetime.datetime(2017, 1, 22, 16, 0, 0, tzinfo=tz.tzutc()), D('1.4100')),
            (datetime.datetime(2017, 1, 23,  0, 0, 0, tzinfo=tz.tzutc()), D('1.4300')),
            (datetime.datetime(2017, 1, 23,  8, 0, 0, tzinfo=tz.tzutc()), D('1.4500')),
            (datetime.datetime(2017, 1, 23, 16, 0, 0, tzinfo=tz.tzutc()), D('1.4700')),
        ]
        with mock.patch.object(oanda, '_fetch_candles', return_value=candles):
            srcprice = self.fetcher.get_historical_price('USD_CAD', query_date)
            self.assertEqual(source.SourcePrice(out_price, out_time, 'CAD'),
                             srcprice)


    def test_valid_same_date(self):
        self._check_valid(
            datetime.date(2017, 1, 22),
            datetime.datetime(2017, 1, 22, 16, 0, tzinfo=tz.tzutc()),
            D('1.4100'))

    def test_valid_before(self):
        self._check_valid(
            datetime.date(2017, 1, 23),
            datetime.datetime(2017, 1, 23, 16, 0, tzinfo=tz.tzutc()),
            D('1.4700'))

    def test_valid_after(self):
        self._check_valid(
            datetime.date(2017, 1, 20),
            datetime.datetime(2017, 1, 21, 0, 0, tzinfo=tz.tzutc()),
            D('1.3100'))
