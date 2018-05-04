"""Test for price extractor of OANDA.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import os
import time
import datetime
import unittest
from unittest import mock

from dateutil import tz

from beancount.prices.sources import oanda
from beancount.prices import source
from beancount.core.number import D
from beancount.utils import net_utils
from beancount.utils import date_utils


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


class TimezoneTestBase:

    def setUp(self):
        tz_value = 'Europe/Berlin'
        self.tz_old = os.environ.get('TZ', None)
        os.environ['TZ'] = tz_value
        time.tzset()

    def tearDown(self):
        if self.tz_old is None:
            del os.environ['TZ']
        else:
            os.environ['TZ'] = self.tz_old
        time.tzset()


class TestOandaFetchCandles(TimezoneTestBase, unittest.TestCase):

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

    def _test_valid(self):
        candles = [
            (datetime.datetime(2017, 1, 21, 0, 45, 15, tzinfo=tz.tzutc()), D('1.330115')),
            (datetime.datetime(2017, 1, 21, 0, 45, 20, tzinfo=tz.tzutc()), D('1.330065')),
        ]
        with mock.patch.object(oanda, '_fetch_candles', return_value=candles):
            srcprice = self.fetcher.get_latest_price('USD_CAD')
            # Latest price, with current time as time.
            self.assertEqual(source.SourcePrice(
                D('1.330065'),
                datetime.datetime(2017, 1, 21, 0, 45, 20, tzinfo=tz.tzutc()),
                'CAD'), srcprice)

    def test_valid(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._test_valid()


class TestOandaGetHistorical(TimezoneTestBase, unittest.TestCase):

    def setUp(self):
        self.fetcher = oanda.Source()
        super(TestOandaGetHistorical, self).setUp()

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
            query_time = datetime.datetime.combine(
                query_date, time=datetime.time(16, 0, 0), tzinfo=tz.tzutc())
            srcprice = self.fetcher.get_historical_price('USD_CAD', query_time)
            if out_time is not None:
                self.assertEqual(source.SourcePrice(out_price, out_time, 'CAD'), srcprice)
            else:
                self.assertEqual(None, srcprice)

    def test_valid_same_date(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._check_valid(
                    datetime.date(2017, 1, 22),
                    datetime.datetime(2017, 1, 22, 16, 0, tzinfo=tz.tzutc()),
                    D('1.4100'))

    def test_valid_before(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._check_valid(
                    datetime.date(2017, 1, 23),
                    datetime.datetime(2017, 1, 23, 16, 0, tzinfo=tz.tzutc()),
                    D('1.4700'))

    def test_valid_after(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._check_valid(datetime.date(2017, 1, 20), None, None)
