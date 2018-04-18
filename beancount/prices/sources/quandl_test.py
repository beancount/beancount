__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
from unittest import mock

from dateutil import tz
import requests

from beancount.prices import source
from beancount.prices.sources import quandl
from beancount.core.number import D
from beancount.utils import date_utils


def response(contents, status_code=requests.codes.ok):
    """Produce a context manager to patch a JSON response."""
    response = mock.Mock()
    response.status_code = status_code
    response.text = ""
    response.json.return_value = contents
    return mock.patch('requests.get', return_value=response)


class QuandlPriceFetcher(unittest.TestCase):

    def test_parse_ticker(self):
        self.assertEqual(('WIKI', 'FB'), quandl.parse_ticker('WIKI:FB'))
        for test in 'WIKI/FB', 'FB', 'WIKI.FB', 'WIKI,FB':
            with self.assertRaises(ValueError):
                quandl.parse_ticker(test)

    def test_error_premium(self):
        contents = {'quandl_error': {
            'code': 'QEPx05',
            'message': ('You have attempted to view a premium database in '
                        'anonymous mode, i.e., without providing a Quandl '
                        'key. Please register for a free Quandl account, '
                        'and then include your API key with your '
                        'requests.')}}
        with response(contents):
            with self.assertRaises(ValueError) as exc:
                quandl.fetch_time_series('WIKI:FB', None)
                self.assertRegex(exc.message, 'premium')

    def test_error_subscription(self):
        contents = {'quandl_error': {
            'code': 'QEPx04',
            'message': ('You do not have permission to view this dataset. '
                        'Please subscribe to this database to get '
                        'access.')}}
        with response(contents):
            with self.assertRaises(ValueError) as exc:
                quandl.fetch_time_series('WIKI:FB', None)
                self.assertRegex(exc.message, 'premium')

    def test_error_network(self):
        with response(None, 404):
            with self.assertRaises(ValueError) as exc:
                quandl.fetch_time_series('WIKI:FB', None)
                self.assertRegex(exc.message, 'premium')

    def _test_valid_response(self):
        contents = {
            'dataset': {'collapse': None,
                        'column_index': None,
                        'column_names': ['Date',
                                         'Open',
                                         'High',
                                         'Low',
                                         'Close',
                                         'Volume',
                                         'Ex-Dividend',
                                         'Split Ratio',
                                         'Adj. Open',
                                         'Adj. High',
                                         'Adj. Low',
                                         'Adj. Close',
                                         'Adj. Volume'],
                        'data': [['2018-03-27',
                                  1063.9,
                                  1064.54,
                                  997.62,
                                  1006.94,
                                  2940957.0,
                                  0.0,
                                  1.0,
                                  1063.9,
                                  1064.54,
                                  997.62,
                                  1006.94,
                                  2940957.0]],
                        'database_code': 'WIKI',
                        'database_id': 4922,
                        'dataset_code': 'GOOGL',
                        'description': 'This dataset has no description.',
                        'end_date': '2018-03-27',
                        'frequency': 'daily',
                        'id': 11304017,
                        'limit': 1,
                        'name': 'Alphabet Inc (GOOGL) Prices, Dividends, Splits and '
                        'Trading Volume',
                        'newest_available_date': '2018-03-27',
                        'oldest_available_date': '2004-08-19',
                        'order': None,
                        'premium': False,
                        'refreshed_at': '2018-03-27T21:46:11.201Z',
                        'start_date': '2004-08-19',
                        'transform': None,
                        'type': 'Time Series'}}
        with response(contents):
            srcprice = quandl.fetch_time_series('WIKI:FB', None)
            self.assertIsInstance(srcprice, source.SourcePrice)

            self.assertEqual(D('1006.94'), srcprice.price)
            self.assertEqual(datetime.datetime(2018, 3, 27, 0, 0, 0,
                                               tzinfo=tz.tzutc()),
                             srcprice.time.astimezone(tz.tzutc()))
            self.assertEqual(None, srcprice.quote_currency)

    def test_valid_response(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._test_valid_response()
