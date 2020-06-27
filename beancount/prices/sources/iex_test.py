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
        contents = [{"symbol": "HOOL",
                     "price": 183.61,
                     "size": 100,
                     "time": 1590177596030}]
        with response(contents):
            srcprice = iex.fetch_quote('HOOL')
            self.assertIsInstance(srcprice, source.SourcePrice)
            self.assertEqual(D('183.61'), srcprice.price)
            self.assertEqual(
                datetime.datetime(2020, 5, 22, 19, 59, 56, 30000, tzinfo=tz.tzutc()),
                srcprice.time.astimezone(tz.tzutc()))
            self.assertEqual('USD', srcprice.quote_currency)

    def test_valid_response(self):
        for tzname in "America/New_York", "Europe/Berlin", "Asia/Tokyo":
            with date_utils.intimezone(tzname):
                self._test_valid_response()


if __name__ == '__main__':
    unittest.main()
