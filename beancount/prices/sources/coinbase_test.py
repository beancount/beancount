import datetime
import unittest

from unittest import mock
from dateutil import tz

import requests

from beancount.core.number import D
from beancount.prices import source
from beancount.prices.sources import coinbase


def response(contents, status_code=requests.codes.ok):
    """Return a context manager to patch a JSON response."""
    response = mock.Mock()
    response.status_code = status_code
    response.text = ""
    response.json.return_value = contents
    return mock.patch('requests.get', return_value=response)


class CoinbasePriceFetcher(unittest.TestCase):

    def test_error_network(self):
        with response(None, 404):
            with self.assertRaises(ValueError) as exc:
                coinbase.fetch_quote('AAPL')
                self.assertRegex(exc.message, 'premium')

    def test_valid_response(self):
        contents = {"data": {"base": "BTC",
                             "currency": "USD",
                             "amount": "101.23456"}}
        with response(contents):
            srcprice = coinbase.Source().get_latest_price('BTC-GBP')
            self.assertIsInstance(srcprice, source.SourcePrice)
            self.assertEqual(D('101.23456'), srcprice.price)
            self.assertEqual('USD', srcprice.quote_currency)

    def test_historical_price(self):
        contents = {"data": {"base": "BTC",
                             "currency": "USD",
                             "amount": "101.23456"}}
        with response(contents):
            time = datetime.datetime(2018, 3, 27, 0, 0, 0, tzinfo=tz.tzutc())
            srcprice = coinbase.Source().get_historical_price('BTC-GBP', time)
            self.assertIsInstance(srcprice, source.SourcePrice)
            self.assertEqual(D('101.23456'), srcprice.price)
            self.assertEqual('USD', srcprice.quote_currency)
            self.assertEqual(datetime.datetime(2018, 3, 27, 0, 0, 0, tzinfo=tz.tzutc()),
                             srcprice.time)


if __name__ == '__main__':
    unittest.main()
