import unittest
from unittest import mock

import requests

from beancount.core.number import D
from beancount.prices import source
from beancount.prices.sources import coinbase


def response(contents, status_code=requests.codes.ok):
    """Produce a context manager to patch a JSON response."""
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
        contents = {
            "data": {
                "base": "BTC",
                "currency": "USD",
                "amount": 101.23
            }
        }

        with response(contents):
            srcprice = coinbase.fetch_quote('BTC-GBP')
            self.assertIsInstance(srcprice, source.SourcePrice)
            self.assertEqual(D('101.23'), srcprice.price)
            self.assertEqual('USD', srcprice.quote_currency)
