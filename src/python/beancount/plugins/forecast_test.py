import unittest

from beancount import loader
from beancount.parser import printer
from beancount.plugins import forecast


EXAMPLE_INPUT = """

2011-01-01 open Expenses:Restaurant
2011-01-01 open Assets:Cash

2011-05-17 # "Something [MONTHLY]"
  Expenses:Restaurant   50.02 USD
  Assets:Cash

"""

class TestExampleForecast(unittest.TestCase):

    def setUp(self):
        loader.install_load_plugin(forecast.forecast_plugin)

    def tearDown(self):
        loader.uninstall_load_plugin(forecast.forecast_plugin)

    def test_forecast(self):
        entries, errors, options = loader.load(EXAMPLE_INPUT,
                                               parse_method='string')
        self.assertLess(3, len(entries))


__incomplete__ = True

