#!/usr/bin/env python3
"""Runner for forecast plugin example."""

import unittest

from beancount import loader
from beancount.parser import printer

import forecast


EXAMPLE_INPUT = """

2011-01-01 open Expenses:Restaurant
2011-01-01 open Assets:Cash

2011-05-17 # "Something [MONTHLY]"
  Expenses:Restaurant   50.02 USD
  Assets:Cash

"""

class TestExampleForecast(unittest.TestCase):

    def setUp(self):
        loader.install_load_filter(forecast.forecast_filter)

    def tearDown(self):
        loader.uninstall_load_filter(forecast.forecast_filter)

    def test_forecast(self):
        entries, errors, options = loader.load(EXAMPLE_INPUT,
                                               parse_method='string')
        self.assertLessEqual(3, len(entries))
        for entry in entries:
            print(printer.format_entry(entry))

if __name__ == '__main__':
    unittest.main()
