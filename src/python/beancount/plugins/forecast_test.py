import textwrap
import unittest

from beancount import loader
from beancount.plugins import forecast
from beancount.parser import printer


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
        entries, _, __ = loader.load(EXAMPLE_INPUT, parse_method='string')
        self.assertLess(3, len(entries))


        expected_entries, _, __ = loader.load(EXAMPLE_INPUT, parse_method='string')
        expected = textwrap.dedent("""
            2011-01-01 open Assets:Cash

            2011-06-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD

            2011-07-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD

            2011-08-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD

            2011-09-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD

            2011-10-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD

            2011-11-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD

            2011-12-17 # "Something"
              Expenses:Restaurant                                                     50.02 USD
              Assets:Cash                                                            -50.02 USD
        """)

        # FIXME: TODO - build a routine to easily compare entries, and more
        # importantly, sets of entries, regardless of ordering in a file.
        # This will be very useful somewhere else!
        print(entries[0] == expected_entries[0])

        printer.print_entries(entries)
        printer.print_entries(expected_entries)

__incomplete__ = True
