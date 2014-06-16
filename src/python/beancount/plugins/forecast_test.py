import textwrap

from beancount import loader
from beancount.plugins import forecast
from beancount.parser import cmptest


class TestExampleForecast(cmptest.TestCase):

    def setUp(self):
        loader.install_load_plugin(forecast.forecast_plugin)

    def tearDown(self):
        loader.uninstall_load_plugin(forecast.forecast_plugin)

    def test_forecast(self):
        INPUT = textwrap.dedent("""

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Cash

            2011-05-17 # "Something [MONTHLY]"
              Expenses:Restaurant   50.02 USD
              Assets:Cash

        """)
        entries, _, __ = loader.load(INPUT, parse_method='string')
        self.assertEqualEntries("""

            2011-01-01 open Expenses:Restaurant
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
        """, entries)
