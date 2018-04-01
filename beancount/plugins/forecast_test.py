__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import textwrap

from beancount import loader
from beancount.parser import cmptest


class TestExampleForecast(cmptest.TestCase):

    def test_forecast(self):
        input_text = textwrap.dedent("""

            plugin "beancount.plugins.forecast"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Cash

            2011-05-17 # "Something [MONTHLY UNTIL 2011-12-31]"
              Expenses:Restaurant   50.02 USD
              Assets:Cash

        """)
        entries, errors, __ = loader.load_string(input_text)
        self.assertFalse(errors)
        self.assertEqualEntries("""

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Cash

            2011-05-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-06-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-07-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-08-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-09-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-10-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-11-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD

            2011-12-17 # "Something"
              Expenses:Restaurant           50.02 USD
              Assets:Cash                  -50.02 USD
        """, entries)
