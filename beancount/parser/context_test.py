__copyright__ = "Copyright (C) 2014-2017, 2019-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import textwrap
import unittest

from beancount import loader
from beancount.parser import context
from beancount.utils import test_utils


class TestContext(test_utils.TestCase):
    @loader.load_doc()
    def test_context(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.implicit_prices"

        2012-01-01 open Assets:US:ETrade:Cash                       USD
        2012-01-01 open Assets:US:ETrade:ITOT                       ITOT
        2012-01-01 open Assets:US:ETrade:GLD                        GLD
        2012-01-01 open Income:US:ETrade:Gains                      USD
        2012-01-01 open Expenses:Financial:Commissions              USD

        2012-07-30 * "Buy shares of ITOT"
          Assets:US:ETrade:Cash              -343.35 USD
          Assets:US:ETrade:ITOT                 2.00 ITOT       {167.20 USD}
          Expenses:Financial:Commissions        8.95 USD

        2012-08-31 * "Buy shares of GLD"
          Assets:US:ETrade:Cash              -784.06 USD
          Assets:US:ETrade:GLD                  7.00 GLD       {110.73 USD}
          Expenses:Financial:Commissions        8.95 USD

        2012-08-31 * "Buy shares of ITOT"
          Assets:US:ETrade:Cash              -701.39 USD
          Assets:US:ETrade:ITOT                 4.00 ITOT       {173.11 USD}
          Expenses:Financial:Commissions        8.95 USD

        2012-10-13 * "Buy shares of ITOT"
          Assets:US:ETrade:Cash                                 -2,337.77 USD
          Assets:US:ETrade:ITOT                13.00 ITOT       {179.14 USD}
          Expenses:Financial:Commissions        8.95 USD

        2013-02-01 * "Sell shares of ITOT"
          Assets:US:ETrade:ITOT               -13.00 ITOT       {179.14 USD} @ 186.21 USD
          Assets:US:ETrade:Cash                                  2,411.78 USD
          Expenses:Financial:Commissions        8.95 USD
          Income:US:ETrade:Gains              -91.91 USD

        2013-02-07 * "Buy shares of ITOT"
          Assets:US:ETrade:Cash                                 -1,126.21 USD
          Assets:US:ETrade:ITOT                 6.00 ITOT       {186.21 USD}  ;; *
          Expenses:Financial:Commissions        8.95 USD

        2013-02-07 * "Buy shares of GLD"
          Assets:US:ETrade:Cash                                 -1,287.70 USD
          Assets:US:ETrade:GLD                 11.00 GLD       {116.25 USD}
          Expenses:Financial:Commissions        8.95 USD

        """
        self.assertFalse(errors)

        search_filename = entries[0].meta["filename"]
        search_lineno = entries[-3].meta["lineno"] + 2
        str_context = context.render_file_context(
            entries, options_map, search_filename, search_lineno
        )

        print(str_context)
        self.assertLines(
            textwrap.dedent("""

        ** Transaction Id --------------------------------

        Hash:a4d4f63da17fc113b6b3b902a7dbc6a7
        Location: <string>:36


        ** Balances before transaction --------------------------------

          Assets:US:ETrade:Cash                                                -1754.79 USD

          Assets:US:ETrade:ITOT                          2.00 ITOT {167.20 USD, 2012-07-30}
          Assets:US:ETrade:ITOT                          4.00 ITOT {173.11 USD, 2012-08-31}

          Expenses:Financial:Commissions                                          44.75 USD


        ** Average Costs --------------------------------

          Assets:US:ETrade:ITOT                          6.00 ITOT {171.14 USD, 2012-07-30}


        ** Unbooked Transaction --------------------------------

        2013-02-07 * "Buy shares of ITOT"
          Assets:US:ETrade:Cash           -1126.21 USD                            ;  -1126.21 USD
          Assets:US:ETrade:ITOT               6.00 ITOT {186.21 USD, 2013-02-07}  ; 1117.2600 USD
          Expenses:Financial:Commissions      8.95 USD                            ;      8.95 USD


        ** Transaction --------------------------------

        2013-02-07 * "Buy shares of ITOT"
          Assets:US:ETrade:Cash           -1126.21 USD                            ;  -1126.21 USD
          Assets:US:ETrade:ITOT               6.00 ITOT {186.21 USD, 2013-02-07}  ; 1117.2600 USD
          Expenses:Financial:Commissions      8.95 USD                            ;      8.95 USD


        ** Residual and Tolerances --------------------------------

        Tolerances: ITOT=0.005, USD=0.005
        Basis: (1117.2600 USD)


        ** Balances after transaction --------------------------------

        * Assets:US:ETrade:Cash                                                -2881.00 USD

          Assets:US:ETrade:ITOT                          2.00 ITOT {167.20 USD, 2012-07-30}
          Assets:US:ETrade:ITOT                          4.00 ITOT {173.11 USD, 2012-08-31}
        * Assets:US:ETrade:ITOT                          6.00 ITOT {186.21 USD, 2013-02-07}

        * Expenses:Financial:Commissions                                          53.70 USD

        """),
            str_context,
        )

    maxDiff = 8192


if __name__ == "__main__":
    unittest.main()
