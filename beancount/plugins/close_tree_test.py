__copyright__ = "Copyright (C) 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.parser import cmptest
from beancount.parser import options
from beancount.plugins import close_tree


class TestCloseTree(cmptest.TestCase):
    def test_empty_entries(self):
        new_entries, _ = close_tree.close_tree([], options.OPTIONS_DEFAULTS.copy())
        self.assertEqual([], new_entries)

    @loader.load_doc()
    def test_basic(self, entries, _, options_map):
        """
        2014-01-01 open Assets:XBank
        2014-01-01 open Assets:XBank:AAPL
        2015-01-01 close Assets:XBank
        """
        new_entries, _ = close_tree.close_tree(entries, options_map)

        self.assertEqualEntries(
            """
            2014-01-01 open Assets:XBank
            2014-01-01 open Assets:XBank:AAPL
            2015-01-01 close Assets:XBank
            2015-01-01 close Assets:XBank:AAPL
        """,
            new_entries,
        )

    @loader.load_doc()
    def test_leave_others_untouched(self, entries, _, options_map):
        """
        2014-01-01 open Assets:YBank
        2014-01-01 open Assets:YBank:AAPL
        2014-01-01 open Assets:XBank
        2014-01-01 open Assets:XBank:AAPL
        2014-01-01 open Assets:XBank:AAPL:Fuji
        2014-01-01 open Assets:XBank:AAPL:Gala
        2014-01-01 open Assets:XBank:ORNG
        2014-01-01 open Assets:XBank:BANANA
        2015-01-01 close Assets:XBank
        """

        new_entries, _ = close_tree.close_tree(entries, options_map)
        self.assertEqualEntries(
            """
            2014-01-01 open Assets:YBank
            2014-01-01 open Assets:YBank:AAPL
            2014-01-01 open Assets:XBank
            2014-01-01 open Assets:XBank:AAPL
            2014-01-01 open Assets:XBank:AAPL:Fuji
            2014-01-01 open Assets:XBank:AAPL:Gala
            2014-01-01 open Assets:XBank:ORNG
            2014-01-01 open Assets:XBank:BANANA
            2015-01-01 close Assets:XBank
            2015-01-01 close Assets:XBank:AAPL
            2015-01-01 close Assets:XBank:AAPL:Fuji
            2015-01-01 close Assets:XBank:AAPL:Gala
            2015-01-01 close Assets:XBank:ORNG
            2015-01-01 close Assets:XBank:BANANA
        """,
            new_entries,
        )

    @loader.load_doc()
    def test_override(self, entries, _, options_map):
        """
        2014-01-01 open Assets:XBank
        2014-01-01 open Assets:XBank:AAPL
        2014-01-01 open Assets:XBank:AAPL:Fuji
        2015-01-01 close Assets:XBank:AAPL:Fuji
        2016-01-01 close Assets:XBank
        """
        new_entries, _ = close_tree.close_tree(entries, options_map)

        self.assertEqualEntries(
            """
            2014-01-01 open Assets:XBank
            2014-01-01 open Assets:XBank:AAPL
            2014-01-01 open Assets:XBank:AAPL:Fuji
            2015-01-01 close Assets:XBank:AAPL:Fuji
            2016-01-01 close Assets:XBank
            2016-01-01 close Assets:XBank:AAPL
        """,
            new_entries,
        )

    @loader.load_doc()
    def test_override_complex(self, entries, _, options_map):
        """
        2014-01-01 open Assets:XBank
        2014-01-01 open Assets:XBank:AAPL
        2014-01-01 open Assets:XBank:AAPL:Fuji
        2015-01-01 close Assets:XBank:AAPL
        2016-01-01 close Assets:XBank
        """
        new_entries, _ = close_tree.close_tree(entries, options_map)

        self.assertEqualEntries(
            """
            2014-01-01 open Assets:XBank
            2014-01-01 open Assets:XBank:AAPL
            2014-01-01 open Assets:XBank:AAPL:Fuji

            2015-01-01 close Assets:XBank:AAPL
            2015-01-01 close Assets:XBank:AAPL:Fuji

            2016-01-01 close Assets:XBank
        """,
            new_entries,
        )

    @loader.load_doc()
    def test_match(self, entries, _, options_map):
        """
        2017-11-10 open Liabilities:Credit-Cards:Spouse:Citi
        2017-11-10 open Liabilities:Credit-Cards:Spouse:Citi-CustomCash
        2017-11-10 open Liabilities:Credit-Cards:Spouse:Citi:Addon
        2018-11-10 close Liabilities:Credit-Cards:Spouse:Citi
        """
        new_entries, _ = close_tree.close_tree(entries, options_map)

        self.assertEqualEntries(
            """
            2017-11-10 open Liabilities:Credit-Cards:Spouse:Citi
            2017-11-10 open Liabilities:Credit-Cards:Spouse:Citi-CustomCash
            2017-11-10 open Liabilities:Credit-Cards:Spouse:Citi:Addon
            2018-11-10 close Liabilities:Credit-Cards:Spouse:Citi
            2018-11-10 close Liabilities:Credit-Cards:Spouse:Citi:Addon
        """,
            new_entries,
        )

    @loader.load_doc(expect_errors=True)
    def test_close_unopened_parent(self, entries, _, options_map):
        """
        2017-11-10 open Assets:Brokerage:AAPL
        2017-11-10 open Assets:Brokerage:ORNG
        2018-11-10 close Assets:Brokerage
        """
        new_entries, _ = close_tree.close_tree(entries, options_map)

        self.assertEqualEntries(
            """
            2017-11-10 open Assets:Brokerage:AAPL
            2017-11-10 open Assets:Brokerage:ORNG
            2018-11-10 close Assets:Brokerage:AAPL
            2018-11-10 close Assets:Brokerage:ORNG
        """,
            new_entries,
        )

    @loader.load_doc()
    def test_auto_accounts_parent_close(self, entries, _, options_map):
        """
        plugin "beancount.plugins.auto_accounts"

        2019-01-01 * "Transaction"
          Expenses:Non-Retirement:Auto:Fit:Insurance   -10 USD
          Expenses:Non-Retirement:Auto:Fit:Gas 10 USD

        2021-06-17 close Expenses:Non-Retirement:Auto:Fit
        """
        new_entries, _ = close_tree.close_tree(entries, options_map)

        self.assertEqualEntries(
            """
            2019-01-01 open Expenses:Non-Retirement:Auto:Fit:Insurance
            2019-01-01 open Expenses:Non-Retirement:Auto:Fit:Gas

            2019-01-01 * "Transaction"
              Expenses:Non-Retirement:Auto:Fit:Insurance   -10 USD
              Expenses:Non-Retirement:Auto:Fit:Gas 10 USD

            2021-06-17 open Expenses:Non-Retirement:Auto:Fit
            2021-06-17 close Expenses:Non-Retirement:Auto:Fit:Insurance
            2021-06-17 close Expenses:Non-Retirement:Auto:Fit:Gas
            2021-06-17 close Expenses:Non-Retirement:Auto:Fit
        """,
            new_entries,
        )


if __name__ == "__main__":
    unittest.main()
