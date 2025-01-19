__copyright__ = "Copyright (C) 2018-2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import textwrap
import unittest

from beancount import loader


class TestMatchAverageCost(unittest.TestCase):
    def test_match_average_cost(self):
        setup = """
          plugin "beancount.plugins.check_average_cost"
          plugin "beancount.plugins.auto_accounts"

          2018-01-01 open Assets:US:Cash
          2018-01-01 open Assets:US:Retirement         "NONE"
          2018-01-01 open Income:US:Retirement:Pnl

          2018-05-01 * "Buy"
            Assets:US:Retirement            100 MSFT {56.00 USD}
            Assets:US:Retirement            100 MSFT {60.00 USD}
            Assets:US:Cash               -11600 USD
        """
        for string in [
            """
          2018-09-01 * "Precisely equal."
            Assets:US:Retirement            -50 MSFT {58.00 USD} @ 61.00 USD
            Assets:US:Cash              3050.00 USD
            Income:US:Retirement:Pnl
        """,
            """
          2018-09-02 * "Above but within range."
            Assets:US:Retirement            -50 MSFT {58.00 * 1.01 USD} @ 61.00 USD
            Assets:US:Cash              3050.00 USD
            Income:US:Retirement:Pnl
        """,
            """
          2018-09-03 * "Below but within range."
            Assets:US:Retirement            -50 MSFT {58.00 * 0.99 USD} @ 61.00 USD
            Assets:US:Cash              3050.00 USD
            Income:US:Retirement:Pnl
        """,
        ]:
            entries, errors, _ = loader.load_string(textwrap.dedent(setup + string))
            self.assertEqual([], errors)

        for string in [
            """
          2018-09-04 * "Above and out of range."
            Assets:US:Retirement            -50 MSFT {58.00 * 1.02 USD} @ 61.00 USD
            Assets:US:Cash              3050.00 USD
            Income:US:Retirement:Pnl
        """,
            """
          2018-09-05 * "Below and out of range."
            Assets:US:Retirement            -50 MSFT {58.00 * 0.98 USD} @ 61.00 USD
            Assets:US:Cash              3050.00 USD
            Income:US:Retirement:Pnl
        """,
        ]:
            entries, errors, _ = loader.load_string(textwrap.dedent(setup + string))
            self.assertEqual(1, len(errors))


if __name__ == "__main__":
    unittest.main()
