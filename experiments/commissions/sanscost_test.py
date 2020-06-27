__copyright__ = "Copyright (C) 2013-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.plugins import sanscost
from beancount import loader


class TestSansCost(unittest.TestCase):

    @loader.load_doc()
    def test_simple(self, entries, _, options_map):
        """
        2014-01-01 open  Assets:US:Invest:Cash
        2014-01-01 open  Assets:US:Invest:GOOG
        2014-01-01 open  Income:US:Invest:Rebates
        2014-01-01 open  Income:US:Invest:Gains

        2014-01-01 open  Expenses:Commissions


        2014-02-10 price GOOG  510.00 USD

        2014-02-10 * "Buy" #gains-no-costs
          Assets:US:Invest:Cash                                        -5009.95 USD
          C Expenses:Commissions                                           9.95 USD
          Assets:US:Invest:GOOG                                           10.00 GOOG {500 USD}


        2014-04-10 price GOOG  530.00 USD

        2014-04-10 * "Sell #1" #gains-no-costs
          Assets:US:Invest:GOOG                                           -4.00 GOOG {500 USD}
          C Expenses:Commissions                                           9.95 USD
          Assets:US:Invest:Cash                                         2110.05 USD
          Income:US:Invest:Gains


        2014-05-10 price GOOG  540.00 USD

        2014-05-10 * "Sell #2" #gains-no-costs
          Assets:US:Invest:GOOG                                           -6.00 GOOG {500 USD}
          C Expenses:Commissions                                           9.95 USD
          Assets:US:Invest:Cash                                         3230.05 USD
          Income:US:Invest:Gains

        """
        sans_entries, errors = sanscost.gains_without_cost(entries, options_map)
        self.assertFalse(errors)

        self.assertEqualEntries("""

        2014-01-01 open  Assets:US:Invest:Cash
        2014-01-01 open  Assets:US:Invest:GOOG
        2014-01-01 open  Income:US:Invest:Rebates
        2014-01-01 open  Income:US:Invest:Gains

        2014-01-01 open  Expenses:Commissions


        2014-02-10 price GOOG  510.00 USD

        2014-02-10 * "Buy" #gains-no-costs
          Assets:US:Invest:Cash                                        -5009.95 USD
          X Expenses:Commissions                                           9.95 USD
          X Income:US:Invest:Rebates                                      -9.95 USD
          X Assets:US:Invest:GOOG                                         10.00 GOOG {500.995 USD / aa2ba9695cc7}


        2014-04-10 price GOOG  530.00 USD

        2014-04-10 * "Sell #1" #gains-no-costs
          X Assets:US:Invest:GOOG                                         -4.00 GOOG {aa2ba9695cc7}
          X Expenses:Commissions                                           9.95 USD
          X Income:US:Invest:Rebates                                      -9.95 USD
          Assets:US:Invest:Cash                                         2110.05 USD
          Income:US:Invest:Gains ; Should be (530 - 500) * 4 - 9.95 * (4/10) - 9.95 = 106.07 USD


        2014-05-10 price GOOG  540.00 USD

        2014-05-10 * "Sell #2" #gains-no-costs
          X Assets:US:Invest:GOOG                                         -6.00 GOOG {aa2ba9695cc7}
          X Expenses:Commissions                                           9.95 USD
          X Income:US:Invest:Rebates                                      -9.95 USD
          Assets:US:Invest:Cash                                         3230.05 USD
          Income:US:Invest:Gains ; Should be (540 - 500) * 6 - 9.95 * (6/10) - 9.95 = 224.08 USD

        """, sans_entries)

        # FIXME: I need looser matching here, for matching from a label, such as this: {aa2ba9695cc7}

        # FIXME: Try with multiple flagged legs.
        # FIXME: Try without a leg that has a position at cost.


if __name__ == '__main__':
    unittest.main()
