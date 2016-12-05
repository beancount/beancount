__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime

from beancount.parser import cmptest
from beancount.plugins import coherent_cost
from beancount import loader


class TestValidateUnusedAccounts(cmptest.TestCase):

    @loader.load_doc()
    def test_validate_unused_accounts(self, entries, in_errors, options_map):
        """
        2014-01-01 open  Assets:Invest:Shares
        2014-01-01 open  Assets:Invest:Cash
        2014-01-01 open  Equity:Opening-Balances

        2014-02-01 *
          Assets:Invest:Shares       2 AAPL {40.32 USD}
          Assets:Invest:Shares       2 HOOL {700.01 USD}
          Assets:Invest:Cash         -1000.00 GBP @ 1.48066 USD

        2014-03-01 *
          Assets:Invest:Shares       -1 AAPL {40.32 USD} @ 44.30 USD
          Assets:Invest:Cash

        2014-03-02 *
          Assets:Invest:Shares       1 HOOL @ 720.00 USD
          Assets:Invest:Cash

        """
        _, errors = coherent_cost.validate_coherent_cost(entries, options_map)
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, r'\bHOOL\b')
        self.assertEqual(datetime.date(2014, 2, 1), errors[0].entry.date)
