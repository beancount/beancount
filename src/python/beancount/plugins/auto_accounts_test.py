__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.parser import cmptest
from beancount.plugins import auto_accounts
from beancount import loader


class TestAutoInsertOpen(cmptest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_auto_open(self, entries, _, options_map):
        """
        2014-02-01 *
          Assets:US:Bank:Checking     100 USD
          Assets:US:Bank:Savings     -100 USD

        2014-03-11 *
          Assets:US:Bank:Checking     100 USD
          Equity:Something           -100 USD
        """
        new_entries, _ = auto_accounts.auto_insert_open(entries, options_map)

        self.assertEqualEntries("""

        2014-02-01 open Assets:US:Bank:Checking
        2014-02-01 open Assets:US:Bank:Savings

        2014-02-01 *
          Assets:US:Bank:Checking     100 USD
          Assets:US:Bank:Savings     -100 USD

        2014-03-11 open Equity:Something

        2014-03-11 *
          Assets:US:Bank:Checking     100 USD
          Equity:Something           -100 USD

        """, new_entries)
