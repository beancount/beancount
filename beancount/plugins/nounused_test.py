__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.parser import cmptest
from beancount.plugins import nounused
from beancount import loader


class TestValidateUnusedAccounts(cmptest.TestCase):

    @loader.load_doc()
    def test_validate_unused_accounts(self, entries, _, options_map):
        """
        2014-01-01 open  Assets:Account1 ; Used, kept open
        2014-01-01 open  Assets:Account2 ; Used and closed
        2014-01-01 open  Assets:Account3 ; Unused
        2014-01-01 open  Equity:Opening-Balances

        2014-02-01 *
          Assets:Account1            1 USD
          Assets:Account2            1 USD
          Equity:Opening-Balances   -2 USD

        2014-06-01 close Assets:Account2
        """
        _, errors = nounused.validate_unused_accounts(entries, options_map)
        self.assertEqual(1, len(errors))
        self.assertEqual('Assets:Account3', errors[0].entry.account)


if __name__ == '__main__':
    unittest.main()
