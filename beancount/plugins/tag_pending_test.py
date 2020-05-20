__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.core import data
from beancount.plugins import tag_pending
from beancount.utils import test_utils


class TestExampleTrackPending(test_utils.TestCase):

    @test_utils.docfile
    def test_tag_pending(self, filename):
        """
        2013-01-01 open Expenses:Electricity
        2013-01-01 open Expenses:Gas
        2013-01-01 open Assets:Checking
        2013-01-01 open Liabilities:AccountsPayable

        2013-03-28 * "Bill for datacenter electricity"  ^invoice-27a30ab61191
          Expenses:Electricity                    450.82 USD
          Liabilities:AccountsPayable

        2013-03-30 * "Bill for gas" ^invoice-562b4da33bd9
          Expenses:Gas                      204.20 USD
          Liabilities:AccountsPayable

        2013-04-15 * "Paying electricity company" ^invoice-27a30ab61191
          Assets:Checking                               -450.82 USD
          Liabilities:AccountsPayable

        """
        original_entries, errors, _ = loader.load_file(filename)
        self.assertFalse(errors)
        entries = tag_pending.tag_pending_transactions(original_entries, 'PENDING')
        self.assertEqual(len(original_entries), len(entries))
        self.assertEqual(data.EMPTY_SET, entries[4].tags)
        self.assertEqual(set(['PENDING']), entries[5].tags)
        self.assertEqual(data.EMPTY_SET, entries[6].tags)


if __name__ == '__main__':
    unittest.main()
