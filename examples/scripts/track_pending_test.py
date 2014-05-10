#!/usr/bin/env python3
"""Runner for forecast plugin example."""

import unittest
import sys
import tempfile

from beancount import loader
from beancount.parser import printer

import track_pending


EXAMPLE_INPUT = """

2013-01-01 open Expenses:Electricity
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

TRANSFER_ACCOUNT = 'Liabilities:AccountsPayable'

class TestExampleTrackPending(unittest.TestCase):

    def test_check(self):
        original_entries, _, _ = loader.load(EXAMPLE_INPUT, parse_method='string')
        entries = track_pending.tag_pending_transactions(original_entries, 'PENDING')
        self.assertEqual(len(original_entries), len(entries))
        for entry in entries:
            print(entry)
        # self.assertEqual(set(['invoice-562b4da33bd9']), pending_entries[0].links)

    # def test_print_example(self):
    #     "Print output as per the example above."
    #     with tempfile.NamedTemporaryFile('w') as f:
    #         f.write(EXAMPLE_INPUT)
    #         f.flush()
    #         sys.argv = [__file__, '--account={}'.format(TRANSFER_ACCOUNT), f.name]
    #         track_pending.main()


if __name__ == '__main__':
    unittest.main()
