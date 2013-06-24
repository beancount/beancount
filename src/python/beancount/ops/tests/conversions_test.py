"""
Test conversions insertion.
"""
import unittest
import sys
from datetime import date
import textwrap
import functools

from beancount.core.account import Account, is_income_statement_account
from beancount.core import realization
from beancount.ops.pad import pad
from beancount.ops.summarize import summarize, transfer_balances, open_at_date
from beancount import parser
from beancount.parser import parsedoc
from beancount.loader import loaddoc


OPENING_BALANCES = Account('Equity:Opening-Balancess', 'Equity')
TRANSFER_BALANCES = Account('Equity:Retained-Earnings', 'Equity')

class TestConversions(unittest.TestCase):

    @loaddoc
    def test_basic_conversions(self, entries, errors, _):
        """
          2013-02-01 open Income:Job
          2013-02-01 open Assets:Checking
          2013-02-01 open Assets:Invest
          2013-02-01 open Assets:Invest:STOCK  STOCK

          2011-03-01 * "Earn some money"
            Income:Job            -1000 USD
            Assets:Checking        1000 USD

          2012-03-02 * "Transfer to Investment"
            Assets:Checking       -800 USD
            Assets:Invest          800 USD

          2012-03-03 * "Buy some stock"
            Assets:Invest         -600 USD
            Assets:Invest:STOCK    60 STOCK {10 USD}
        """
        real_accounts = realization.realize(entries)
        realization.dump_tree_balances(real_accounts, sys.stdout)



# ## FIXME: complete Conversions transfer.
#     __TEST_CONVERSIONS__ = False
#     if __TEST_CONVERSIONS__:

#         # Insert entries to account for conversions until the begin date.
#         iter_entries = iter(entries)
#         prev_balance = inventory.Inventory()
#         for begin_index, entry in enumerate(iter_entries):
#             if entry.date >= begin_date:
#                 last_entry = entry
#                 break
#             if isinstance(entry, Transaction):
#                 for posting in entry.postings:
#                     prev_balance.add_position(posting.position, allow_negative=True)

#         print('prev_balance', prev_balance.get_cost())

#         next_balance = copy.copy(prev_balance)
#         for entry in itertools.chain((last_entry,), iter_entries):
#             if isinstance(entry, Transaction):
#                 for posting in entry.postings:
#                     next_balance.add_position(posting.position, allow_negative=True)

#         print('next_balance', next_balance.get_cost())

#         fileloc = FileLocation('<conversions>', -1)
#         narration = 'Conversion for {}'.format(next_balance)
#         conversion_entry = Transaction(fileloc, end_date, flags.FLAG_CONVERSIONS, None, narration, None, None, [])
#         for position in next_balance.get_cost().get_positions():
#             conversion_entry.postings.append(
#                 Posting(conversion_entry, account_conversions, -position, None, None))


#     if __TEST_CONVERSIONS__:
#         entries.append(conversion_entry)
