"""
Test conversions insertion.
"""
import unittest
import sys

from beancount.core.account import Account
from beancount.core import realization
from beancount.core import data
from beancount.core import flags
from beancount.ops import summarize
from beancount.loader import loaddoc


ACCOUNT_CONVERSIONS1 = Account('Equity:Conversions:Previous', 'Equity')
ACCOUNT_CONVERSIONS2 = Account('Equity:Conversions:Current', 'Equity')

class TestConversions(unittest.TestCase):

    @loaddoc
    def test_basic_conversions(self, entries, errors, _):
        """
          2013-02-01 open Income:US:Job           USD
          2013-02-01 open Assets:US:Checking      USD
          2013-02-01 open Assets:CA:Invest        CAD
          2013-02-01 open Assets:CA:Invest:NT     NT

          2011-03-01 * "Earn some money"
            Income:US:Job            -1000 USD
            Assets:US:Checking        1000 USD

          2012-03-02 * "Transfer to Investment"
            Assets:US:Checking       -800 USD
            Assets:CA:Invest          800 CAD @ 1 USD

          2012-03-03 * "Buy some stock"
            Assets:CA:Invest         -600 CAD
            Assets:CA:Invest:NT        60 NT {10 CAD}

          2013-02-01 * "Transfer some money back"
            Assets:CA:Invest         -100 CAD @ 1 USD
            Assets:US:Checking        100 USD

        """
        conversion_entries = conversions(entries, ACCOUNT_CONVERSIONS1)
        entries += conversion_entries

        converted_balance = summarize.compute_total_balance(entries)
        assert converted_balance.get_cost().is_empty()


        real_accounts = realization.realize(entries)
        realization.dump_tree_balances(real_accounts, sys.stdout)



def conversions(entries, account_conversions):

    balance = summarize.compute_total_balance(entries)
    print(balance.get_cost())


    # # Insert entries to account for conversions until the begin date.
    # iter_entries = iter(entries)
    # prev_balance = inventory.Inventory()
    # for begin_index, entry in enumerate(iter_entries):
    #     if entry.date >= begin_date:
    #         last_entry = entry
    #         break
    #     if isinstance(entry, Transaction):
    #         for posting in entry.postings:
    #             prev_balance.add_position(posting.position, allow_negative=True)

    # print('prev_balance', prev_balance.get_cost())

    # next_balance = copy.copy(prev_balance)
    # for entry in itertools.chain((last_entry,), iter_entries):
    #     if isinstance(entry, Transaction):
    #         for posting in entry.postings:
    #             next_balance.add_position(posting.position, allow_negative=True)

    # print('next_balance', next_balance.get_cost())

    new_entries = []

    if not balance.is_empty():

        last_date = entries[-1].date

        fileloc = data.FileLocation('<conversions>', -1)
        narration = 'Conversion for {}'.format(balance)
        conversion_entry = data.Transaction(fileloc, last_date, flags.FLAG_CONVERSIONS, None, narration, None, None, [])
        for position in balance.get_cost().get_positions():
            conversion_entry.postings.append(
                data.Posting(conversion_entry, account_conversions, -position, None, None))

        new_entries.append(conversion_entry)

    return new_entries
