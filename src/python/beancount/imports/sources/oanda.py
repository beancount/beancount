"""OANDA Corporation transaction detail CSV file importer.

Go to the old transaction detail page, select CSV detail, and and cut-n-paste
the output into a file (you have to do this manually, unfortunately, there is no
option).
"""
import re
import datetime
import collections

from beancount.core import data
from beancount.core.data import Posting, Transaction, Check, Note, Decimal, Amount
from beancount.core.data import format_entry
from beancount.core.position import Lot, Position
from beancount.core import compress
from beancount.utils import csv_utils
from beancount.imports import imports


CONFIG = {
    'FILE'     : 'Account for filing',
    'asset'    : 'Account holding the cash margin',
    'interest' : 'Interest income',
    'pnl'      : 'PnL income',
    'transfer' : 'Other account for wire transfers',
    'limbo'    : "Account used to book against transfer where we don't know",
    'fees'     : 'Wire and API fees',
}


def import_file(filename, config):
    config = imports.module_config_accountify(config)
    return import_csv_file(filename, config)


IGNORE_TRANSACTIONS = """
Buy Order
Sell Order
Change Margin
Change Order
Change Trade
Order Cancelled
Order Expired
Order Filled
""".strip().splitlines()

RELEVANT_TRANSACTIONS = """
API Fee
API License Fee
Wire Fee
Buy Market Filled
Close Position
Close Trade
Fund Deposit
Fund Deposit (Transfer)
Fund Withdrawal
Fund Withdrawal (Transfer)
Interest
Sell Market Filled
Stop Loss
Stop Loss (Cancelled)
Take Profit
Trade Cancel
Buy Market
Sell Market
FXGlobalTransfer Sent
FXGlobalTransfer Fee
""".strip().splitlines()


def find_changing_types(filename):
    bytype = collections.defaultdict(list)
    for obj in csv_utils.csv_dict_reader(open(filename)):
        txntype = obj['transaction']
        bytype[txntype].append(obj)

    unchanging_types = set(bytype.keys())
    prev_balance = Decimal()
    for obj in csv_utils.csv_dict_reader(open(filename)):
        balance = obj['balance'].strip()
        if balance and balance != prev_balance:
            if obj['transaction'] in unchanging_types:
                print(obj)
            unchanging_types.discard(obj['transaction'])
            prev_balance = balance

    print("Unchanging types:")
    for txntype in unchanging_types:
        print(txntype)
    print()

    print("Changing types:")
    changing_types = set(bytype.keys()) - unchanging_types
    for txntype in changing_types:
        print(txntype)
    print()


def get_number(obj, aname):
    str_value = obj[aname].strip()
    if str_value:
        return Decimal(str_value)
    else:
        return Decimal()


def is_balanced_txn(obj):
    "Return true if the transaction is supposed to be balanced."
    txntype = obj['transaction']
    return not (txntype.startswith('Fund ') or
                txntype.startswith('FXGlobalTransfer ') or
                txntype.startswith('API ') or
                re.search(txntype, 'Wire Fee') or
                re.search(txntype, 'FXGlobalTransfer Fee'))


ZERO = Decimal()
ONE_CENT = Decimal('0.01')

LINK_FORMAT = 'oanda-{}'

def guess_currency(filename):
    """Try to guess the base currency of the account.
    We use the first transaction with a deposit or something
    that does not involve an instrument."""
    for obj in csv_utils.csv_dict_reader(open(filename)):
        if re.match('[A-Z]+$', obj['pair']):
            return obj['pair']


def oanda_add_posting(entry, account, number, currency):
    position = Position(Lot(currency, None, None), number)
    posting = Posting(entry, account, position, None, None)
    entry.postings.append(posting)


def import_csv_file(filename, config, _):
    new_entries = []

    max_diff = Decimal()
    ## return find_changing_types(filename)

    currency = guess_currency(filename)

    # Iterate over all the transactions in the OANDA account.
    prev_balance = Decimal('0')
    prev_date = datetime.date(1970, 1, 1)
    for lineno, obj in enumerate(csv_utils.csv_dict_reader(open(filename))):
        txntype = obj['transaction']
        date = datetime.datetime.strptime(obj['date'], '%B %d %H:%M:%S %Y %Z').date()

        # Insert some Check entries every month or so.
        if date.month != prev_date.month:
            prev_date = date
            fileloc = data.FileLocation(filename, lineno)
            amount = Amount(prev_balance, currency)
            new_entries.append(Check(fileloc, date, config['asset'], amount, None))

        # Ignore certain ones that have no effect on the balance, they just
        # change our positions.
        if txntype in IGNORE_TRANSACTIONS:
            continue
        assert txntype in RELEVANT_TRANSACTIONS, txntype

        # Get the change amounts.
        interest = get_number(obj, 'interest')
        pnl      = get_number(obj, 'p_l')
        amount   = get_number(obj, 'amount')
        other    = None

        # If the amount of change is supposed to balance, assert the
        # interest/pnl components match the amount.
        if is_balanced_txn(obj):
            assert interest + pnl == amount, (interest, pnl, amount, obj)
        else:
            # Otherwise use the amount itself.
            assert interest == ZERO
            assert pnl == ZERO
            other = amount

            if re.search('Wire Fee', txntype):
                # The wire fee amount is inverted, and book to fees account.
                other = -other
                other_account = config['fees']

            elif re.search('Transfer', txntype):
                # For transfers, use the other OANDA account.
                other_account = config['transfer']
            else:
                # Otherwise, well... we just don't know.
                other_account = config['limbo']

        del amount

        # Compute the change in any case.
        change = pnl + interest + (other or ZERO)

        # Compute the running balance and cross-check against what they report.
        balance = get_number(obj, 'balance')
        computed_balance = prev_balance + change

        # # (Note: there appeared to be a bug in how the balance was reported in
        # # 2007 for "Trade Cancel" entries; for that one we don't check)
        # if (abs(computed_balance - balance) > ONE_CENT and
        #     not (txntype == 'Trade Cancel' and date.year == 2007)):
        #     print("CANCEL_BUG")
        #     print((prev_balance, balance, change), (computed_balance - balance))
        #     pprint(obj)
        #     # raise SystemExit

        # Save the balance to compute the next one.
        prev_balance = balance

        # Create the transaction.
        fileloc = data.FileLocation(filename, lineno)
        narration = '{} - {}'.format(txntype, obj['pair'], obj['ticket'])

        # Create links.
        links = set([LINK_FORMAT.format(obj['ticket'].strip())])
        link = obj['tran_link'].strip()
        if link:
            links.add(LINK_FORMAT.format(link))

        entry = Transaction(fileloc, date, data.data.FLAG_IMPORT, None, narration, None, links, [])

        # FIXME: Add the rates for transfers
        oanda_add_posting(entry, config['asset'], change, currency)
        if pnl != ZERO:
            oanda_add_posting(entry, config['pnl'], -pnl, currency)
        if interest != ZERO:
            oanda_add_posting(entry, config['interest'], -interest, currency)
        if other is not None:
            oanda_add_posting(entry, other_account, -other, currency)

        if len(entry.postings) < 2:
            continue

        new_entries.append(entry)



        diff = balance - computed_balance
        # if diff > 0.5:
        #     print('DIFF', diff)
        if diff > max_diff:
            max_diff = diff

        if 0:
            balance_entry = Note(entry.fileloc, entry.date, config['asset'],
                                 'balance {}  -  {}  =  {}'.format(balance,
                                                                   computed_balance,
                                                                   diff))
            new_entries.append(balance_entry)




        assert len(entry.postings) > 1, format_entry(entry)

    new_entries.sort(key=lambda entry: entry.date)


    if True:
        # Compress all the interest entries for a shorter and cleaner set of
        # imported transactions.
        new_entries = compress.compress(new_entries, lambda entry: re.search('Interest', entry.narration))

    return new_entries


# Future work:
# - Check that ignored transactions have zero amounts
# - Render trades into positions from subaccounts, just like we do for stocks. A
#   large positive number and a large negative number, this should be possible,
#   under e.g. 'Income:US:OANDA:Primary:Positions:EUR_USD'
