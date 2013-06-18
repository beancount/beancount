"""Think-or-Swim platform transaction detail importer.

This code parses the file that can be downloaded from the Think-or-Swim
application from the Activity page.
"""
import csv
import re
import itertools
import datetime

from beancount.core import data
from beancount.core.data import to_decimal
from beancount.core.data import account_from_name
from beancount.core.data import create_simple_posting
from beancount.core.data import Transaction, Posting, Pad, Decimal, Amount, Check
from beancount.core.inventory import Position, Lot
from beancount import utils
from beancount.utils import csv_utils
from beancount.utils.text_utils import Matcher
from beancount.imports import imports


CONFIG = {
    'FILE'               : 'Account for filing',
    'cash_currency'      : 'Currency used for cash account',
    'asset_cash'         : 'Cash account',
    'asset_money_market' : 'Money market account associated with this account',
    'asset_forex'        : 'Retail foreign exchange trading account',
    'asset_position'     : 'Root account for all position sub-accounts',
    'fees'               : 'Fees',
    'commission'         : 'Commissions',
    'interest'           : 'Interest income',
    'dividend_nontax'    : 'Non-taxable dividend income',
    'dividend'           : 'Taxable dividend income',
    'transfer'           : 'Other account for inter-bank transfers',
    'third_party'        : 'Other account for third-party transfers (wires)',
    'adjustment'         : 'Opening balances account, used to make transfer when you opt-in',
}


def import_file(filename, config):
    """Import a CSV file from Think-or-Swim."""

    config = imports.module_config_accountify(config)
    new_entries = []

    cash_currency = config['cash_currency']

    sections = csv_utils.csv_split_sections(csv.reader(open(filename)))
    if 0:
        for section_name, rows in sections.items():
            if re.search(r'\bSummary\b', section_name):
                continue
            print('============================================================', section_name)
            if not rows:
                continue
            irows = iter(rows)
            Tuple = csv_utils.csv_parse_header(next(irows))
            for row in irows:
                obj = Tuple(*row)
                print(obj)

    # Process the cash balance report.
    irows = iter(sections['Cash Balance'])
    prev_balance = Amount(Decimal(), cash_currency)
    prev_date = datetime.date(1970, 1, 1)
    Tuple = csv_utils.csv_parse_header(next(irows))
    matcher = Matcher()
    for index, row in enumerate(itertools.starmap(Tuple, irows)):
        # Skip the empty balances; these aren't interesting.
        if re.search('balance at the start of business day', row.description):
            continue

        # Skip end lines that cannot be parsed.
        if not row.date:
            continue

        # Get the row's date and fileloc.
        fileloc = data.FileLocation(filename, index)
        date = datetime.datetime.strptime(row.date, '%d/%m/%y').date()

        # Insert some Check entries every time the day changed.
        if ((debug and date != prev_date) or
            (not debug and date.month != prev_date.month)):

            prev_date = date
            fileloc = data.FileLocation(filename, index)
            new_entries.append(Check(fileloc, date, config['asset_cash'], prev_balance, None))

        # Create a new transaction.
        narration = "({0.type}) {0.description}".format(row)
        links = set([row.ref])
        entry = Transaction(fileloc, date, data.FLAG_IMPORT, None, narration, None, links, [])

        amount = Decimal(row.amount.replace(',', ''))
        assert not row.fees, row

        balance = Amount(Decimal(row.balance.replace(',', '')), cash_currency)

        if row.description == 'CLIENT REQUESTED ELECTRONIC FUNDING RECEIPT (FUNDS NOW)':
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['transfer'], -amount, cash_currency)

        elif re.match('MONEY MARKET INTEREST', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['interest'], -amount, cash_currency)

        elif re.match('TRANSFER (TO|FROM) FOREX ACCOUNT', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['asset_forex'], -amount, cash_currency)

        elif re.match('ORDINARY DIVIDEND', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['dividend'], -amount, cash_currency)

        elif re.match('NON-TAXABLE DIVIDENDS', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['dividend_nontax'], -amount, cash_currency)

        elif row.description == 'THIRD PARTY':
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['third_party'], -amount, cash_currency)

        elif matcher.match(r'WEB:WEB_UNIF(?:_SNAP)? BOT ([+\-0-9]+) (.+) @([0-9\.]+)', row.description):
            quantity = Decimal(matcher.mo.group(1))
            symbol = matcher.mo.group(2)
            price = Decimal(matcher.mo.group(3))

            account = account_from_name('{}:{}'.format(config['asset_position'].name, symbol))
            cost = Amount(price, cash_currency)
            position = Position(Lot(symbol, cost, None), Decimal(quantity))
            posting = Posting(entry, account, position, None, None)
            entry.postings.append(posting)

            if row.commissions:
                create_simple_posting(entry, config['commission'], -to_decimal(row.commissions), cash_currency)
                amount += Decimal(row.commissions)

            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)

        elif row.description == 'Account Opt In':
            # If this is the first year, an opt-in probably requires an adjustment.
            entry = Pad(fileloc, date, config['asset_cash'], config['adjustment'])
            new_entries.append(entry)

            # And an associated check.
            new_entries.append(Check(fileloc, date, config['asset_cash'], balance, None))

            continue # No entry.

        else:
            raise ValueError("Unknown transaction {}".format(row))

        new_entries.append(entry)
        prev_balance = balance

    return new_entries


debug = False
