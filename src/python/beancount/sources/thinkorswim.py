"""Think-or-Swim platform transaction detail importer.

This code parses the file that can be downloaded from the Think-or-Swim
application from the Activity page.
"""
import csv
import re
import itertools
import datetime

from beancount.core import data
from beancount.core.amount import to_decimal, Decimal, Amount, ZERO
from beancount.core.account import account_from_name
from beancount.core.data import create_simple_posting
from beancount.core.data import Transaction, Posting, Pad, Check
from beancount.core.position import Lot, Position
from beancount.utils import csv_utils
from beancount.utils.text_utils import Matcher
from beancount.core.account import accountify_dict
from beancount.core import flags


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
    'pnl'                : 'Capital Gains/Losses',
    'transfer'           : 'Other account for inter-bank transfers',
    'third_party'        : 'Other account for third-party transfers (wires)',
    'adjustment'         : 'Opening balances account, used to make transfer when you opt-in',
}


def import_file(filename, config):
    """Import a CSV file from Think-or-Swim."""

    config = accountify_dict(config)
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

    return process_cash(sections['Cash Balance'], filename, config)


def process_cash(section, filename, config):
    """Process the cash balance report, which contains stocks; this is the main
    account.
    """
    new_entries = []
    cash_currency = config['cash_currency']

    irows = iter(section)
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
        entry = Transaction(fileloc, date, flags.FLAG_IMPORT, None, narration, None, links, [])

        amount = convert_number(row.amount)
        if row.type != 'TRD':
            assert not row.fees, row
            assert not row.commissions, row

        balance = Amount(convert_number(row.balance), cash_currency)

        if row.type == 'EFN':
            assert row.description == 'CLIENT REQUESTED ELECTRONIC FUNDING RECEIPT (FUNDS NOW)'
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['transfer'], -amount, cash_currency)

        elif row.type == 'RAD':
            assert re.match('MONEY MARKET INTEREST', row.description)
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['interest'], -amount, cash_currency)

        elif row.type == 'JRN':
            assert re.match('TRANSFER (TO|FROM) FOREX ACCOUNT', row.description)
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['asset_forex'], -amount, cash_currency)

        elif row.type == 'DOI':
            if re.match('ORDINARY DIVIDEND', row.description):
                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['dividend'], -amount, cash_currency)

            elif re.match('NON-TAXABLE DIVIDENDS', row.description):
                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['dividend_nontax'], -amount, cash_currency)
            else:
                assert False, row.description

        elif row.type == 'WIN':
            assert row.description == 'THIRD PARTY'
            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            create_simple_posting(entry, config['third_party'], -amount, cash_currency)

        elif row.type == 'TRD':
            matcher.match(r'WEB:WEB_UNIF(?:_SNAP)? (BOT|SOLD) ([+\-0-9]+) (.+) @([0-9\.]+)', row.description)

            quantity = Decimal(matcher.mo.group(2))
            isbuy = matcher.mo.group(1) == 'BOT'
            symbol = matcher.mo.group(3)
            price_number = Decimal(matcher.mo.group(4))

            account = account_from_name('{}:{}'.format(config['asset_position'].name, symbol))
            price = Amount(price_number, cash_currency)
            position = Position(Lot(symbol, price, None), Decimal(quantity))
            if isbuy:
                posting = Posting(entry, account, position, None, None)
            else:
                position = Position(Lot(symbol, price, None), Decimal(quantity))
                posting = Posting(entry, account, position, price, None)
            entry.postings.append(posting)

            if row.commissions:
                create_simple_posting(entry, config['commission'], -to_decimal(row.commissions), cash_currency)
                amount += Decimal(row.commissions)

            if row.fees:
                create_simple_posting(entry, config['fees'], -to_decimal(row.fees), cash_currency)
                amount += Decimal(row.fees)

            create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
            if not isbuy:
                create_simple_posting(entry, config['pnl'], None, None)

        elif row.type == 'ADJ':
            if row.description == 'Account Opt In':

                # If this is the first year, an opt-in probably requires an adjustment.
                entry = Pad(fileloc, date, config['asset_cash'], config['adjustment'])
                new_entries.append(entry)

                # And an associated check.
                new_entries.append(Check(fileloc, date, config['asset_cash'], balance, None))

                continue # No entry.

            elif row.description == 'Courtesy Credit':
                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['dividend_nontax'], -amount, cash_currency)

        else:
            raise ValueError("Unknown transaction {}".format(row))

        new_entries.append(entry)
        prev_balance = balance

    return new_entries


def convert_number(string):
    if string == '--':
        return Decimal()
    mo = re.match(r'\((.*)\)', string)
    if mo:
        sign = -1
        string = mo.group(1)
    else:
        sign = 1

    number = Decimal(re.sub('[\$,]', '', string)) if string != '--' else Decimal()
    return number * sign


debug = False
