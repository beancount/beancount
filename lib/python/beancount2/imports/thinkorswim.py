"""Think-or-Swim platform transaction detail importer.

This code parses the file that can be downloaded from the Think-or-Swim
application from the Activity page.
"""
import csv
import re
import itertools
import datetime

from beancount2.core import data
from beancount2.core.data import account_from_name
from beancount2.core.data import create_simple_posting
from beancount2.core.data import Transaction, Posting, Pad, Decimal, Amount, Check
from beancount2.core.inventory import Position, Lot
from beancount2 import utils


debug = False


# This importer has been tested wtih the following sources.
SOURCES = [
    ('Think-or-Swim' , 'US'),
    ]




def import_file(filename, config, entries):
    """Import a CSV file from Think-or-Swim."""

    new_entries = []
    annotations = {}

    # Find out which is the base currency.
    base_currency = data.get_currency_for_account(config['asset_cash'], entries)

    sections = utils.csv_split_sections(csv.reader(open(filename)))
    if 0:
        for section_name, rows in sections.items():
            if re.search(r'\bSummary\b', section_name):
                continue
            print('============================================================', section_name)
            if not rows:
                continue
            irows = iter(rows)
            Tuple = utils.csv_parse_header(next(irows))
            for row in irows:
                obj = Tuple(*row)
                print(obj)

    # Process the cash balance report.
    irows = iter(sections['Cash Balance'])
    prev_balance = Amount(Decimal(), base_currency)
    prev_date = datetime.date(1970, 1, 1)
    Tuple = utils.csv_parse_header(next(irows))
    matcher = utils.Matcher()
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
        narration = row.description
        links = set([row.ref])
        entry = Transaction(fileloc, date, data.FLAG_IMPORT, None, narration, None, links, [])

        amount = Decimal(row.amount.replace(',', ''))
        assert not row.fees, row

        balance = Amount(Decimal(row.balance.replace(',', '')), base_currency)

        if row.description == 'CLIENT REQUESTED ELECTRONIC FUNDING RECEIPT (FUNDS NOW)':
            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
            create_simple_posting(entry, config['transfer'], -amount, base_currency)

        elif re.match('MONEY MARKET INTEREST', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
            create_simple_posting(entry, config['interest'], -amount, base_currency)

        elif re.match('TRANSFER (TO|FROM) FOREX ACCOUNT', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
            create_simple_posting(entry, config['asset_forex'], -amount, base_currency)

        elif re.match('NON-TAXABLE DIVIDENDS', row.description):
            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
            create_simple_posting(entry, config['dividend_nontax'], -amount, base_currency)

        elif row.description == 'THIRD PARTY':
            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
            create_simple_posting(entry, config['third_party'], -amount, base_currency)

        elif matcher.match(r'WEB:WEB_UNIF(?:_SNAP)? BOT ([+\-0-9]+) (.+) @([0-9\.]+)', row.description):
            quantity = Decimal(matcher.mo.group(1))
            symbol = matcher.mo.group(2)
            price = Decimal(matcher.mo.group(3))

            account = account_from_name('{}:{}'.format(config['asset_position'].name, symbol))
            cost = Amount(price, base_currency)
            position = Position(Lot(symbol, cost, None), Decimal(quantity))
            posting = Posting(entry, account, position, None, None)
            entry.postings.append(posting)

            if row.commissions:
                create_simple_posting(entry, config['commission'], row.commissions, base_currency)
                amount += Decimal(row.commissions)

            create_simple_posting(entry, config['asset_cash'], amount, base_currency)

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

    return new_entries, annotations
