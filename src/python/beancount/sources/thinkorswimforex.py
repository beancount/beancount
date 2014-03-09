"""Think-or-Swim platform transaction detail importer.

This code parses the file that can be downloaded from the Think-or-Swim
application from the Activity page.
"""
import csv
import re
import itertools
import datetime

from beancount.imports import importer
from beancount.core import data
from beancount.core.amount import Decimal, ZERO
from beancount.core.data import create_simple_posting
from beancount.core.data import Transaction
from beancount.utils import csv_utils
from beancount.utils.text_utils import Matcher
from beancount.core import flags
from beancount.sources.thinkorswim import convert_number


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'               : 'Account for filing',
        'cash_currency'      : 'Currency used for cash account',
        'asset_cash'         : 'Cash account',
        #'asset_position'     : 'Root account for all position sub-accounts',
        'fees'               : 'Fees',
        'commission'         : 'Commissions',
        'interest'           : 'Interest income',
        'pnl'                : 'Capital Gains/Losses',
        'transfer'           : 'Other account for inter-bank transfers',
    }

    def import_file(self, filename):
        """Import a CSV file from Think-or-Swim."""

        config = self.get_config()
        sections = csv_utils.csv_split_sections_with_titles(csv.reader(open(filename)))
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

        return process_forex(sections['Forex Statements'], filename, config)


def process_forex(section, filename, config):
    """Process the FOREX subaccount entries."""
    new_entries = []
    cash_currency = config['cash_currency']

    irows = iter(section)
    prev_balance = Decimal()
    running_balance = Decimal()
    prev_date = datetime.date(1970, 1, 1)
    Tuple = csv_utils.csv_parse_header(next(irows))
    matcher = Matcher()
    for index, row in enumerate(itertools.starmap(Tuple, irows)):

        # For transfers, they don't put the date in (WTF?) so use the previous
        # day's date, because the rows are otherwise correctly sorted. Then
        # parse the date.
        if not row.date:
            date = prev_date
        else:
            date = datetime.datetime.strptime(row.date, '%d/%m/%y').date()

        balance = convert_number(row.balance)

        row_amount = convert_number(row.amount_usd)
        running_balance += row_amount

        ##print('RUNNING_BALANCE', running_balance, balance)
        assert(abs(running_balance - balance) <= Decimal('0.01'))

        amount = balance - prev_balance
        assert(abs(row_amount - amount) <= Decimal('0.01'))

        # Check some invariants.
        assert row.commissions == '--'

        if row.type not in ('BAL',):

            # Create a new transaction.
            narration = re.sub('[ ]+', ' ', "({0.type}) {0.description}".format(row).replace('\n', ' ')).strip()
            fileloc = data.FileLocation(filename, index)
            links = set([row.ref] if row.ref != '--' else [])
            entry = Transaction(fileloc, date, flags.FLAG_IMPORT, None, narration, None, links, [])

            if row.type in ('FND', 'WDR'):
                create_simple_posting(entry, config['transfer'], amount, cash_currency)
                create_simple_posting(entry, config['asset_cash'], -amount, cash_currency)

            elif row.type == 'TRD':
                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['pnl'], -amount, cash_currency)

            elif row.type == 'ROLL':
                if amount != ZERO:
                    create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                    create_simple_posting(entry, config['pnl'], -amount, cash_currency)

            if entry.postings:
                new_entries.append(entry)

        prev_date = date
        prev_balance = balance

    return new_entries


debug = False
