"""
Interpret the Google AdSense CSV file and output transactions in a format
suitable for Ledger.
"""
import datetime

from beancount.imports import importer
from beancount.core import data
from beancount.core.amount import to_decimal, Amount
from beancount.core.data import Transaction, Balance
from beancount.utils import DateIntervalTicker
from beancount.utils import csv_utils
from beancount.core import flags


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'          : 'Account for filing',
        'cash_currency' : 'USD',
        'cash'          : 'Main account holding the funds',
        'income'        : 'Income account',
        'transfer'      : 'Default account where money gets transferred to',
    }

    def import_file(self, filename):
        """Import a Google AdSense file."""

        config = self.get_config()
        new_entries = []

        currency = config['cash_currency']
        payee = "Google AdSense"

        ticker = DateIntervalTicker(
            lambda date: ((date.year * 12 + (date.month - 1)) // 3))
        prev_row = None

        f = open(filename, "r", encoding='utf-16')
        for index, row in enumerate(csv_utils.csv_tuple_reader(f, delimiter='\t')):

            # Convert the datatypes.
            row = row._replace(
                date = datetime.datetime.strptime(row.date, '%m/%d/%y').date(),
                amount = to_decimal(row.amount),
                account_balance = to_decimal(row.account_balance))

            fileloc = data.FileLocation(filename, index)

            # Insert some Balance entries every 3 months or so.
            n3mths = (row.date.year * 12 + row.date.month) // 3

            if ticker(row.date):
                if prev_row:
                    check = Balance(fileloc, row.date, config['cash'],
                                  Amount(prev_row.account_balance, currency), None)
                    new_entries.append(check)
            prev_row = row

            entry = Transaction(fileloc, row.date, flags.FLAG_IMPORT, payee, row.description, None, None, [])

            if row.description == 'Payment issued':
                data.create_simple_posting(entry, config['cash'], row.amount, currency)
                data.create_simple_posting(entry, config['transfer'], -row.amount, currency)

            elif row.description.startswith('Earnings '):
                data.create_simple_posting(entry, config['cash'], row.amount, currency)
                data.create_simple_posting(entry, config['income'], -row.amount, currency)

            elif row.description.startswith('EFT not successful - earnings credited back'):
                data.create_simple_posting(entry, config['cash'], row.amount, currency)
                data.create_simple_posting(entry, config['transfer'], -row.amount, currency)

            else:
                raise ValueError('Unknown row type: {}'.format(row))

            new_entries.append(entry)

        check = Balance(fileloc, row.date + datetime.timedelta(days=1), config['cash'],
                      Amount(row.account_balance, currency), None)
        new_entries.append(check)

        return new_entries
