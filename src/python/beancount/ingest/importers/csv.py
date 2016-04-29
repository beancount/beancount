"""CSV importer.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import datetime
import re
from os import path

from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.utils.date_utils import parse_date_liberally
from beancount.core import data
from beancount.ingest import importer
from beancount.ingest import regression
from beancount.ingest.importers import regexp
from beancount.utils import csv_utils
from beancount.parser import printer
from beancount.utils import misc_utils


# The set of interpretable columns.
class Col(misc_utils.Enum):
    DATE = '[DATE]'
    NARRATION = '[NARRATION]'
    AMOUNT = '[AMOUNT]'
    BALANCE = '[BALANCE]'
    TAG = '[TAG]'


class Importer(regexp.RegexpImporterMixin, importer.ImporterProtocol):
    """Importer for Chase credit card accounts."""

    def __init__(self, columns, account, currency, header, institution,
                 extra_regexps=None,
                 debug=False):
        regexp.RegexpImporterMixin.__init__(self, [header] + (extra_regexps or []))
        self.columns = columns
        self.account = account
        self.header = header
        self.institution = institution
        self.currency = currency
        self.debug = debug

    def name(self):
        name = self.name or super().name()
        return '{}: "{}"'.format(super().name(), self.file_account(None))

    def file_account(self, _):
        return self.account

    def file_name(self, file):
        return '{}.{}.csv'.format(self.institution,
                                  path.splitext(path.basename(file.name))[0])

    def file_date(self, file):
        "Get the maximum date from the file."
        return max(parse_date_liberally(getattr(row, self.columns[Col.DATE]))
                   for row in csv_utils.csv_tuple_reader(open(file.name)))

    def extract(self, file):
        entries = []

        # Parse all the transactions.
        for index, row in enumerate(csv_utils.csv_tuple_reader(open(file.name))):
            # If debugging, print out the rows.
            if self.debug:
                print(row)

            # Extract the data we need from the row, based on the configuration.
            date = getattr(row, self.columns[Col.DATE])
            amount = D(getattr(row, self.columns[Col.AMOUNT]))
            narration = getattr(row, self.columns[Col.NARRATION])
            tags = ({getattr(row, self.columns[Col.TAG])}
                    if Col.TAG in self.columns else
                    None)

            # Create a transaction and add it to the list of new entries.
            meta = data.new_metadata(file.name, index)
            date = parse_date_liberally(date)
            units = Amount(amount, self.currency)
            txn = data.Transaction(meta, date, self.FLAG, None, narration, tags, None, [
                data.Posting(self.account, units, None, None, None, None),
            ])
            entries.append(txn)

        # # Parse the final balance.
        # meta = data.new_metadata(file.name, index)
        # date = parse_date_liberally(first_row.posting_date) + datetime.timedelta(days=1)
        # entries.append(
        #     data.Balance(meta, date,
        #                  self.account, amount.Amount(D(row.balance), self.currency),
        #                  None, None))

        return entries


# TODO: Bring in balance support.
# TODO: Add balances every month or week.
# TODO: Test ascending and descending orders.
