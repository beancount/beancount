"""Example importer for example broker UTrade.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import csv
import datetime
import re
import pprint
from os import path

from dateutil.parser import parse

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import account
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.ingest import importer
from beancount.ingest import regression


class Importer(importer.ImporterProtocol):
    """An importer for UTrade CSV files (an example investment bank)."""

    def __init__(self, currency,
                 account_root,
                 account_cash,
                 account_dividends,
                 account_fees,
                 account_external):
        self.currency = currency
        self.account_root = account_root
        self.account_cash = account_cash
        self.account_dividends = account_dividends
        self.account_fees = account_fees
        self.account_external = account_external

    def identify(self, file):
        # Match if the filename is as downloaded and the header has the unique
        # fields combination we're looking for.
        return (re.match(r"UTrade\d\d\d\d\d\d\d\d\.csv", path.basename(file.name)) and
                re.match("DATE,TYPE,REF", file.head()))

    def file_account(self, _):
        return self.account

    def file_date(self, file):
        # Extract the statement date from the filename.
        return datetime.datetime.strptime(path.basename(file.name), 'UTrade%Y%m%d.csv').date()

    def extract(self, file):
        # Open the CSV file and create directives.
        entries = []
        index = 0
        for index, row in enumerate(csv.DictReader(open(file.name))):
            meta = data.new_metadata(file.name, index)
            date = parse(row['DATE']).date()
            rtype = row['TYPE']
            link = "ut{0[REF #]}".format(row)
            desc = "({0[TYPE]}) {0[DESCRIPTION]}".format(row)
            units = amount.Amount(D(row['AMOUNT']), self.currency)
            fees = amount.Amount(D(row['FEES']), self.currency)
            other = amount.amount_add(units, fees)

            if rtype == 'XFER':
                assert fees.number == ZERO
                txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                    data.Posting(self.account_cash, units, None, None, None, None),
                    data.Posting(self.account_external, other, None, None, None, None),
                    ])

            elif rtype == 'DIV':
                assert fees.number == ZERO
                txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                    data.Posting(self.account_cash, units, None, None, None, None),
                    data.Posting(self.account_dividends, other, None, None, None, None),
                    ])

            elif rtype == 'BUY':
                txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                    data.Posting(self.account_cash, -units, None, None, None, None),
                    data.Posting(self.account_fees, fees, None, None, None, None),
                    # FIXME: Continue here.
                    ])

            elif rtype == 'SELL':
                txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                    data.Posting(self.account_cash, units, None, None, None, None),
                    data.Posting(self.account_fees, fees, None, None, None, None),
                    # FIXME: Continue here.
                    ])

            else:
                logging.error("Unknown row type: %s; skipping", rtype)
                continue

            entries.append(txn)

        # Insert a final balance check.
        if index:
            entries.append(
                data.Balance(meta, date, self.account_cash,
                             amount.Amount(D(row['BALANCE']), self.currency),
                             None, None))

        return entries


def test():
    importer = Importer()
    yield from regression.compare_sample_files(importer, __file__)
