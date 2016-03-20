"""Example importer for example broker UTrade.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import csv
import datetime
import re
import pprint
import logging
from os import path

from dateutil.parser import parse

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core.number import MISSING
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
                 account_gains,
                 account_fees,
                 account_external):
        self.currency = currency
        self.account_root = account_root
        self.account_cash = account_cash
        self.account_dividends = account_dividends
        self.account_gains = account_gains
        self.account_fees = account_fees
        self.account_external = account_external

    def identify(self, file):
        # Match if the filename is as downloaded and the header has the unique
        # fields combination we're looking for.
        return (re.match(r"UTrade\d\d\d\d\d\d\d\d\.csv", path.basename(file.name)) and
                re.match("DATE,TYPE,REF", file.head()))

    def file_name(self, file):
        return 'utrade.{}'.format(path.basename(file.name))

    def file_account(self, _):
        return self.account_root

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
            other = amount.add(units, fees)

            if rtype == 'XFER':
                assert fees.number == ZERO
                txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                    data.Posting(self.account_cash, units, None, None, None, None),
                    data.Posting(self.account_external, -other, None, None, None, None),
                    ])

            elif rtype == 'DIV':
                assert fees.number == ZERO

                # Extract the instrument name from its description.
                match = re.search(r'~([A-Z]+)$', row['DESCRIPTION'])
                if not match:
                    logging.error("Missing instrument name in '%s'", row['DESCRIPTION'])
                    continue
                instrument = match.group(1)
                account_dividends = self.account_dividends.format(instrument)

                txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                    data.Posting(self.account_cash, units, None, None, None, None),
                    data.Posting(account_dividends, -other, None, None, None, None),
                    ])

            elif rtype in ('BUY', 'SELL'):

                # Extract the instrument name, number of units, and price from
                # the description. That's just what we're provided with (this is
                # actually realistic of some data from some institutions, you
                # have to figure out a way in your parser).
                match = re.search(r'\+([A-Z]+)\b +([0-9.]+)\b +@([0-9.]+)',
                                  row['DESCRIPTION'])
                if not match:
                    logging.error("Missing purchase infos in '%s'", row['DESCRIPTION'])
                    continue
                instrument = match.group(1)
                account_inst = account.join(self.account_root, instrument)
                units_inst = amount.Amount(D(match.group(2)), instrument)
                rate = D(match.group(3))

                if rtype == 'BUY':
                    cost = position.Cost(rate, self.currency, None, None)
                    txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                        data.Posting(self.account_cash, units, None, None, None, None),
                        data.Posting(self.account_fees, fees, None, None, None, None),
                        data.Posting(account_inst, units_inst, cost, None, None, None),
                        ])

                elif rtype == 'SELL':
                    # Extract the lot. In practice this information not be there
                    # and you will have to identify the lots manually by editing
                    # the resulting output. You can leave the cost.number slot
                    # set to None if you like.
                    match = re.search(r'\(LOT ([0-9.]+)\)', row['DESCRIPTION'])
                    if not match:
                        logging.error("Missing cost basis in '%s'", row['DESCRIPTION'])
                        continue
                    cost_number = D(match.group(1))
                    cost = position.Cost(cost_number, self.currency, None, None)
                    price = amount.Amount(rate, self.currency)
                    account_gains = self.account_gains.format(instrument)
                    txn = data.Transaction(meta, date, self.FLAG, None, desc, None, {link}, [
                        data.Posting(self.account_cash, units, None, None, None, None),
                        data.Posting(self.account_fees, fees, None, None, None, None),
                        data.Posting(account_inst, units_inst, cost, price, None, None),
                        data.Posting(account_gains, None, None, None, None, None),
                        ])

            else:
                logging.error("Unknown row type: %s; skipping", rtype)
                continue

            entries.append(txn)

        # Insert a final balance check.
        if index:
            entries.append(
                data.Balance(meta, date + datetime.timedelta(days=1),
                             self.account_cash,
                             amount.Amount(D(row['BALANCE']), self.currency),
                             None, None))

        return entries


def test():
    # Create an importer instance for running the regression tests.
    importer = Importer("USD",
                        "Assets:US:UTrade",
                        "Assets:US:UTrade:Cash",
                        "Income:US:UTrade:{}:Dividend",
                        "Income:US:UTrade:{}:Gains",
                        "Expenses:Financial:Fees",
                        "Assets:US:BofA:Checking")
    yield from regression.compare_sample_files(importer, __file__)
