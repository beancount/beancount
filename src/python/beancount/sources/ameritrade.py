"""TD Ameritrade investment account CSV file importer.

This module parses the file that can be downloaded from the Activity &
Statements tab on their website, NOT the file that can be downloaded from
Think-or-Swim. For a more accurate import, you ought to use the Think-or-Swim
importer (only provided if you opted-in to TOS).
"""
import re
import datetime

from beancount.imports import importer
from beancount.core import data
from beancount.core.amount import Decimal, Amount
from beancount.core.data import create_simple_posting
from beancount.core.data import Posting, Transaction, Check
from beancount.core.account import account_from_name
from beancount.core.position import Lot, Position
from beancount.core.account import accountify_dict
from beancount.utils import csv_utils
from beancount.core import flags


debug = False


ALWAYS_EMPTY_FIELDS = ('reg_fee',
                       'short_term_rdm_fee',
                       'fund_redemption_fee',
                       'deferred_sales_charge')


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
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
    }


    def import_file(self, filename):
        """Import a CSV file from Ameritrade."""

        config = self.get_accountified_config()
        new_entries = []

        cash_currency = config['cash_currency']

        # Iterate over the groups of entries, which will form transactions.
        prev_balance = Amount(Decimal(), cash_currency)
        prev_date = datetime.date(1970, 1, 1)
        rdr = csv_utils.csv_tuple_reader(open(filename))
        for index, row in enumerate(rdr):

            # Check that the fields we're not dealing with are empty.
            assert all(not getattr(row, fieldname) for fieldname in ALWAYS_EMPTY_FIELDS)

            # Get the new entry's date.
            date = datetime.datetime.strptime(row.date, '%m/%d/%Y').date()

            # Insert some Check entries every time the day changed.
            if ((debug and date != prev_date) or
                (not debug and date.month != prev_date.month)):

                prev_date = date
                fileloc = data.FileLocation(filename, index)
                new_entries.append(Check(fileloc, date, config['asset_cash'], prev_balance, None))

            # Create a new transaction.
            narration = row.description
            links = set([row.transaction_id])
            fileloc = data.FileLocation(filename, index)

            entry = Transaction(fileloc, date, flags.FLAG_IMPORT, None, narration, None, links, [])

            amount = Decimal(row.amount)

            # Process the transaction, each code specialized for its specific case.
            if row.description in (
                    'CLIENT REQUESTED ELECTRONIC FUNDING RECEIPT (FUNDS NOW)',
                    'CLIENT REQUESTED ELECTRONIC FUNDING DISBURSEMENT (FUNDS NOW)'):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['transfer'], -amount, cash_currency)

            elif row.description in (
                    'MONEY MARKET INTEREST (MMDA1)',):

                create_simple_posting(entry, config['asset_money_market'], amount, cash_currency)
                create_simple_posting(entry, config['interest'], -amount, cash_currency)

            elif row.description in (
                    'MONEY MARKET PURCHASE',
                    'MONEY MARKET REDEMPTION'):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['asset_money_market'], -amount, cash_currency)

            elif row.description in (
                    'MONEY MARKET PURCHASE (MMDA1)',
                    'MONEY MARKET REDEMPTION (MMDA1)',
                    'MONEY MARKET PURCHASE (MMDA10)',
                    'MONEY MARKET REDEMPTION (MMDA10)',):

                # Ignore these, they are complementary to the other side of the
                # transaction and the amounts are rounded anyhow! This is so wrong.
                continue

            elif row.description in (
                    'PAPER STATEMENT FEE',
                    'WIRE CHARGE (FEE)',):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['fees'], -amount, cash_currency)

            elif row.description in (
                    'WIRE OUTGOING (ACD WIRE DISBURSEMENTS)',
                    'THIRD PARTY',
                    'CASH RECEIPTS THIRD PARTY',):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['third_party'], -amount, cash_currency)

            elif row.description in (
                    'TRANSFER TO FOREX ACCOUNT',
                    'TRANSFER FROM FOREX ACCOUNT',):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['asset_forex'], -amount, cash_currency)

            elif row.description in (
                    'OFF-CYCLE INTEREST (MMDA1)',
                    'FREE BALANCE INTEREST ADJUSTMENT'):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['interest'], -amount, cash_currency)

            elif re.match('Bought ([^ ]+) ([^ ]+) @ ([^ ]+)', row.description):

                account = account_from_name('{}:{}'.format(config['asset_position'].name, row.symbol))
                cost = Amount(row.price, cash_currency)
                position = Position(Lot(row.symbol, cost, None), Decimal(row.quantity))
                posting = Posting(entry, account, position, None, None)
                entry.postings.append(posting)

                if row.commission:
                    create_simple_posting(entry, config['commission'], row.commission, cash_currency)

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)

            elif re.match(r'ORDINARY DIVIDEND \((.*)\)', row.description):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['dividend'], -amount, cash_currency)

            elif re.match(r'NON-TAXABLE DIVIDENDS \((.*)\)', row.description):

                create_simple_posting(entry, config['asset_cash'], amount, cash_currency)
                create_simple_posting(entry, config['dividend_nontax'], -amount, cash_currency)

            else:
                raise ValueError("Unknown transaction {}".format(row))

            new_entries.append(entry)

            prev_balance = Amount(Decimal(row.net_cash_balance), cash_currency)

        fileloc = data.FileLocation(filename, index)
        new_entries.append(Check(fileloc,
                                 date + datetime.timedelta(days=1),
                                 config['asset_cash'], prev_balance, None))

        return new_entries
