"""
Vanguard CSV file format, which is better than their OFX.

NOTE: This is incomplete.
"""
import datetime
import functools
import io

from beancount.imports import importer
from beancount.parser import printer
from beancount.core import data
from beancount.core.amount import to_decimal, Amount
from beancount.core.data import Transaction, Balance
from beancount.utils import DateIntervalTicker
from beancount.utils import file_utils
from beancount.utils import csv_utils
from beancount.core import flags


debugging = False


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'          : 'Account for filing',
        'asset_cash'   : 'Cash account that receives the contributions',
        'asset_pretax' : 'Root of positions from pre-tax contributions',
        'asset_match'  : 'Root of positions from matching contributions',
        'income_match' : 'Income from matching contributions',
        'dividend'     : 'Dividends',
        'fees'         : 'Fees',
        'tickers'      : 'A dict of investment name to symbol ticker',
    }

    def import_file(self, filename):
        config = self.get_accountified_config()
        new_entries = []

        # The input file has two sections; split it.
        positions, transaction_detail = list(map(''.join,
                                                 file_utils.iter_sections(open(filename))))

        # Process the transaction detail.
        transformer = functools.partial(transform,
                                        trade_date=parse_date,
                                        run_date=parse_date,
                                        investment_name=self.get_symbol,
                                        share_price=to_decimal,
                                        transaction_shares=to_decimal,
                                        dollar_amount=to_decimal)
        for index, row in enumerate(csv_utils.csv_tuple_reader(
            io.StringIO(transaction_detail))):
            row = transformer(row)


            fileloc = data.FileLocation(filename, index)
            entry = Transaction(fileloc, row.trade_date, flags.FLAG_IMPORT,
                                None, row.transaction_description,
                                None, None, [])
            if debugging:
                print(printer.format_entry(entry))

            # TODO(blais): Stopped here. I don't need to complete this, because
            # Vanguard does not include the source of the contribution! I want
            # to track the contribution source, so

        # Process the final positions.
        if debugging:
            for row in csv_utils.csv_tuple_reader(io.StringIO(positions)):
                print(row)

    def get_symbol(self, name):
        return self.config['tickers'][name]


def transform(row, **transforms):
    """Transform a namedtuple by the given transforms.

    Args:
      row: A namedtuple instance.
      transforms: A mapping of attribute name to functions to apply to the
        attribute's value.
    Returns:
      A namedtuple with modified attributes.
    """
    new_values = {name: transform(getattr(row, name))
                  for name, transform in transforms.items()}
    return row._replace(**new_values)


def parse_date(date_str):
    """Parse a Vanguard date (m/d/y).

    Args:
      date_str: A string, as it comes in the file.
    Returns:
      A datetime object.
    """
    return datetime.datetime.strptime(date_str, "%m/%d/%Y").date()
