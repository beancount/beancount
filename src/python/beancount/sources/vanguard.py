"""
Vanguard CSV file format, which is better than their OFX.
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
        'asset_cash'   : 'Cash account that receives the contributions',
        'asset_pretax' : 'Root of positions from pre-tax contributions',
        'asset_match'  : 'Root of positions from matching contributions',
        'income_match' : 'Income from matching contributions',
        'dividend'     : 'Dividends',
        'fees'         : 'Fees',
    }

    def ___import_file(self, filename):
        """Import a Google AdSense file."""

        config = self.get_accountified_config()
        new_entries = []

## TODO(blais)
