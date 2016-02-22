__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest
from os import path

from beancount.ingest import importer
from beancount.ingest import cache
from beancount.ingest.importers import compat


class _GoldImporter(compat.Importer):

    REQUIRED_CONFIG = {
        'FILE'         : 'Account for filing',
        'asset_cash'   : 'Cash account',
        'asset_shares' : 'Positions',
        'dividend'     : 'Dividends',
        'fees'         : 'Fees',
    }

    def extract(self, _):
        return ['NUGGET']

    def file_date(self, _):
        return datetime.date(2014, 4, 2)


class TestImporter(unittest.TestCase):

    def setUp(self):
        self.config = {
            'FILE'         : 'Asset:Gold',
            'asset_cash'   : 'Asset:Gold:Cash',
            'asset_shares' : 'Asset:Gold:Ingots',
            'dividend'     : 'Income:Dividends',
            'fees'         : 'Expenses:Fees',
            }
        self.gold = _GoldImporter(self.config, content_regexps='.*')

    def test_constructor(self):
        # Check with a valid config.
        gold = _GoldImporter(self.config, content_regexps='.*')

        # Check with some missing elements.
        config_missing = self.config.copy()
        del config_missing['asset_cash']
        with self.assertRaises(ValueError):
            gold = _GoldImporter(config_missing, content_regexps='.*')

        # Check with some invalid/extra elements.
        config_extra = self.config.copy()
        config_extra['asset_interest'] = 'Income:Dividends'
        with self.assertRaises(ValueError):
            gold = _GoldImporter(config_extra, content_regexps='.*')

    def test_get_config(self):
        self.assertEqual(self.config, self.gold.get_config())

    def test_extract(self):
        self.assertEqual(['NUGGET'], self.gold.extract(None))

    def test_file_account(self):
        self.assertEqual('Asset:Gold', self.gold.file_account(None))

    def test_file_name(self):
        filename = __file__
        file = cache.FileMemo(filename)
        self.assertEqual(None, self.gold.file_name(file))

    def test_file_date(self):
        file = cache.FileMemo('/tmp/whatever')
        self.assertEqual(datetime.date(2014, 4, 2), self.gold.file_date(file))


# This needs to be test more thoroughly.
__incomplete__ = True
