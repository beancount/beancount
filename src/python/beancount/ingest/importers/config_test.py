__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.ingest import importer
from beancount.ingest.importers import config


class SimpleTestImporter(config.ConfigImporterMixin,
                         importer.ImporterProtocol):
    REQUIRED_CONFIG = {
        'FILE'         : 'Account for filing',
        'asset_cash'   : 'Cash account',
        'asset_shares' : 'Positions',
        'dividend'     : 'Dividends',
        'fees'         : 'Fees',
    }


class TestConfigMixin(unittest.TestCase):

    def setUp(self):
        self.default_config = {key: 'Assets:Something'
                               for key in SimpleTestImporter.REQUIRED_CONFIG}

    def test_constructors(self):
        # Test invalid input type.
        with self.assertRaises(AssertionError):
            SimpleTestImporter('*')

        # Test a succeeding case.
        SimpleTestImporter(self.default_config)

    def test_file_account(self):
        config = self.default_config.copy()
        config['FILE'] = 'Assets:FilingAccount'
        importer = SimpleTestImporter(config)
        self.assertEqual('Assets:FilingAccount', importer.file_account(None))

    def test_invalid_missing(self):
        config = self.default_config.copy()
        del config['asset_cash']
        with self.assertRaises(ValueError):
            SimpleTestImporter(config)

    def test_invalid_extra(self):
        config = self.default_config.copy()
        config['asset_other'] = 'Assets:Other'
        with self.assertRaises(ValueError):
            SimpleTestImporter(config)
