"""
Tests for Vanguard importer.
"""
from os import path
import unittest
from beancount.sources import vanguard


class TestImporterVanguard(unittest.TestCase):

    TICKERS = {
        'American Funds The Growth Fund of America R6': 'RGAGX',
        'AF Grwth Fd of America R6': 'RGAGX',
        'Vanguard Total Bond Market Index Fund Institutional Plus Shares': 'VBMPX',
        'Total Bond Mkt Ix Ist Pls': 'VBMPX',
        }

    CONFIG = {
        'FILE'         : 'Assets:US:Vanguard',
        'asset_cash'   : 'Assets:US:Vanguard:Cash',
        'asset_pretax' : 'Assets:US:Vanguard:PreTax',
        'asset_match'  : 'Assets:US:Vanguard:Match',
        'income_match' : 'Income:US:Google:Match401k',
        'dividend'     : 'Income:US:Vanguard:Dividend',
        'fees'         : 'Expenses:US:Vanguard:Fee',
        'tickers'      : TICKERS,
        }

    def setUp(self):
        self.importer = vanguard.Importer(self.CONFIG)

    def test_base(self):
        filename = path.join(path.dirname(__file__), 'test_vanguard.csv')
        a = self.importer.import_file(filename)
