"""Tests for price finding routines.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import types
import unittest

from beancount.prices import find_prices
from beancount.prices.sources import google
from beancount.prices.sources import yahoo
from beancount import loader


class TestImportSource(unittest.TestCase):

    def test_import_source_valid(self):
        for name in 'google', 'yahoo':
            module = find_prices.import_source(name)
            self.assertIsInstance(module, types.ModuleType)
        module = find_prices.import_source('beancount.prices.sources.google')
        self.assertIsInstance(module, types.ModuleType)

    def test_import_source_invalid(self):
        with self.assertRaises(ImportError):
            module = find_prices.import_source('non.existing.module')


class TestParseSource(unittest.TestCase):

    def test_source_invalid(self):
        with self.assertRaises(ValueError):
            find_prices.parse_source_string('AAPL')
        with self.assertRaises(ValueError):
            find_prices.parse_source_string('***//--')

        # The module gets imported at this stage.
        with self.assertRaises(ImportError):
            find_prices.parse_source_string('invalid.module.name/NASDAQ:AAPL')

    def test_source_valid(self):
        psource = find_prices.parse_source_string('google/NASDAQ:AAPL')
        self.assertEqual(find_prices.PriceSource(google, 'NASDAQ:AAPL', False),
                         psource)

        psource = find_prices.parse_source_string('beancount.prices.sources.yahoo/AAPL')
        self.assertEqual(
            find_prices.PriceSource(yahoo, 'AAPL', False),
            psource)


class TestFromFileAtDate(unittest.TestCase):


    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
        2000-01-10 open Assets:US:Investments:QQQ
        2000-01-10 open Assets:CA:Investments:XSP
        2000-01-10 open Assets:Cash
        2000-01-10 open Assets:External

        2015-02-06 *
          Assets:Cash                     1505.00 USD
          Assets:External                -1000.00 GBP @ 1.505 USD

        2015-04-13 *
          Assets:US:Investments:QQQ             3 QQQ {107.48 USD}
          Assets:Cash

        2015-05-22 *
          Assets:Cash                     1000.00 USD @ 1.2283 CAD
          Assets:External                -1228.30 CAD

        2015-09-04 *
          Assets:US:Investments:QQQ             2 QQQ {101.16 USD}
          Assets:Cash

        2015-11-07 *
          Assets:CA:Investments:XSP             2 XSP {24.28 CAD}
          Assets:Cash

        """
        self.entries = entries

    def test_currencies_held_at_cost_at_date(self):
        currencies = find_prices.currencies_held_at_cost_at_date(self.entries, None)
        self.assertEqual({('XSP', 'CAD'), ('QQQ', 'USD')}, currencies)

        currencies = find_prices.currencies_held_at_cost_at_date(self.entries,
                                                                 datetime.date(2015, 11, 1))
        self.assertEqual({('QQQ', 'USD')}, currencies)

        currencies = find_prices.currencies_held_at_cost_at_date(self.entries,
                                                                 datetime.date(2015, 3, 1))
        self.assertEqual(set(), currencies)

    def test_currencies_priced_at_date(self):
        currencies = find_prices.currencies_priced_at_date(self.entries, None)
        self.assertEqual({('GBP', 'USD'), ('USD', 'CAD')}, currencies)

        currencies = find_prices.currencies_priced_at_date(self.entries,
                                                           datetime.date(2015, 5, 1))
        self.assertEqual({('GBP', 'USD')}, currencies)

        currencies = find_prices.currencies_priced_at_date(self.entries,
                                                           datetime.date(2015, 1, 15))
        self.assertEqual(set(), currencies)
