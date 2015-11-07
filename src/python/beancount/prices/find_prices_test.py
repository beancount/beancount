"""Tests for price finding routines.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import types
import unittest

from beancount.prices import find_prices
from beancount.prices.sources import google
from beancount.prices.sources import yahoo


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
