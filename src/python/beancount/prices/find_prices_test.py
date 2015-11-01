"""Tests for price finding routines.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import types
import unittest

from beancount.prices import find_prices


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
            find_prices.parse_source('AAPL')
        with self.assertRaises(ValueError):
            find_prices.parse_source('***//--')

        # The module does not get imported at this stage.
        find_prices.parse_source('invalid.module.name/NASDAQ:AAPL')

    def test_source_valid(self):
        job = find_prices.parse_source('google/NASDAQ:AAPL')
        self.assertEqual(
            find_prices.Job('google', 'NASDAQ:AAPL',
                            None, False, None, None), job)

        job = find_prices.parse_source('beancount.prices.sources.yahoo/AAPL')
        self.assertEqual(
            find_prices.Job('beancount.prices.sources.yahoo', 'AAPL',
                            None, False, None, None), job)
