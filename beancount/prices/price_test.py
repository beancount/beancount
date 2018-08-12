"""Tests for main driver for price fetching.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
import shutil
import tempfile
import os
from os import path
from unittest import mock

from dateutil import tz

from beancount.prices.source import SourcePrice
from beancount.prices import price
from beancount.prices import find_prices
from beancount.prices.sources import yahoo
from beancount.core.number import D
from beancount.utils import test_utils
from beancount.parser import cmptest
from beancount import loader


class TestSetupCache(unittest.TestCase):

    def test_clear_cache_unset(self):
        with mock.patch('os.remove') as mock_remove:
            with tempfile.TemporaryDirectory():
                price.setup_cache(None, True)
        self.assertEqual(0, mock_remove.call_count)

    def test_clear_cache_not_present(self):
        with mock.patch('os.remove') as mock_remove:
            with tempfile.TemporaryDirectory() as tmpdir:
                filename = path.join(tmpdir, 'cache.db')
                price.setup_cache(filename, True)
                self.assertEqual(0, mock_remove.call_count)
                self.assertTrue(any(dirfile.startswith('cache.db')
                                    for dirfile in os.listdir(tmpdir)))
                price.reset_cache()

    def test_clear_cache_present(self):
        mock_remove = mock.MagicMock()
        real_remove = os.remove
        def remove(*args):
            mock_remove(*args)
            real_remove(*args)
        with mock.patch('os.remove', remove):
            with tempfile.TemporaryDirectory() as tmpdir:
                filename = path.join(tmpdir, 'cache.db')
                with open(filename, 'w'): pass
                price.setup_cache(filename, True)
                self.assertEqual(1, mock_remove.call_count)
                self.assertTrue(any(dirfile.startswith('cache.db')
                                    for dirfile in os.listdir(tmpdir)))
                price.reset_cache()

    def test_leave_cache(self):
        with mock.patch('os.remove') as mock_remove:
            with tempfile.TemporaryDirectory() as tmpdir:
                filename = path.join(tmpdir, 'cache.db')
                with open(filename, 'w'): pass
                price.setup_cache(None, False)
                self.assertEqual(0, mock_remove.call_count)
                self.assertTrue(path.exists(filename))


class TestCache(unittest.TestCase):

    def test_fetch_cached_price__disabled(self):
        # Latest.
        with mock.patch('beancount.prices.price._CACHE', None):
            self.assertIsNone(price._CACHE)
            source = mock.MagicMock()
            price.fetch_cached_price(source, 'HOOL', None)
            self.assertTrue(source.get_latest_price.called)

        # Historical.
        with mock.patch('beancount.prices.price._CACHE', None):
            self.assertIsNone(price._CACHE)
            source = mock.MagicMock()
            price.fetch_cached_price(source, 'HOOL', datetime.date.today())
            self.assertTrue(source.get_historical_price.called)

    def test_fetch_cached_price__latest(self):
        tmpdir = tempfile.mkdtemp()
        tmpfile = path.join(tmpdir, 'prices.cache')
        try:
            price.setup_cache(tmpfile, False)

            srcprice = SourcePrice(D('1.723'), datetime.datetime.now(tz.tzutc()), 'USD')
            source = mock.MagicMock()
            source.get_latest_price.return_value = srcprice
            source.__file__ = '<module>'

            # Cache miss.
            result = price.fetch_cached_price(source, 'HOOL', None)
            self.assertTrue(source.get_latest_price.called)
            self.assertEqual(1, len(price._CACHE))
            self.assertEqual(srcprice, result)

            source.get_latest_price.reset_mock()

            # Cache hit.
            result = price.fetch_cached_price(source, 'HOOL', None)
            self.assertFalse(source.get_latest_price.called)
            self.assertEqual(1, len(price._CACHE))
            self.assertEqual(srcprice, result)

            srcprice2 = SourcePrice(
                D('1.894'), datetime.datetime.now(tz.tzutc()), 'USD')
            source.get_latest_price.reset_mock()
            source.get_latest_price.return_value = srcprice2

            # Cache expired.
            time_beyond = datetime.datetime.now() + price._CACHE.expiration * 2
            with mock.patch('beancount.prices.price.now', return_value=time_beyond):
                result = price.fetch_cached_price(source, 'HOOL', None)
                self.assertTrue(source.get_latest_price.called)
                self.assertEqual(1, len(price._CACHE))
                self.assertEqual(srcprice2, result)
        finally:
            price.reset_cache()
            if path.exists(tmpdir):
                shutil.rmtree(tmpdir)

    def test_fetch_cached_price__historical(self):
        tmpdir = tempfile.mkdtemp()
        tmpfile = path.join(tmpdir, 'prices.cache')
        try:
            price.setup_cache(tmpfile, False)

            srcprice = SourcePrice(
                D('1.723'), datetime.datetime.now(tz.tzutc()), 'USD')
            source = mock.MagicMock()
            source.get_historical_price.return_value = srcprice
            source.__file__ = '<module>'

            # Cache miss.
            day = datetime.date(2006, 1, 2)
            result = price.fetch_cached_price(source, 'HOOL', day)
            self.assertTrue(source.get_historical_price.called)
            self.assertEqual(1, len(price._CACHE))
            self.assertEqual(srcprice, result)

            source.get_historical_price.reset_mock()

            # Cache hit.
            result = price.fetch_cached_price(source, 'HOOL', day)
            self.assertFalse(source.get_historical_price.called)
            self.assertEqual(1, len(price._CACHE))
            self.assertEqual(srcprice, result)
        finally:
            price.reset_cache()
            if path.exists(tmpdir):
                shutil.rmtree(tmpdir)


class TestProcessArguments(unittest.TestCase):

    def test_filename_not_exists(self):
        with test_utils.capture('stderr'):
            with self.assertRaises(SystemExit):
                args, jobs, _ = test_utils.run_with_args(
                    price.process_args, ['--no-cache', '/some/file.beancount'])

    @test_utils.docfile
    def test_explicit_file__badcontents(self, filename):
        """
        2015-01-01 open Assets:Invest
        2015-01-01 open USD ;; Error
        """
        with test_utils.capture('stderr'):
            args, jobs, _, __ = test_utils.run_with_args(
                price.process_args, ['--no-cache', filename])
            self.assertEqual([], jobs)

    def test_filename_exists(self):
        with tempfile.NamedTemporaryFile('w') as tmpfile:
            with test_utils.capture('stderr'):
                args, jobs, _, __ = test_utils.run_with_args(
                    price.process_args, ['--no-cache', tmpfile.name])
                self.assertEqual([], jobs)  # Empty file.

    def test_expressions(self):
        with test_utils.capture('stderr'):
            args, jobs, _, __ = test_utils.run_with_args(
                price.process_args, ['--no-cache', '-e', 'USD:yahoo/AAPL'])
            self.assertEqual(
                [find_prices.DatedPrice(
                    'AAPL', 'USD', None,
                    [find_prices.PriceSource(yahoo, 'AAPL', False)])], jobs)


class TestClobber(cmptest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
          ;; Existing file.
          2015-01-05 price HDV                                 75.56 USD
          2015-01-23 price HDV                                 77.34 USD
          2015-02-06 price HDV                                 77.16 USD
          2015-02-12 price HDV                                 78.17 USD
          2015-05-01 price HDV                                 77.48 USD
          2015-06-02 price HDV                                 76.33 USD
          2015-06-29 price HDV                                 73.74 USD
          2015-07-06 price HDV                                 73.79 USD
          2015-08-11 price HDV                                 74.19 USD
          2015-09-04 price HDV                                 68.98 USD
        """
        self.entries = entries

        # New entries.
        self.price_entries, _, __ = loader.load_string("""
          2015-01-27 price HDV                                 76.83 USD
          2015-02-06 price HDV                                 77.16 USD
          2015-02-19 price HDV                                  77.5 USD
          2015-06-02 price HDV                                 76.33 USD
          2015-06-19 price HDV                                    76 USD
          2015-07-06 price HDV                                 73.79 USD
          2015-07-31 price HDV                                 74.64 USD
          2015-08-11 price HDV                                 74.20 USD ;; Different
        """, dedent=True)

    def test_clobber_nodiffs(self):
        new_price_entries, _ = price.filter_redundant_prices(self.price_entries,
                                                             self.entries,
                                                             diffs=False)
        self.assertEqualEntries("""
          2015-01-27 price HDV                                 76.83 USD
          2015-02-19 price HDV                                  77.5 USD
          2015-06-19 price HDV                                    76 USD
          2015-07-31 price HDV                                 74.64 USD
        """, new_price_entries)

    def test_clobber_diffs(self):
        new_price_entries, _ = price.filter_redundant_prices(self.price_entries,
                                                             self.entries,
                                                             diffs=True)
        self.assertEqualEntries("""
          2015-01-27 price HDV                                 76.83 USD
          2015-02-19 price HDV                                  77.5 USD
          2015-06-19 price HDV                                    76 USD
          2015-07-31 price HDV                                 74.64 USD
          2015-08-11 price HDV                                 74.20 USD ;; Different
        """, new_price_entries)


class TestTimezone(unittest.TestCase):

    @mock.patch.object(price, 'fetch_cached_price')
    def test_fetch_price__naive_time_no_timeozne(self, fetch_cached):
        fetch_cached.return_value = SourcePrice(
            D('125.00'), datetime.datetime(2015, 11, 22, 16, 0, 0), 'JPY')
        dprice = find_prices.DatedPrice('JPY', 'USD', datetime.date(2015, 11, 22), None)
        with self.assertRaises(ValueError):
            price.fetch_price(dprice._replace(sources=[
                find_prices.PriceSource(yahoo, 'USDJPY', False)]), False)


class TestInverted(unittest.TestCase):

    def setUp(self):
        fetch_cached = mock.patch('beancount.prices.price.fetch_cached_price').start()
        fetch_cached.return_value = SourcePrice(
            D('125.00'), datetime.datetime(2015, 11, 22, 16, 0, 0, tzinfo=tz.tzlocal()),
            'JPY')
        self.dprice = find_prices.DatedPrice('JPY', 'USD', datetime.date(2015, 11, 22),
                                             None)
        self.addCleanup(mock.patch.stopall)

    def test_fetch_price__normal(self):
        entry = price.fetch_price(self.dprice._replace(sources=[
            find_prices.PriceSource(yahoo, 'USDJPY', False)]), False)
        self.assertEqual(('JPY', 'USD'), (entry.currency, entry.amount.currency))
        self.assertEqual(D('125.00'), entry.amount.number)

    def test_fetch_price__inverted(self):
        entry = price.fetch_price(self.dprice._replace(sources=[
            find_prices.PriceSource(yahoo, 'USDJPY', True)]), False)
        self.assertEqual(('JPY', 'USD'), (entry.currency, entry.amount.currency))
        self.assertEqual(D('0.008'), entry.amount.number)

    def test_fetch_price__swapped(self):
        entry = price.fetch_price(self.dprice._replace(sources=[
            find_prices.PriceSource(yahoo, 'USDJPY', True)]), True)
        self.assertEqual(('USD', 'JPY'), (entry.currency, entry.amount.currency))
        self.assertEqual(D('125.00'), entry.amount.number)
