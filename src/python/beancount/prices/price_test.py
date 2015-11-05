"""Tests for main driver for price fetching.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import datetime
import unittest
import tempfile
import os
from os import path
from unittest import mock
from urllib import request
from urllib import error

from beancount.prices import price
from beancount.prices import find_prices
from beancount.core.number import D, Decimal
from beancount.utils import test_utils


class TestSetupCache(unittest.TestCase):

    def test_clear_cache_unset(self):
        with mock.patch('os.remove') as mock_remove:
            with tempfile.TemporaryDirectory() as tmpdir:
                price.setup_cache(None, True)
        self.assertEqual(0, mock_remove.call_count)

    def test_clear_cache_not_present(self):
        with mock.patch('os.remove') as mock_remove:
            with tempfile.TemporaryDirectory() as tmpdir:
                filename = path.join(tmpdir, 'cache.db')
                price.setup_cache(filename, True)
                self.assertEqual(0, mock_remove.call_count)
                self.assertTrue(path.exists(filename))

    def test_clear_cache_present(self):
        mock_remove = mock.MagicMock()
        real_remove = os.remove
        def remove(*args):
            mock_remove(*args)
            real_remove(*args)
        with mock.patch('os.remove', remove):
            with tempfile.TemporaryDirectory() as tmpdir:
                filename = path.join(tmpdir, 'cache.db')
                open(filename, 'w')
                price.setup_cache(filename, True)
                self.assertEqual(1, mock_remove.call_count)
                self.assertTrue(path.exists(filename))

    def test_leave_cache(self):
        with mock.patch('os.remove') as mock_remove:
            with tempfile.TemporaryDirectory() as tmpdir:
                filename = path.join(tmpdir, 'cache.db')
                open(filename, 'w')
                price.setup_cache(None, False)
                self.assertEqual(0, mock_remove.call_count)
                self.assertTrue(path.exists(filename))


class TestProcessArguments(unittest.TestCase):

    def test_filename_not_exists(self):
        with test_utils.capture('stderr') as stderr:
            with self.assertRaises(SystemExit):
                args, jobs = test_utils.run_with_args(
                    price.process_args, ['--no-cache', '/some/file.beancount'])

    def test_filename_exists(self):
        with tempfile.NamedTemporaryFile('w') as tmpfile:
            with test_utils.capture('stderr') as stderr:
                args, jobs = test_utils.run_with_args(
                    price.process_args, ['--no-cache', tmpfile.name])
                self.assertEqual([], jobs)  # Empty file.

    def test_expressions(self):
        with test_utils.capture('stderr') as stderr:
            args, jobs = test_utils.run_with_args(
                price.process_args, ['--no-cache', '-e', 'google/NASDAQ:AAPL'])
            self.assertEqual(
                [find_prices.Job('google', 'NASDAQ:AAPL', None, False, None, None)], jobs)




# FIXME: TODO - Behavior to be implemented and tested.
"""
    parse_date = lambda s: parse_datetime(s).date()
    parser.add_argument('--date', action='store', type=parse_date,
                        help="Specify the date for which to fetch the price.")

    parser.add_argument('-i', '--always-invert', action='store_true',
                        help=("Never just swap currencies for inversion, always invert the "
                              "actual rate"))

    parser.add_argument('-a', '--all', '--all-commodities', '--all-instruments',
                        action='store_true',
                        help=("Select all commodities from files, not just the ones active "
                              "on the date"))

    parser.add_argument('-c', '--clobber', action='store_true',
                        help=("Do not skip prices which are already present in input "
                              "files; fetch them anyway."))

    parser.add_argument('-n', '--dry-run', '--jobs', '--print-only', action='store_true',
                        help=("Don't actually fetch the prices, just print the list of the "
                              "ones to be fetched."))
"""

__incomplete__ = True
