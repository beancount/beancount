__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest

from beancount.utils import test_utils

from beanprice.scripts import fetch


SOURCES = ['google', 'yahoo']


class ParseTicker(unittest.TestCase):

    def test_tickers(self):
        self.assertEqual(('google', 'HOOL', False),
                         fetch.parse_ticker('HOOL', 'google'))

        self.assertEqual(('google', 'HOOL', False),
                         fetch.parse_ticker('google/HOOL', 'yahoo'))

        self.assertEqual(('google', 'HOOL', True),
                         fetch.parse_ticker('google/^HOOL', 'yahoo'))

        self.assertEqual(('google', 'TSE:XSP', False),
                         fetch.parse_ticker('google/TSE:XSP', 'yahoo'))

        self.assertEqual(('google', 'USDCAD', True),
                         fetch.parse_ticker('google/^USDCAD', 'yahoo'))


class FetchArgsSpec(unittest.TestCase):

    bogus_spec = 'price://google/HOOL'

    def test_empty(self):
        with self.assertRaises(SystemExit):
            with test_utils.capture('stderr'):
                jobs, do_cache = fetch.process_args([], SOURCES)
                self.assertEqual([], jobs)

    def test_cache(self):
        jobs, do_cache = fetch.process_args([self.bogus_spec], SOURCES)
        self.assertTrue(do_cache)
        jobs, do_cache = fetch.process_args(['--no-cache', self.bogus_spec], SOURCES)
        self.assertFalse(do_cache)

    def test_explicit_invalid(self):
        with test_utils.capture('stderr'):
            with self.assertRaises(SystemExit):
                fetch.process_args(['http://google/AAPL'], SOURCES)

    def test_explicit_price__invalid(self):
        with test_utils.capture('stderr'):
            with self.assertRaises(SystemExit):
                jobs, do_cache = fetch.process_args(['price://bleh/AAPL'], SOURCES)

    def test_explicit_price__valid(self):
        jobs, do_cache = fetch.process_args(['price://google/AAPL'], SOURCES)
        self.assertEqual([fetch.Job('google', 'AAPL', None, None, None, False)], jobs)

    def test_explicit_price__multi(self):
        jobs, do_cache = fetch.process_args(['price://yahoo/FB', 'price://google/AAPL'],
                                            SOURCES)
        self.assertEqual([
            fetch.Job('yahoo', 'FB', None, None, None, False),
            fetch.Job('google', 'AAPL', None, None, None, False),
            ], jobs)

    def test_explicit_file__nonexist(self):
        with test_utils.capture('stderr'):
            with self.assertRaises(SystemExit):
                jobs, do_cache = fetch.process_args(['file:///path/to/nowhere'], SOURCES)

    @test_utils.docfile
    def test_explicit_file__valid(self, filename):
        """
        2015-01-01 open Assets:Invest
        2015-01-01 open Equity:Opening-Balances

        2000-01-01 commodity VEA
          ticker: "NYSEARCA:VEA"

        2000-01-01 commodity VWO
          ticker: "yahoo/VWO"

        2000-01-01 commodity JPY
          ticker: "^CURRENCY:USDJPY"
          quote: USD

        2015-02-20 *
          Assets:Invest         100 VEA {41.2000 USD}
          Assets:Invest         100 VWO {45.3000 USD}
          Assets:Invest          20 BND {81.90 USD}
          Equity:Opening-Balances

        2015-02-20 *
          Assets:Invest       10000 JPY
        """
        jobs, do_cache = fetch.process_args(["file://{}".format(filename)],
                                            SOURCES)
        self.assertEqual([
            fetch.Job('google', 'CURRENCY:USDJPY', None, 'JPY', 'USD', True),
            fetch.Job('google', 'NYSEARCA:VEA', None, 'VEA', 'USD', False),
            fetch.Job('yahoo', 'VWO', None, 'VWO', 'USD', False),
            ], sorted(jobs))
        self.assertTrue(do_cache)

    @test_utils.docfile
    def test_explicit_file__date(self, filename):
        """
        2015-01-01 open Assets:Invest
        2015-01-01 open Equity:Opening-Balances

        2000-01-01 commodity VEA
          ticker: "NYSEARCA:VEA"

        2000-01-01 commodity VWO
          ticker: "NYSEARCA:VWO"

        2015-02-10 *
          Assets:Invest         100 VEA {41.2000 USD}
          Equity:Opening-Balances

        2015-02-15 *
          Assets:Invest         100 VWO {45.3000 USD}
          Equity:Opening-Balances
        """
        jobs, do_cache = fetch.process_args(["--date=2015-02-12",
                                             "file://{}".format(filename)],
                                            SOURCES)
        self.assertEqual([
            fetch.Job('google', 'NYSEARCA:VEA', datetime.date(2015, 2, 12),
                      'VEA', 'USD', False),
            ], sorted(jobs))
        self.assertTrue(do_cache)

    @test_utils.docfile
    def test_explicit_file__badcontents(self, filename):
        """
        2015-01-01 open Assets:Invest
        2015-01-01 open USD ;; Error
        """
        jobs, do_cache = fetch.process_args(["file://{}".format(filename)],
                                            SOURCES)
        self.assertEqual([], jobs)

    def test_implicit_file(self):
        with test_utils.capture('stderr'):
            with self.assertRaises(SystemExit):
                jobs, do_cache = fetch.process_args(['/path/to/nowhere'],
                                                    SOURCES)

    @test_utils.docfile
    def test_source(self, filename):
        """
        2015-01-01 open Assets:Invest
        2015-01-01 open Equity:Opening-Balances

        2000-01-01 commodity VEA
          ticker: "VEA"

        2015-02-20 *
          Assets:Invest         100 VEA {41.2000 USD}
          Equity:Opening-Balances
        """
        jobs, do_cache = fetch.process_args(["--source=yahoo",
                                             "file://{}".format(filename)],
                                            SOURCES)
        self.assertEqual([
            fetch.Job('yahoo', 'VEA', None, 'VEA', 'USD', False),
            ], sorted(jobs))
        self.assertTrue(do_cache)
