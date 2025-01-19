__copyright__ = "Copyright (C) 2014-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount import loader
from beancount.ops import lifetimes
from beancount.utils import test_utils


class TestCommodityLifetimes(test_utils.TestCase):
    @loader.load_doc()
    def test_lifetimes_different_currencies(self, entries, errors, _):
        """
        2000-01-01 open Assets:US:Invest:Cash    USD
        2000-01-01 open Assets:US:Invest:AAPL    AAPL
        2000-01-01 open Assets:US:Invest:CSCO    CSCO
        2000-01-01 open Assets:US:Invest:INTL    INTL
        2000-01-01 open Assets:US:Invest:IBM     IBM
        2000-01-01 open Income:US:Invest:PnL
        2000-01-01 open Assets:US:Bank:Checking  USD

        2000-01-02 * "Deposit"
          Assets:US:Bank:Checking    -10000 USD
          Assets:US:Invest:Cash       10000 USD

        2001-02-10 * "Buy shares"
          Assets:US:Invest:AAPL     10 AAPL {43.00 USD}
          Assets:US:Invest:CSCO     10 CSCO {21.00 USD}
          Assets:US:Invest:INTL     10 INTL {75.00 USD}
          Assets:US:Invest:IBM      10 IBM  {16.00 USD}
          Assets:US:Invest:Cash

        2001-07-20 * "Sell AAPL"
          Assets:US:Invest:AAPL     -10 AAPL {43.00 USD}
          Assets:US:Invest:Cash   500.00 USD
          Income:US:Invest:PnL

        2001-07-21 * "Sell CSCO"
          Assets:US:Invest:CSCO     -10 CSCO {21.00 USD}
          Assets:US:Invest:Cash   300.00 USD
          Income:US:Invest:PnL

        2001-07-22 * "Sell INTL"
          Assets:US:Invest:INTL     -10 INTL {75.00 USD}
          Assets:US:Invest:Cash   800.00 USD
          Income:US:Invest:PnL

        2001-07-23 * "Sell IBM"
          Assets:US:Invest:IBM     -10 IBM {16.00 USD}
          Assets:US:Invest:Cash   200.00 USD
          Income:US:Invest:PnL

        """
        self.assertFalse(errors)
        lifetimes_map = lifetimes.get_commodity_lifetimes(entries)
        self.assertEqual(
            {
                ("USD", None): [(datetime.date(2000, 1, 2), None)],
                ("AAPL", "USD"): [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 21))],
                ("CSCO", "USD"): [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 22))],
                ("INTL", "USD"): [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 23))],
                ("IBM", "USD"): [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 24))],
            },
            lifetimes_map,
        )

    @loader.load_doc()
    def test_lifetimes_closed_open(self, entries, errors, _):
        """
        2000-01-01 open Assets:US:Invest:Cash    USD
        2000-01-01 open Assets:US:Invest:AAPL    AAPL
        2000-01-01 open Income:US:Invest:PnL
        2000-01-01 open Assets:US:Bank:Checking  USD

        2000-01-02 * "Deposit"
          Assets:US:Bank:Checking    -10000 USD
          Assets:US:Invest:Cash       10000 USD

        2001-03-10 * "Buy Apple"
          Assets:US:Invest:AAPL     10 AAPL {43.40 USD}
          Assets:US:Invest:Cash

        2001-08-10 * "Sell some Apple - some will remain"
          Assets:US:Invest:AAPL     -8 AAPL {43.40 USD}
          Assets:US:Invest:Cash   360.00 USD
          Income:US:Invest:PnL

        2001-12-10 * "Sell remaining Apple - this completes the interval"
          Assets:US:Invest:AAPL     -2 AAPL {43.40 USD}
          Assets:US:Invest:Cash   96.00 USD
          Income:US:Invest:PnL

        2002-02-10 * "Buy Apple again - this begins a new interval"
          Assets:US:Invest:AAPL     5 AAPL {48.00 USD}
          Assets:US:Invest:Cash

        2002-06-10 * "Sell Apple again - this ends it"
          Assets:US:Invest:AAPL     -5 AAPL {48.00 USD}
          Assets:US:Invest:Cash   260.00 USD
          Income:US:Invest:PnL

        2003-04-10 * "Buy Apple - keep this open"
          Assets:US:Invest:AAPL     7 AAPL {50.00 USD}
          Assets:US:Invest:Cash
        """
        self.assertFalse(errors)
        lifetimes_map = lifetimes.get_commodity_lifetimes(entries)
        self.assertEqual(
            {
                ("USD", None): [(datetime.date(2000, 1, 2), None)],
                ("AAPL", "USD"): [
                    (datetime.date(2001, 3, 10), datetime.date(2001, 12, 11)),
                    (datetime.date(2002, 2, 10), datetime.date(2002, 6, 11)),
                    (datetime.date(2003, 4, 10), None),
                ],
            },
            lifetimes_map,
        )

    @loader.load_doc()
    def test_lifetimes_cross_accounts(self, entries, errors, _):
        """
        2000-01-01 open Assets:US:InvestA:Cash    USD
        2000-01-01 open Assets:US:InvestA:AAPL    AAPL
        2000-01-01 open Income:US:InvestA:PnL
        2000-01-01 open Assets:US:InvestB:Cash    USD
        2000-01-01 open Assets:US:InvestB:AAPL    AAPL
        2000-01-01 open Income:US:InvestB:PnL
        2000-01-01 open Assets:US:Bank:Checking  USD

        2000-01-02 * "Deposit"
          Assets:US:Bank:Checking    -10000 USD
          Assets:US:InvestA:Cash       5000 USD
          Assets:US:InvestB:Cash       5000 USD

        2001-03-10 * "Buy Apple - in first account"
          Assets:US:InvestA:AAPL     10 AAPL {43.40 USD}
          Assets:US:InvestA:Cash

        2001-05-10 * "Buy Apple - in second account"
          Assets:US:InvestB:AAPL     10 AAPL {44.10 USD}
          Assets:US:InvestB:Cash

        2001-06-10 * "Sell Apple - in first account, resulting position is zero"
          Assets:US:InvestA:AAPL     -10 AAPL {43.40 USD}
          Assets:US:InvestA:Cash  500.00 USD
          Income:US:InvestA:PnL

        2001-06-11 balance Assets:US:InvestA:AAPL    0 AAPL

        2001-09-10 * "Sell Apple - in second account, this is the last AAPL position"
          Assets:US:InvestB:AAPL     -10 AAPL {44.10 USD}
          Assets:US:InvestB:Cash  500.00 USD
          Income:US:InvestB:PnL

        2001-09-11 balance Assets:US:InvestB:AAPL    0 AAPL
        """
        self.assertFalse(errors)
        lifetimes_map = lifetimes.get_commodity_lifetimes(entries)
        self.assertEqual(
            {
                ("AAPL", "USD"): [(datetime.date(2001, 3, 10), datetime.date(2001, 9, 11))],
                ("USD", None): [(datetime.date(2000, 1, 2), None)],
            },
            lifetimes_map,
        )


class TestCompressLifetimes(test_utils.TestCase):
    def test_single_closed(self):
        intervals = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 10))]
        compressed = lifetimes.compress_intervals_days(intervals, 10)
        self.assertEqual(intervals, compressed)
        compressed = lifetimes.compress_intervals_days(intervals, 100)
        self.assertEqual(intervals, compressed)

    def test_single_open(self):
        intervals = [(datetime.date(2014, 2, 3), None)]
        compressed = lifetimes.compress_intervals_days(intervals, 10)
        self.assertEqual(intervals, compressed)
        compressed = lifetimes.compress_intervals_days(intervals, 100)
        self.assertEqual(intervals, compressed)

    def test_multiple_no_compress(self):
        intervals = [
            (datetime.date(2014, 2, 3), datetime.date(2014, 3, 10)),
            (datetime.date(2014, 3, 20), datetime.date(2014, 7, 1)),
        ]
        for num_days in 0, 1, 9, 10:
            compressed = lifetimes.compress_intervals_days(intervals, num_days)
            self.assertEqual(intervals, compressed)

    def test_multiple_compress(self):
        intervals = [
            (datetime.date(2014, 2, 3), datetime.date(2014, 3, 10)),
            (datetime.date(2014, 3, 20), datetime.date(2014, 7, 1)),
        ]
        expected = [(datetime.date(2014, 2, 3), datetime.date(2014, 7, 1))]
        for num_days in 11, 12, 100, 2000:
            compressed = lifetimes.compress_intervals_days(intervals, num_days)
            self.assertEqual(expected, compressed)


class TestTrimLifetimes(test_utils.TestCase):
    def test_single_closed(self):
        intervals = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 10))]
        expected = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 10))]
        trimmed = lifetimes.trim_intervals(intervals, None, None)
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 7), datetime.date(2014, 3, 10))]
        trimmed = lifetimes.trim_intervals(intervals, datetime.date(2014, 2, 7), None)
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 3))]
        trimmed = lifetimes.trim_intervals(intervals, None, datetime.date(2014, 3, 3))
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 7), datetime.date(2014, 3, 3))]
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 2, 7), datetime.date(2014, 3, 3)
        )
        self.assertEqual(expected, trimmed)

        expected = []
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 1, 2), datetime.date(2014, 2, 2)
        )
        self.assertEqual(expected, trimmed)

        expected = []
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 3, 11), datetime.date(2014, 3, 20)
        )
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 3))]
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 1, 2), datetime.date(2014, 3, 3)
        )
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 7), datetime.date(2014, 3, 10))]
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 2, 7), datetime.date(2014, 3, 20)
        )
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 10))]
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 1, 2), datetime.date(2014, 3, 20)
        )
        self.assertEqual(expected, trimmed)

    def test_single_open(self):
        intervals = [(datetime.date(2014, 2, 3), None)]
        expected = [(datetime.date(2014, 2, 3), None)]
        trimmed = lifetimes.trim_intervals(intervals, None, None)
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 7), datetime.date(2014, 3, 10))]
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 2, 7), datetime.date(2014, 3, 10)
        )
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 3), datetime.date(2014, 3, 10))]
        trimmed = lifetimes.trim_intervals(intervals, None, datetime.date(2014, 3, 10))
        self.assertEqual(expected, trimmed)

        expected = [(datetime.date(2014, 2, 7), None)]
        trimmed = lifetimes.trim_intervals(intervals, datetime.date(2014, 2, 7), None)
        self.assertEqual(expected, trimmed)

    def test_multiple(self):
        intervals = [
            (datetime.date(2014, 2, 3), datetime.date(2014, 3, 10)),
            (datetime.date(2014, 3, 20), datetime.date(2014, 7, 1)),
        ]
        expected = [
            (datetime.date(2014, 2, 7), datetime.date(2014, 3, 10)),
            (datetime.date(2014, 3, 20), datetime.date(2014, 5, 1)),
        ]
        trimmed = lifetimes.trim_intervals(
            intervals, datetime.date(2014, 2, 7), datetime.date(2014, 5, 1)
        )
        self.assertEqual(expected, trimmed)


class TestLifetimeDateIterators(test_utils.TestCase):
    def test_iter_weeks(self):
        lifetimes_map = {
            ("AAPL", "USD"): [
                (datetime.date(2014, 2, 6), datetime.date(2014, 3, 10)),
                (datetime.date(2014, 5, 20), datetime.date(2014, 7, 1)),
            ],
            ("USD", None): [(datetime.date(2014, 1, 1), None)],
        }

        required_prices = list(
            lifetimes.required_weekly_prices(lifetimes_map, datetime.date(2014, 9, 1))
        )
        self.assertEqual(
            [
                (datetime.date(2014, 1, 31), "AAPL", "USD"),
                (datetime.date(2014, 2, 7), "AAPL", "USD"),
                (datetime.date(2014, 2, 14), "AAPL", "USD"),
                (datetime.date(2014, 2, 21), "AAPL", "USD"),
                (datetime.date(2014, 2, 28), "AAPL", "USD"),
                (datetime.date(2014, 3, 7), "AAPL", "USD"),
                (datetime.date(2014, 5, 16), "AAPL", "USD"),
                (datetime.date(2014, 5, 23), "AAPL", "USD"),
                (datetime.date(2014, 5, 30), "AAPL", "USD"),
                (datetime.date(2014, 6, 6), "AAPL", "USD"),
                (datetime.date(2014, 6, 13), "AAPL", "USD"),
                (datetime.date(2014, 6, 20), "AAPL", "USD"),
                (datetime.date(2014, 6, 27), "AAPL", "USD"),
            ],
            required_prices,
        )

    def test_iter_weekdays(self):
        lifetimes_map = {
            ("AAPL", "USD"): [
                (datetime.date(2014, 2, 2), datetime.date(2014, 2, 10)),
                (datetime.date(2014, 5, 20), datetime.date(2014, 5, 24)),
            ],
            ("USD", None): [(datetime.date(2014, 1, 1), None)],
        }

        required_prices = list(
            lifetimes.required_daily_prices(lifetimes_map, datetime.date(2014, 9, 1), True)
        )
        self.assertEqual(
            [
                (datetime.date(2014, 1, 31), "AAPL", "USD"),
                (datetime.date(2014, 2, 3), "AAPL", "USD"),
                (datetime.date(2014, 2, 4), "AAPL", "USD"),
                (datetime.date(2014, 2, 5), "AAPL", "USD"),
                (datetime.date(2014, 2, 6), "AAPL", "USD"),
                (datetime.date(2014, 2, 7), "AAPL", "USD"),
                (datetime.date(2014, 5, 20), "AAPL", "USD"),
                (datetime.date(2014, 5, 21), "AAPL", "USD"),
                (datetime.date(2014, 5, 22), "AAPL", "USD"),
                (datetime.date(2014, 5, 23), "AAPL", "USD"),
            ],
            required_prices,
        )

    def test_iter_days(self):
        lifetimes_map = {
            ("AAPL", "USD"): [
                (datetime.date(2014, 2, 2), datetime.date(2014, 2, 10)),
                (datetime.date(2014, 5, 20), datetime.date(2014, 5, 24)),
            ],
            ("USD", None): [(datetime.date(2014, 1, 1), None)],
        }

        required_prices = list(
            lifetimes.required_daily_prices(lifetimes_map, datetime.date(2014, 9, 1), False)
        )
        self.assertEqual(
            [
                (datetime.date(2014, 2, 2), "AAPL", "USD"),
                (datetime.date(2014, 2, 3), "AAPL", "USD"),
                (datetime.date(2014, 2, 4), "AAPL", "USD"),
                (datetime.date(2014, 2, 5), "AAPL", "USD"),
                (datetime.date(2014, 2, 6), "AAPL", "USD"),
                (datetime.date(2014, 2, 7), "AAPL", "USD"),
                (datetime.date(2014, 2, 8), "AAPL", "USD"),
                (datetime.date(2014, 2, 9), "AAPL", "USD"),
                (datetime.date(2014, 5, 20), "AAPL", "USD"),
                (datetime.date(2014, 5, 21), "AAPL", "USD"),
                (datetime.date(2014, 5, 22), "AAPL", "USD"),
                (datetime.date(2014, 5, 23), "AAPL", "USD"),
            ],
            required_prices,
        )


if __name__ == "__main__":
    unittest.main()
