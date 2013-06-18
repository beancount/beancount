"""
Tests for general utils.
"""
from beancount import utils

import datetime
import unittest


class TestUtils(unittest.TestCase):

    def test_date_ticker_one_month(self):
        ticker = utils.DateIntervalTicker(lambda date: date.month)
        for date in utils.iter_dates(datetime.date(2012, 1, 1),
                                     datetime.date(2013, 6, 1)):
            if ticker.check(date):
                self.assertEqual(date.day, 1)

    def test_date_ticker_three_months(self):
        ticker = utils.DateIntervalTicker(
            lambda date: ((date.year * 12 + (date.month - 1)) // 3))

        for date in utils.iter_dates(datetime.date(2012, 1, 1),
                                     datetime.date(2013, 6, 1)):
            if ticker.check(date):
                self.assertEqual(date.day, 1)
                self.assertTrue(((date.month-1) % 3) == 0)
