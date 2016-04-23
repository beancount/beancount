__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import datetime

from beancount.utils import date_utils


class TestDateUtils(unittest.TestCase):

    def test_iter_dates(self):
        date1 = datetime.date(2013, 5, 6)
        date2 = datetime.date(2013, 5, 11)
        self.assertEqual([], list(date_utils.iter_dates(date1, date1)))
        self.assertEqual([datetime.date(2013, 5, x) for x in range(6, 11)],
                         list(date_utils.iter_dates(date1, date2)))
        self.assertEqual([], list(date_utils.iter_dates(date2, date1)))

    def test_parse_date(self):
        self.assertEqual(datetime.date(2014, 12, 7),
                         date_utils.parse_date_liberally('12/7/2014'))
        self.assertEqual(datetime.date(2014, 12, 7),
                         date_utils.parse_date_liberally('7-Dec-2014'))

    def test_next_month(self):
        self.assertEqual(datetime.date(2015, 11, 1),
                         date_utils.next_month(datetime.date(2015, 10, 1)))
        self.assertEqual(datetime.date(2015, 11, 1),
                         date_utils.next_month(datetime.date(2015, 10, 30)))
        self.assertEqual(datetime.date(2016, 1, 1),
                         date_utils.next_month(datetime.date(2015, 12, 15)))
