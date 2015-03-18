__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import datetime

from beancount.utils import date_utils


class TestDateUtils(unittest.TestCase):

    def test_parse_date(self):
        self.assertEqual(datetime.date(2014, 12, 7),
                         date_utils.parse_date_liberally('12/7/2014'))
        self.assertEqual(datetime.date(2014, 12, 7),
                         date_utils.parse_date_liberally('7-Dec-2014'))
