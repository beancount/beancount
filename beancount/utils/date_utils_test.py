__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import datetime
import dateutil

from beancount.utils import date_utils


class TestDateUtils(unittest.TestCase):

    def test_iter_dates(self):
        date1 = datetime.date(2013, 5, 6)
        date2 = datetime.date(2013, 5, 11)
        self.assertEqual([], list(date_utils.iter_dates(date1, date1)))
        self.assertEqual([datetime.date(2013, 5, x) for x in range(6, 11)],
                         list(date_utils.iter_dates(date1, date2)))
        self.assertEqual([], list(date_utils.iter_dates(date2, date1)))

    def test_parse_date_liberally(self):
        const_date = datetime.date(2014, 12, 7)
        test_cases = (
            ('12/7/2014',),
            ('7-Dec-2014',),
            ('7/12/2014', {'parserinfo': dateutil.parser.parserinfo(dayfirst=True)}),
            ('12/7', {'default': datetime.datetime(2014, 1, 1)}),
            ('7.12.2014', {'dayfirst': True}),
            ('14 12 7', {'yearfirst': True}),
            ('Transaction of 7th December 2014', {'fuzzy': True}),
        )
        for case in test_cases:
            if len(case) == 2:
                parse_date = date_utils.parse_date_liberally(case[0], case[1])
            else:
                parse_date = date_utils.parse_date_liberally(case[0])
            self.assertEqual(const_date, parse_date)

    def test_next_month(self):
        self.assertEqual(datetime.date(2015, 11, 1),
                         date_utils.next_month(datetime.date(2015, 10, 1)))
        self.assertEqual(datetime.date(2015, 11, 1),
                         date_utils.next_month(datetime.date(2015, 10, 30)))
        self.assertEqual(datetime.date(2016, 1, 1),
                         date_utils.next_month(datetime.date(2015, 12, 15)))

    def test_intimezone(self):
        with date_utils.intimezone("America/New_York"):
            now_nyc = datetime.datetime.now()
        with date_utils.intimezone("Europe/Berlin"):
            now_berlin = datetime.datetime.now()
        with date_utils.intimezone("Asia/Tokyo"):
            now_tokyo = datetime.datetime.now()
        self.assertNotEqual(now_nyc, now_berlin)
        self.assertNotEqual(now_berlin, now_tokyo)
        self.assertNotEqual(now_tokyo, now_nyc)


if __name__ == '__main__':
    unittest.main()
