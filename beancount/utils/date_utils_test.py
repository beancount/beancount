__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import datetime

from beancount.utils import date_utils


class TestDateUtils(unittest.TestCase):
    def test_iter_dates(self):
        date1 = datetime.date(2013, 5, 6)
        date2 = datetime.date(2013, 5, 11)
        self.assertEqual([], list(date_utils.iter_dates(date1, date1)))
        self.assertEqual(
            [datetime.date(2013, 5, x) for x in range(6, 11)],
            list(date_utils.iter_dates(date1, date2)),
        )
        self.assertEqual([], list(date_utils.iter_dates(date2, date1)))

    def test_next_month(self):
        self.assertEqual(
            datetime.date(2015, 11, 1), date_utils.next_month(datetime.date(2015, 10, 1))
        )
        self.assertEqual(
            datetime.date(2015, 11, 1), date_utils.next_month(datetime.date(2015, 10, 30))
        )
        self.assertEqual(
            datetime.date(2016, 1, 1), date_utils.next_month(datetime.date(2015, 12, 15))
        )

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


if __name__ == "__main__":
    unittest.main()
