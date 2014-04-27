"""
Tests for general utils.
"""
import unittest
import time

from beancount.utils import misc_utils


class TestMiscUtils(unittest.TestCase):

    def test_print_time(self):
        with misc_utils.print_time('test-op'):
            time.sleep(0.1)

    def test_groupby(self):
        data = [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
        grouped = misc_utils.groupby(lambda x: x[0], data)
        self.assertEqual(set(['a', 'b', 'c', 'd']), grouped.keys())
        self.assertEqual(
            [[('a', 1)], [('b', 2)], [('c', 3)], [('d', 4)]],
            sorted(grouped.values()))

    def test_filter_type(self):
        class A: pass
        class B: pass
        class C: pass
        data = [x() for x in [A, B, A, A, C, B, C, A]]
        self.assertEqual([A, A, A, A],
                         list(map(type, misc_utils.filter_type(data, A))))


    # def test_date_ticker_one_month(self):
    #     ticker = utils.DateIntervalTicker(lambda date: date.month)
    #     for date in utils.iter_dates(datetime.date(2012, 1, 1),
    #                                  datetime.date(2013, 6, 1)):
    #         if ticker.check(date):
    #             self.assertEqual(date.day, 1)

    # def test_date_ticker_three_months(self):
    #     ticker = utils.DateIntervalTicker(
    #         lambda date: ((date.year * 12 + (date.month - 1)) // 3))
    #
    #     for date in utils.iter_dates(datetime.date(2012, 1, 1),
    #                                  datetime.date(2013, 6, 1)):
    #         if ticker.check(date):
    #             self.assertEqual(date.day, 1)
    #             self.assertTrue(((date.month-1) % 3) == 0)


__incomplete__ = True
