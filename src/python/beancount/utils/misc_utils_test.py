"""
Tests for general utils.
"""
import unittest
import time
from collections import namedtuple
import operator
import datetime

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

    def test_longest(self):
        data = [(1,), (2,3,4,5), (2,3)]
        self.assertEqual((2,3,4,5), misc_utils.longest(data))

    def test_get_tuple_values(self):
        Something = namedtuple('Something', 'a b c d e')
        SomethingElse = namedtuple('SomethingElse', 'f g h')
        class A(str): pass
        ntuple = Something(1, 2, SomethingElse(A('a'), None, 2), [A('b'), 'c'], 5)
        x = misc_utils.get_tuple_values(ntuple, lambda x: isinstance(x, A))
        self.assertEqual([A('a'), A('b')], list(x))
        
    def test_index_key(self):
        objects = [object() for _ in range(10)]
        index = misc_utils.index_key(objects, objects[4], lambda x: x, operator.is_)
        self.assertEqual(4, index)

    def test_iter_dates(self):
        date1 = datetime.date(2013, 5, 6)
        date2 = datetime.date(2013, 5, 11)
        self.assertEqual([], list(misc_utils.iter_dates(date1, date1)))
        self.assertEqual([datetime.date(2013, 5, x) for x in range(6, 11)],
                         list(misc_utils.iter_dates(date1, date2)))
        self.assertEqual([], list(misc_utils.iter_dates(date2, date1)))


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
