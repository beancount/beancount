__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import datetime
import random

from beancount.reports import gviz


class TestGviz(unittest.TestCase):

    def test_gviz_timeline(self):
        times = [datetime.datetime(2010, 3, 3),
                 datetime.datetime(2010, 3, 17),
                 datetime.datetime(2010, 3, 19),
                 datetime.datetime(2010, 4, 7),
                 datetime.datetime(2010, 4, 8),
                 datetime.datetime(2010, 4, 8),
                 datetime.datetime(2010, 4, 10),
                 datetime.datetime(2010, 7, 1),
                 datetime.datetime(2010, 8, 22)]
        data = {
            'x': [random.random() * 20 for _ in range(len(times))],
            'y': [random.random() * 15 for _ in range(len(times))],
            'z': [random.random() * 15 for _ in range(len(times) + 2)],
        }
        html = gviz.gviz_timeline(times, data)
        self.assertTrue(isinstance(html, str))

        self.assertRegex(html, r'\bx\b')
        self.assertRegex(html, r'\by\b')
        self.assertRegex(html, r'\bz\b')
        self.assertRegex(html, r'\.visualization\.AnnotatedTimeLine')
        self.assertRegex(html, r'\.setOnLoadCallback')
