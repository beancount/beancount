import datetime
import io
import re
import unittest
import textwrap
import functools
import collections

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.query import query_render
from beancount.utils.misc_utils import box


class TestQueryRender(unittest.TestCase):

    def test_render_str(self):
        types = [('account', str)]
        Row = collections.namedtuple('TestRow', [name for name, type in types])
        rows = [
            Row('Assets:US:Babble:Vacation'),
            Row('Expenses:Vacation'),
            Row('Income:US:Babble:Vacation'),
        ]
        oss = io.StringIO()
        query_render.render_text(types, rows, oss)
        with box():
            print(oss.getvalue())

    def test_render_Decimal(self):
        types = [('number', Decimal)]
        Row = collections.namedtuple('TestRow', [name for name, type in types])
        rows = [
            Row(D('123.1')),
            Row(D('234.12')),
            Row(D('345.123')),
            Row(D('456.1234')),
        ]
        oss = io.StringIO()
        query_render.render_text(types, rows, oss)
        with box():
            print(oss.getvalue())
