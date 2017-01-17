"""Unit tests for sheets upload tool."""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

import subprocess
import unittest
import textwrap
from os import path

from beancount.tools import sheets_upload


_NOT_FOUND = '__NOT_FOUND__'

class TestPopAlist(unittest.TestCase):

    def test_pop_alist__empty(self):
        self.assertIs(_NOT_FOUND, sheets_upload.pop_alist([], 'apples', _NOT_FOUND))

    def test_pop_alist__not_found(self):
        self.assertIs(_NOT_FOUND, sheets_upload.pop_alist([
            ('oranges', 100),
            ('bananas', 101),
        ], 'apples', _NOT_FOUND))

    def test_pop_alist__found(self):
        self.assertIs(101, sheets_upload.pop_alist([
            ('oranges', 100),
            ('apples', 101),
            ('bananas', 102),
        ], 'apples'))

    def test_pop_alist__not_unique(self):
        self.assertIs(101, sheets_upload.pop_alist([
            ('oranges', 100),
            ('apples', 101),
            ('apples', 102),
            ('bananas', 103),
            ('apples', 104),
        ], 'apples'))


class TestSheetsUtils(unittest.TestCase):

    def test_sheet_range(self):
        self.assertEqual('Balances!A1:T100',
                         sheets_upload.sheet_range('Balances', 100, 20))
