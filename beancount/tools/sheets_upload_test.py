"""Unit tests for sheets upload tool."""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

# Skip the tests if oauth2client is not available.
import unittest
import pytest
oauth2client = pytest.importorskip('oauth2client')

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

    def test_get_alpha_column(self):
        self.assertEqual("A", sheets_upload.get_alpha_column(0))
        self.assertEqual("B", sheets_upload.get_alpha_column(1))
        self.assertEqual("Z", sheets_upload.get_alpha_column(25))
        self.assertEqual("AA", sheets_upload.get_alpha_column(26))
        self.assertEqual("AZ", sheets_upload.get_alpha_column(51))
        self.assertEqual("BA", sheets_upload.get_alpha_column(52))
        self.assertEqual("BZ", sheets_upload.get_alpha_column(77))
        self.assertEqual("ZZ", sheets_upload.get_alpha_column(701))
        self.assertEqual("AAA", sheets_upload.get_alpha_column(702))
        self.assertEqual("ABA", sheets_upload.get_alpha_column(728))
        self.assertEqual("AZZ", sheets_upload.get_alpha_column(1377))
        self.assertEqual("BAA", sheets_upload.get_alpha_column(1378))
        self.assertEqual("ZZZ", sheets_upload.get_alpha_column(18277))
        self.assertEqual("AAAA", sheets_upload.get_alpha_column(18278))
        self.assertEqual("AZZZ", sheets_upload.get_alpha_column(35853))
        self.assertEqual("BAAA", sheets_upload.get_alpha_column(35854))
        self.assertEqual("ZZZZ", sheets_upload.get_alpha_column(475253))
        self.assertEqual("AAAAA", sheets_upload.get_alpha_column(475254))

    def test_sheet_range(self):
        self.assertEqual('Balances!A1:T100',
                         sheets_upload.sheet_range(100, 20, 'Balances'))
        self.assertEqual('A1:T100',
                         sheets_upload.sheet_range(100, 20))


if __name__ == '__main__':
    unittest.main()
