__copyright__ = "Copyright (C) 2014-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.core import compare
from beancount.core import data

TEST_INPUT = """

2012-02-01 open Assets:US:Cash
2012-02-01 open Assets:US:Credit-Card
2012-02-01 open Expenses:Grocery
2012-02-01 open Expenses:Coffee
2012-02-01 open Expenses:Restaurant

2012-05-18 * "Buying food" #dinner
  Expenses:Restaurant         100 USD
  Expenses:Grocery            200 USD
  Assets:US:Cash

2013-06-20 * "Whole Foods Market" "Buying books" #books #dinner ^ee89ada94a39
  Expenses:Restaurant         150 USD
  Assets:US:Credit-Card

2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash

2014-02-01 close Assets:US:Cash
2014-02-01 close Assets:US:Credit-Card

"""


class TestCompare(unittest.TestCase):
    def test_hash_entries(self):
        previous_hashes = None
        for _ in range(64):
            entries, errors, options_map = loader.load_string(TEST_INPUT)
            hashes, errors = compare.hash_entries(entries)
            self.assertFalse(errors)
            if previous_hashes is None:
                previous_hashes = hashes
            else:
                self.assertEqual(previous_hashes.keys(), hashes.keys())

    def test_hash_entries_with_duplicates(self):
        entries, _, __ = loader.load_string("""
          2014-08-01 price HOOL  603.10 USD
        """)
        hashes, errors = compare.hash_entries(entries)
        self.assertEqual(1, len(hashes))

        entries, _, __ = loader.load_string("""
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
        """)
        hashes, errors = compare.hash_entries(entries)
        self.assertEqual(1, len(hashes))

    def test_hash_entries_same_postings(self):
        entries1, _, __ = loader.load_string("""
          2020-01-01 * "BarAlice" "Beer with my guy friends ASDF"
            Assets:Cash           -10.00 USD
            Expenses:Food:Drinks    2.00 USD
              shared: "Assets:Debtors:Bob 4.00 USD"
              shared901: "Assets:Debtors:Bob 4.00 USD"
            Assets:Debtors:Bob      4.00 USD
              shared: "Expenses:Food:Drinks 4.00 USD"
            Assets:Debtors:Bob      4.00 USD
              shared: "Expenses:Food:Drinks 4.00 USD"
        """)

        entries2, _, __ = loader.load_string("""
          2020-01-01 * "BarAlice" "Beer with my guy friends ASDF"
            Assets:Cash           -10.00 USD
            Expenses:Food:Drinks    2.00 USD
              shared: "Assets:Debtors:Bob 4.00 USD"
              shared901: "Assets:Debtors:Bob 4.00 USD"
            Assets:Debtors:Bob      4.00 USD
              shared: "Expenses:Food:Drinks 4.00 USD"
        """)
        hashes1, _ = compare.hash_entries(entries1)
        hashes2, _ = compare.hash_entries(entries2)
        self.assertNotEqual(set(hashes1), set(hashes2.keys()))

    def test_compare_entries(self):
        entries1, _, __ = loader.load_string(TEST_INPUT)
        entries2, _, __ = loader.load_string(TEST_INPUT)

        # Check two equal sets.
        same, missing1, missing2 = compare.compare_entries(entries1, entries2)
        self.assertTrue(same)
        self.assertFalse(missing1)
        self.assertFalse(missing2)

        # First > Second.
        same, missing1, missing2 = compare.compare_entries(entries1, entries2[:-1])
        self.assertFalse(same)
        self.assertTrue(missing1)
        self.assertFalse(missing2)
        self.assertEqual(1, len(missing1))
        self.assertTrue(isinstance(missing1.pop(), data.Close))

        # First < Second.
        same, missing1, missing2 = compare.compare_entries(entries1[:-1], entries2)
        self.assertFalse(same)
        self.assertFalse(missing1)
        self.assertTrue(missing2)
        self.assertEqual(1, len(missing2))
        self.assertTrue(isinstance(missing2.pop(), data.Close))

        # Both have missing.
        same, missing1, missing2 = compare.compare_entries(entries1[1:], entries2[:-1])
        self.assertFalse(same)
        self.assertTrue(missing1)
        self.assertTrue(missing2)
        self.assertEqual(1, len(missing1))
        self.assertTrue(isinstance(missing1.pop(), data.Close))
        self.assertEqual(1, len(missing2))
        self.assertTrue(isinstance(missing2.pop(), data.Open))

    def test_includes_entries(self):
        entries1, _, __ = loader.load_string(TEST_INPUT)
        entries2, _, __ = loader.load_string(TEST_INPUT)

        includes, missing = compare.includes_entries(entries1[0:-3], entries2)
        self.assertTrue(includes)
        self.assertFalse(missing)

        includes, missing = compare.includes_entries(entries1, entries2[0:-3])
        self.assertFalse(includes)
        self.assertEqual(3, len(missing))

    def test_excludes_entries(self):
        entries1, _, __ = loader.load_string(TEST_INPUT)
        entries2, _, __ = loader.load_string(TEST_INPUT)

        excludes, extra = compare.excludes_entries(entries1[0:4], entries2)
        self.assertFalse(excludes)
        self.assertTrue(extra)

        excludes, extra = compare.excludes_entries(entries1[0:4], entries2[4:])
        self.assertTrue(excludes)
        self.assertFalse(extra)

    def test_hash_with_exclude_meta(self):
        entries, _, __ = loader.load_string("""
          2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
            Expenses:Coffee         5 USD
            Assets:US:Cash

          2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
            Expenses:Coffee         5 USD
            Assets:US:Cash
        """)
        self.assertNotEqual(
            compare.hash_entry(entries[0], exclude_meta=False),
            compare.hash_entry(entries[1], exclude_meta=False),
        )
        self.assertEqual(
            compare.hash_entry(entries[0], exclude_meta=True),
            compare.hash_entry(entries[1], exclude_meta=True),
        )


if __name__ == "__main__":
    unittest.main()
