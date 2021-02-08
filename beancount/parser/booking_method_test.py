"""Tests for individual booking methods.

Note that these should be already covered by the tests in booking_full_test, but
we may want to add more tests here, just calling each method directly and
covering all the possible branches.
"""

__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import inventory
from beancount.parser import (
    booking_method as bm,
    parser,
    booking_full as bf,
    booking_full_test)


class TestGenerateAverageRebookingPostings(unittest.TestCase):
    ACCOUNT = "Assets:Account"

    @parser.parse_doc(allow_incomplete=True)
    def test_single_ante_inventory(self, expected_entries, _, opts):
        """
        2015-10-02 * "Expected postings"
          ; None. Already at average cost.
        """
        for ante_inv in ("1 HOOL", "1 HOOL {2 USD}"):
            self.verify_rebook_at_average(expected_entries, ante_inv)

    @parser.parse_doc(allow_incomplete=True)
    def test_unrelated_ante_inventory_untouched(self, expected_entries, _, opts):
        """
        2015-10-02 * "Expected postings"
          ; None. Targeting unrelated inventory.
        """
        ante_inv = "1 AMZN {1 USD}, 1 AMZN {2 USD}"
        self.verify_rebook_at_average(expected_entries, ante_inv)

    @parser.parse_doc(allow_incomplete=True)
    def test_inventory_without_cost_untouched(self, expected_entries, _, opts):
        """
        2015-10-02 * "Expected postings"
          ; None. No cost attached.
        """
        ante_inv = "1 HOOL, 2 HOOL"
        self.verify_rebook_at_average(expected_entries, ante_inv)

    @parser.parse_doc(allow_incomplete=True)
    def test_ante_inventory_merged(self, expected_entries, _, opts):
        """
        2015-10-02 * "Expected postings"
          M Assets:Account -1 HOOL {1 USD}
          M Assets:Account -1 HOOL {2 USD}
          M Assets:Account 2 HOOL {1.5 USD}
        """
        ante_inv = "1 HOOL {1 USD}, 1 HOOL {2 USD}"
        self.verify_rebook_at_average(expected_entries, ante_inv)

    @parser.parse_doc(allow_incomplete=True)
    def test_ante_inventory_merged_unrelated_inv_ignored(self, expected_entries, _, opts):
        """
        2015-10-02 * "Expected postings"
          M Assets:Account -1 HOOL {1 USD}
          M Assets:Account -1 HOOL {2 USD}
          M Assets:Account 2 HOOL {1.5 USD}
        """
        ante_inv = "1 HOOL {1 USD}, 1 HOOL {2 USD}, 1 OTHER {5 USD}, 1 OTHER {10 USD}"
        self.verify_rebook_at_average(expected_entries, ante_inv)

    def verify_rebook_at_average(self, expected_entries, ante_inv_str):
        ante_inventory = inventory.from_string(ante_inv_str)
        actual_postings = bm.generate_rebook_at_average_postings(
            ante_inventory, self.ACCOUNT, "HOOL", "USD")
        expected_postings = expected_entries[0].postings

        # Interpolate the CostSpecs from the parser to match Cost from our parsed inventory
        expected_postings, inter_errs, _ = bf.interpolate_group(expected_postings,
                                                                balances=None,
                                                                currency="USD",
                                                                tolerances=None)
        self.assertListEqual([], inter_errs)

        # Fixup the metadata
        expected_postings = booking_full_test.normalize_postings(expected_postings)
        self.assertEqual(expected_postings, actual_postings)


if __name__ == '__main__':
    unittest.main()
