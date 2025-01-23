__copyright__ = "Copyright (C) 2014-2017, 2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader


class TestLeafOnly(unittest.TestCase):
    @loader.load_doc(expect_errors=True)
    def test_leaf_only1(self, _, errors, __):
        """
        plugin "beancount.plugins.leafonly"

        2011-01-01 open Expenses:Food
        2011-01-01 open Expenses:Food:Restaurant
        2011-01-01 open Assets:Other

        2011-05-17 * "Something"
          Expenses:Food:Restaurant   1.00 USD
          Assets:Other              -1.00 USD

        2011-05-17 * "Something"
          Expenses:Food         1.00 USD ;; Offending posting.
          Assets:Other         -1.00 USD

        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Expenses:Food")

    @loader.load_doc(expect_errors=True)
    def test_leaf_only2(self, _, errors, __):
        """
        plugin "beancount.plugins.leafonly"

        ;;; 2011-01-01 open Expenses:Food
        2011-01-01 open Expenses:Food:Restaurant
        2011-01-01 open Assets:Other

        2011-05-17 * "Something"
          Expenses:Food         1.00 USD ;; Offending posting.
          Assets:Other         -1.00 USD

        """
        # Issue #5: If you have a non-leaf posting on an account that doesn't
        # exist, the leafonly plugin raises an AttributeError if there is no
        # Open directive. The problem is that 'open_entry' is None.
        self.assertEqual(2, len(errors))
        for error in errors:
            self.assertRegex(error.message, "Expenses:Food")

    @loader.load_doc(expect_errors=False)
    def test_leaf_only3(self, _, errors, __):
        """
        plugin "beancount.plugins.leafonly"

        2011-01-01 open Expenses:Food
        2011-01-01 open Expenses:Food:Restaurant
        2011-01-01 open Assets:Other

        2011-05-17 * "Something"
          Expenses:Food:Restaurant   1.00 USD
          Assets:Other              -1.00 USD

        2011-05-18 balance Expenses:Food 1.00 USD  ;; Allowed balance posting.
        """
        self.assertEqual(0, len(errors))


if __name__ == "__main__":
    unittest.main()
