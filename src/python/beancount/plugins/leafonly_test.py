import re
import unittest

from beancount import loader


class TestLeafOnly(unittest.TestCase):

    @loader.loaddoc
    def test_leaf_only(self, _, errors, __):
        """
            option "plugin" "beancount.plugins.leafonly"

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
        self.assertTrue(re.search('Expenses:Food', errors[0].message))
