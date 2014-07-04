import re
import io
import unittest

from beancount import loader
from beancount.core import realization
from beancount.core import inventory
from beancount.core import data
from beancount.web import acctree


class TestActiveAccounts(unittest.TestCase):

    @loader.loaddoc
    def test_is_account_active(self, entries, _, __):
        """
        2014-01-01 open Assets:Inactive
        2014-01-01 open Assets:Active
        2014-01-01 open Equity:Other

        2014-07-04 *
          Assets:Active   1 USD
          Equity:Other

        """
        real_root = realization.realize(entries)
        self.assertFalse(acctree.is_account_active(
            realization.get(real_root, 'Assets:Inactive')))
        self.assertTrue(acctree.is_account_active(
            realization.get(real_root, 'Assets:Active')))



# class TestTreeTable(unittest.TestCase):

#     def test_tree_table(self):
#         raise NotImplementedError


# class TestTableOfBalances(unittest.TestCase):

#     def test_table_of_balances(self):
#         raise NotImplementedError
