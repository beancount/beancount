__author__ = "Martin Blais <blais@furius.ca>"

import re
import io
import unittest

from beancount import loader
from beancount.core import realization
from beancount.reports import tree_table


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
        self.assertFalse(tree_table.is_account_active(
            realization.get(real_root, 'Assets:Inactive')))
        self.assertTrue(tree_table.is_account_active(
            realization.get(real_root, 'Assets:Active')))


class TestTables(unittest.TestCase):

    @loader.loaddoc
    def setUp(self, entries, _, __):
        """
        2014-01-01 open Assets:US:Checking
        2014-01-01 open Assets:CA:Checking
        2014-01-01 open Assets:Inactive
        2014-01-01 open Equity:Opening-Balances

        2014-07-04 *
          Assets:US:Checking      3000 USD
          Equity:Opening-Balances

        2014-07-04 *
          Assets:CA:Checking      3000 CAD
          Equity:Opening-Balances

        """
        self.real_root = realization.realize(entries)

    def test_tree_table(self):
        oss = io.StringIO()
        for real_node, cells, classes in tree_table.tree_table(
                oss, self.real_root, None,
                header=['Account', 'Balance'],
                classes=['5cdc3b134179']):

            if real_node is tree_table.TOTALS_LINE:
                cells.append('THE_TOTAL')
                continue
            cells.append("<pre>{}</pre>".format(real_node.balance))
        html = oss.getvalue()
        self.assertTrue(re.search('<table', html))
        self.assertTrue(re.search('3,000', html))
        self.assertTrue(re.search('-3,000', html))
        self.assertTrue(re.search('5cdc3b134179', html))
        self.assertTrue(re.search('Assets:US:Checking', html))

    def test_table_of_balances(self):
        html = tree_table.table_of_balances(self.real_root, ['USD', 'CAD'], None,
                                            classes=['586e8200b379'])
        self.assertTrue(re.search('<table', html))
        self.assertTrue(re.search('USD', html))
        self.assertTrue(re.search('CAD', html))
        self.assertTrue(re.search('586e8200b379', html))
        self.assertTrue(re.search('Checking', html))

        # Check that an inactive account is being skipped.
        self.assertFalse(re.search('Inactive', html))
