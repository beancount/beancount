__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import io
import unittest

from beancount import loader
from beancount.core import realization
from beancount.core import display_context
from beancount.reports import tree_table
from beancount.reports import html_formatter


class TestActiveAccounts(unittest.TestCase):

    @loader.load_doc()
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

    @loader.load_doc()
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
        self.assertRegex(html, '<table')
        self.assertRegex(html, '3000')
        self.assertRegex(html, '-3000')
        self.assertRegex(html, '5cdc3b134179')
        self.assertRegex(html, 'Assets:US:Checking')

    def test_table_of_balances(self):
        formatter = html_formatter.HTMLFormatter(display_context.DEFAULT_DISPLAY_CONTEXT)
        price_map, price_date = {}, None
        html = tree_table.table_of_balances(self.real_root, price_map, price_date,
                                            ['USD', 'CAD'], formatter,
                                            classes=['586e8200b379'])
        self.assertRegex(html, '<table')
        self.assertRegex(html, 'USD')
        self.assertRegex(html, 'CAD')
        self.assertRegex(html, '586e8200b379')
        self.assertRegex(html, 'Checking')

        # Check that an inactive account is being skipped.
        self.assertNotRegex(html, 'Inactive')
