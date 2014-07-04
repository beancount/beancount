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



def mock_build_url(name, **kw):
    "A fake URL builder, just for testing."
    return '/{}/{}'.format(name, '/'.join(value
                                          for _, value in sorted(kw.items())))


class TestTables(unittest.TestCase):

    @loader.loaddoc
    def setUp(self, entries, _, __):
        """
        2014-01-01 open Assets:Checking
        2014-01-01 open Equity:OpeningBalances

        2014-07-04 *
          Assets:Checking      3000 USD
          Equity:OpeningBalances
        """
        self.real_root = realization.realize(entries)

    def test_tree_table(self):
        oss = io.StringIO()
        for real_node, cells, classes in acctree.tree_table(oss,
                                                            self.real_root,
                                                            mock_build_url,
                                                            ['Account', 'Balance']):
            if real_node is acctree.TOTALS_LINE:
                cells.append('THE_TOTAL')
                continue
            cells.append("<pre>{}</pre>".format(real_node.balance))

        html = oss.getvalue()
        self.assertTrue(re.search('<table', html))
        self.assertTrue(re.search('3000', html))
        self.assertTrue(re.search('-3000', html))



# class TestTableOfBalances(unittest.TestCase):

#     def test_table_of_balances(self):
#         raise NotImplementedError
