import re
import unittest
from unittest import mock

from beancount import loader
from beancount.parser import options
from beancount.core import realization
from beancount.core import inventory
from beancount.web import journal


def mock_build_url(name, **kw):
    "A fake URL builder, just for testing."
    return '/{}/{}'.format(name, '/'.join(value
                                          for _, value in sorted(kw.items())))


class TestHTMLAccountLink(unittest.TestCase):

    def test_account_link(self):
        # Call with string, with no view.
        link = journal.account_link('Assets:US:BofA:Checking', None, False)
        self.assertTrue(re.search('<span', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Assets:US:BofA:Checking', link))

        # Call with string, with a build function.
        link = journal.account_link('Assets:US:BofA:Checking', mock_build_url, False)
        self.assertTrue(re.search(r'<a\b', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Assets:US:BofA:Checking', link))

        # Call with RealAccount instance.
        real_root = realization.RealAccount('')
        real_account = realization.get_or_create(real_root, 'Assets:US:BofA:Checking')
        link_real = journal.account_link('Assets:US:BofA:Checking', mock_build_url, False)
        self.assertEqual(link, link_real)

        # Call rendering the leaf only.
        link = journal.account_link('Assets:US:BofA:Checking', mock_build_url, True)
        self.assertTrue(re.search(r'<a\b', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Checking', link))
        self.assertFalse(re.search('Assets:US:BofA:Checking', link))

    def test_account_link_rootonly(self):
        # Call with just a root account name.
        link = journal.account_link('Income', None, False)
        self.assertTrue(re.search('Income', link))

        link = journal.account_link('Income', None, True)
        self.assertTrue(re.search('Income', link))

        link = journal.account_link('Income', mock_build_url, False)
        self.assertTrue(re.search('Income', link))

        link = journal.account_link('Income', mock_build_url, True)
        self.assertTrue(re.search('Income', link))


class TestHTMLBalance(unittest.TestCase):

    def test_balance_html(self):
        balance = inventory.Inventory()
        self.assertEqual('', journal.balance_html(balance))

        balance = inventory.Inventory.from_string('111 USD, 222 CAD, 3 GOOG {400 USD}')
        html_balance = journal.balance_html(balance)
        self.assertTrue(re.search(r'\b111\b', html_balance))
        self.assertTrue(re.search(r'\bUSD\b', html_balance))
        self.assertTrue(re.search(r'\b222\b', html_balance))
        self.assertTrue(re.search(r'\bCAD\b', html_balance))
        self.assertTrue(re.search(r'\b3\b', html_balance))
        self.assertTrue(re.search(r'\bGOOG\b', html_balance))
        self.assertTrue(re.search(r'\b400\b', html_balance))


class TestIterateRenderTransactions(unittest.TestCase):

    @loader.loaddoc
    def test_iterate_render_postings(self, entries, _, __):
        """
        2013-01-01 open Assets:Checking
        2013-01-01 open Assets:Savings
        2013-01-01 open Income:MountainOfMoney

        2014-12-31 close Assets:Checking
        """
        real_root = realization.realize(entries)
        real_account = realization.get(real_root, 'Assets:Checking')

        # for line in journal.iterate_render_postings(real_account.postings,
        #                                             mock_build_url):
        #     print(line)




    # def test_entries_table_with_balance(self):
    #     raise NotImplementedError

    # def test_entries_table(self):
    #     raise NotImplementedError

    # def test_render_links(self):
    #     raise NotImplementedError
