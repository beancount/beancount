import re
import unittest
from unittest import mock


from beancount import loader
from beancount.parser import options
from beancount.core import realization
from beancount.core import inventory
from beancount.web import journal




class TestJournal(unittest.TestCase):

    def setUp(self):
        self.request = mock.MagicMock()
        self.request.view = None

        def mock_get_account(url_name, slashed_account_name):
            return '/account/{}'.format(slashed_account_name)
        self.request.app.get_url = mock_get_account

    def test_account_link(self):
        # Call with string, with no view.
        link = journal.account_link('Assets:US:BofA:Checking', False, self.request)
        self.assertTrue(re.search('<span', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Assets:US:BofA:Checking', link))

        # Call with string, with a view.
        self.request.view = object()
        link = journal.account_link('Assets:US:BofA:Checking', False, self.request)
        self.assertTrue(re.search(r'<a\b', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Assets:US:BofA:Checking', link))

        # Call with RealAccount instance.
        real_root = realization.RealAccount('')
        real_account = realization.get_or_create(real_root, 'Assets:US:BofA:Checking')
        link_real = journal.account_link('Assets:US:BofA:Checking', False, self.request)
        self.assertEqual(link, link_real)

        # Call rendering the leaf only.
        link = journal.account_link('Assets:US:BofA:Checking', True, self.request)
        self.assertTrue(re.search(r'<a\b', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Checking', link))
        self.assertFalse(re.search('Assets:US:BofA:Checking', link))

    def test_account_link_rootonly(self):
        # Call with just a root account name.
        link = journal.account_link('Income', False, self.request)
        self.assertTrue(re.search('Income', link))

        link = journal.account_link('Income', True, self.request)
        self.assertTrue(re.search('Income', link))

        self.request.view = object()
        link = journal.account_link('Income', False, self.request)
        self.assertTrue(re.search('Income', link))

        link = journal.account_link('Income', True, self.request)
        self.assertTrue(re.search('Income', link))

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

    # def test_iterate_render_transactions(self):
    #     raise NotImplementedError

    # def test_entries_table_with_balance(self):
    #     raise NotImplementedError

    # def test_entries_table(self):
    #     raise NotImplementedError

    # def test_render_links(self):
    #     raise NotImplementedError
