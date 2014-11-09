import unittest
import re

from beancount.reports import html_formatter
from beancount.core import data
from beancount.core import inventory


class TestHTMLFormatter(unittest.TestCase):

    def test_functions(self):
        # Just a smoke test.
        formatter = html_formatter.HTMLFormatter()
        formatter.render_account('Assets:US:Bank:Checking')
        formatter.render_inventory(inventory.from_string('10 CAD, 2 GOOG {500 USD}'))
        formatter.render_context('2b4722c3f89f43841cacf16325c2')
        formatter.render_link('fc6189c48a53')
        formatter.render_doc('/path/to/my/document.pdf')
        formatter.render_event_type('location')
        formatter.render_commodity(('GOOG', 'USD'))
        formatter.render_source(
            data.Source('/path/to/my/input.beancount', 17))

    def test_render_inventory(self):
        formatter = html_formatter.HTMLFormatter()
        balance = inventory.Inventory()
        self.assertEqual('', formatter.render_inventory(balance))

        balance = inventory.Inventory.from_string('111 USD, 222 CAD, 3 GOOG {400 USD}')
        html_balance = formatter.render_inventory(balance)
        self.assertTrue(re.search(r'\b111\b', html_balance))
        self.assertTrue(re.search(r'\bUSD\b', html_balance))
        self.assertTrue(re.search(r'\b222\b', html_balance))
        self.assertTrue(re.search(r'\bCAD\b', html_balance))
        self.assertTrue(re.search(r'\b3\b', html_balance))
        self.assertTrue(re.search(r'\bGOOG\b', html_balance))
        self.assertTrue(re.search(r'\b400\b', html_balance))
