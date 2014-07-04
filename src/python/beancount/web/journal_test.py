import unittest

from beancount import loader
from beancount.parser import options
from beancount.core import realization
from beancount.web import views


class TestJournal(unittest.TestCase):

    def test_account_link(self):
        raise NotImplementedError

    def test_balance_html(self):
        raise NotImplementedError

    def test_iterate_render_transactions(self):
        raise NotImplementedError

    def test_entries_table_with_balance(self):
        raise NotImplementedError

    def test_entries_table(self):
        raise NotImplementedError

    def test_render_links(self):
        raise NotImplementedError
