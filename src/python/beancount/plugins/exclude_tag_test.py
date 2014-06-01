import textwrap
import unittest

from beancount import loader
from beancount.plugins import exclude_tag
from beancount.parser import printer
from beancount.utils import test_utils



class TestExampleExcludeTag(test_utils.TestCase):

    def setUp(self):
        loader.install_load_plugin(exclude_tag.exclude_tag)

    def tearDown(self):
        loader.uninstall_load_plugin(exclude_tag.exclude_tag)

    def test_exclude_tag(self):
        INPUT = textwrap.dedent("""

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Cash

            2011-05-17 * "This transaction should be included"
              Expenses:Restaurant   50.02 USD
              Assets:Cash

            2011-05-17 * "This transaction should be excluded" #virtual
              Expenses:Restaurant   11.11 USD
              Assets:Cash

        """)
        entries, _, __ = loader.load(INPUT, parse_method='string')
        self.assertEqualEntries("""

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Cash

            2011-05-17 * "This transaction should be included"
              Expenses:Restaurant   50.02 USD
              Assets:Cash

        """, entries)
