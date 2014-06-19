import textwrap

from beancount import loader
from beancount.plugins import exclude_tag
from beancount.parser import cmptest


class TestExampleExcludeTag(cmptest.TestCase):

    def test_exclude_tag(self):
        INPUT = textwrap.dedent("""

            option "plugin" "beancount.plugins.exclude_tag"

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
