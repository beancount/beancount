__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.parser import cmptest
from beancount.parser import parser
from beancount.core import data
from beancount import loader


class TestMarkUnverified(cmptest.TestCase):

    @loader.load_doc()
    def test_mark_unverified(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.mark_unverified"

        2016-01-01 open Assets:Account1
        2016-01-01 open Assets:Account2
        2016-01-01 open Assets:Account3
        2016-01-01 open Assets:Account4
        2016-01-01 open Assets:Account5
        2016-01-01 open Assets:Account6
        2016-01-01 open Assets:Other

        2016-02-10 *
          Assets:Account1     10 USD
          Assets:Account2     10 USD
          Assets:Account3     10 USD
          Assets:Account4     10 USD
          Assets:Account5     10 USD
          Assets:Account6     10 USD
          Assets:Other

        2016-02-20 *
          Assets:Account1     10 USD
          Assets:Account2     10 USD
          Assets:Account3     10 USD
          Assets:Account4     10 USD
          Assets:Account5     10 USD
          Assets:Account6     10 USD
          Assets:Other

        2016-02-09 balance Assets:Account1   0 USD
        2016-02-10 balance Assets:Account2   0 USD
        2016-02-15 balance Assets:Account3  10 USD
        2016-02-20 balance Assets:Account4  10 USD
        2016-02-21 balance Assets:Account5  20 USD
        """
        expect_entries, _, __ = parser.parse_string("""
        2016-02-10 *
          Assets:Account1   10 USD
            unverified: TRUE
          Assets:Account2   10 USD
            unverified: TRUE
          Assets:Account3   10 USD
          Assets:Account4   10 USD
          Assets:Account5   10 USD
          Assets:Account6   10 USD
          Assets:Other     -50 USD

        2016-02-20 *
          Assets:Account1   10 USD
            unverified: TRUE
          Assets:Account2   10 USD
            unverified: TRUE
          Assets:Account3   10 USD
            unverified: TRUE
          Assets:Account4   10 USD
            unverified: TRUE
          Assets:Account5   10 USD
          Assets:Account6   10 USD
          Assets:Other     -50 USD
        """)
        for expect_entry, entry in zip(expect_entries, data.filter_txns(entries)):
            for expect_posting, posting in zip(expect_entry.postings, entry.postings):
                self.assertEqual(expect_posting.meta.get('unverified'),
                                 posting.meta.get('unverified'))
