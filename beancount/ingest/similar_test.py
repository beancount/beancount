__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.core.number import D
from beancount.core import data
from beancount.parser import cmptest
from beancount.parser import parser
from beancount import loader
from beancount.ingest import similar


class TestDups(cmptest.TestCase):

    @loader.load_doc()
    def test_find_similar_entries(self, entries, _, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2016-01-03 *
              Expenses:Tips         1.03 USD
              Assets:Other

            2016-01-04 *
              Expenses:Coffee       1.04 USD
              Assets:Other

            2016-01-05 *
              Expenses:Restaurant   1.05 USD
              Assets:Other

            2016-01-06 *
              Expenses:Groceries    1.06 USD
              Assets:Other

            2016-01-07 *
              Expenses:Alcohol      1.07 USD
              Assets:Other

            2016-01-08 *
              Expenses:Smoking      1.08 USD
              Assets:Other

            2016-01-09 *
              Expenses:Taxi         1.09 USD
              Assets:Other
        """
        new_entries, _, __ = loader.load_string("""
            plugin "beancount.plugins.auto_accounts"

            2016-01-06 *
              Expenses:Groceries    1.06 USD
              Assets:Other
        """)
        for days, num_comparisons in [(0, 1), (1, 1), (2, 1)]:
            duplicates = similar.find_similar_entries(new_entries, entries,
                                                      lambda e1, e2: True,
                                                      window_days=days)
            self.assertEqual(num_comparisons, len(duplicates))

            duplicates = similar.find_similar_entries(new_entries, entries,
                                                      lambda e1, e2: False,
                                                      window_days=days)
            self.assertEqual(0, len(duplicates))

    @loader.load_doc()
    def test_find_similar_entries__multiple_matches(self, entries, _, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2016-02-01 * "A"
              Assets:Account1    10.00 USD
              Assets:Account2   -10.00 USD

            2016-02-02 * "B"
              Assets:Account1    10.00 USD
              Assets:Account2   -10.00 USD

            2016-02-03 * "C"
              Assets:Account1    10.00 USD
              Assets:Account2   -10.00 USD

            2016-02-04 * "D"
              Assets:Account1    10.00 USD
              Assets:Account2   -10.00 USD

            2016-02-05 * "D"
              Assets:Account1    10.00 USD
              Assets:Account2   -10.00 USD
        """
        # Test it with a single entry.
        new_entries = list(data.filter_txns(entries))[2:3]
        duplicates = similar.find_similar_entries(new_entries, entries, window_days=1)
        self.assertEqual(1, len(duplicates))
        self.assertEqual(new_entries[0], duplicates[0][0])

        # Test it with multiple duplicate entries.
        new_entries = list(data.filter_txns(entries))[1:4]
        duplicates = similar.find_similar_entries(new_entries, entries, window_days=1)
        self.assertEqual(len(new_entries), len(duplicates))

    @parser.parse_doc(allow_incomplete=True)
    def test_amounts_map(self, entries, _, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2016-01-03 *
              Expenses:Alcohol     20.00 USD
              Expenses:Tips         1.03 USD
              Assets:Other

            2016-01-03 *
              Expenses:Tips         1.01 USD
              Expenses:Tips         1.02 USD
              Assets:Other
        """
        txns = list(data.filter_txns(entries))
        amap = similar.amounts_map(txns[0])
        self.assertEqual({('Expenses:Tips', 'USD'): D('1.03'),
                          ('Expenses:Alcohol', 'USD'): D('20.00')}, amap)

        amap = similar.amounts_map(txns[1])
        self.assertEqual({('Expenses:Tips', 'USD'): D('2.03')}, amap)


class TestSimilarityComparator(cmptest.TestCase):

    def setUp(self):
        self.comparator = similar.SimilarityComparator(datetime.timedelta(days=2))

    @loader.load_doc()
    def test_simple(self, entries, _, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2016-01-03 * "Base reservation" ^base
              Expenses:Alcohol     20.00 USD
              Expenses:Tips         1.03 USD
              Assets:Other

            2016-01-03 * "Similar amount within bounds" ^in-bounds
              Expenses:Alcohol     20.99 USD
              Assets:Other
            2016-01-03 * "Similar amount out of bounds" ^out-bounds
              Expenses:Alcohol     21.00 USD
              Assets:Other

            2016-01-06 * "Date too far" ^too-late
              Expenses:Alcohol     20.00 USD
              Expenses:Tips         1.03 USD
              Assets:Other

            2016-01-03 * "Non-overlapping accounts" ^non-accounts
              Expenses:Alcohol     20.00 USD
              Expenses:Tips         1.03 USD
              Assets:SomethingElse
        """
        txns = list(data.filter_txns(entries))

        def compare(expected, link1, link2):
            self.assertEqual(expected, self.comparator(
                next(txn for txn in txns if link1 in txn.links),
                next(txn for txn in txns if link2 in txn.links)))

        compare(True, 'base', 'base')
        compare(True, 'base', 'in-bounds')
        compare(False, 'base', 'out-bounds')
        compare(False, 'base', 'too-late')
        compare(False, 'base', 'non-accounts')


if __name__ == '__main__':
    unittest.main()
