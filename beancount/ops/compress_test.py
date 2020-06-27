__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.parser import cmptest
from beancount.core import data
from beancount.ops import compress


class TestMerge(cmptest.TestCase):

    @loader.load_doc()
    def test_merge(self, entries, errors, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2011-05-17 * "Something"
              Expenses:Restaurant   1.11 USD
              Assets:Other

            2011-05-18 * "Something Else"
              Expenses:Restaurant   1.22 USD
              Assets:Other
        """
        txn = next(data.filter_txns(entries))
        merged_entry = compress.merge(entries, txn)
        self.assertEqualEntries("""

            2011-05-17 * "Something"
              Expenses:Restaurant   2.33 USD
              Assets:Other         -2.33 USD

        """, [merged_entry])

    @loader.load_doc()
    def test_merge_cost(self, entries, errors, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2011-05-17 * "Something"
              Expenses:Restaurant   1.11 USD {100 ECUS, 2011-02-01}
              Assets:Other

            2011-05-18 * "Something Else"
              Expenses:Restaurant   1.22 USD {100 ECUS, 2011-02-01}
              Assets:Other
        """
        txn = next(data.filter_txns(entries))
        merged_entry = compress.merge(entries, txn)
        self.assertEqualEntries("""

            2011-05-17 * "Something"
              Expenses:Restaurant   2.33 USD {100 ECUS, 2011-02-01}
              Assets:Other       -233.00 ECUS

        """, [merged_entry])

    @loader.load_doc()
    def test_merge_price(self, entries, errors, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2011-05-17 * "Something"
              Expenses:Restaurant   1.11 USD @ 100 ECUS
              Assets:Other

            2011-05-18 * "Something Else"
              Expenses:Restaurant   1.22 USD @ 100 ECUS
              Assets:Other
        """
        txn = next(data.filter_txns(entries))
        merged_entry = compress.merge(entries, txn)
        self.assertEqualEntries("""

            2011-05-17 * "Something"
              Expenses:Restaurant   2.33 USD @ 100 ECUS
              Assets:Other       -233.00 ECUS

        """, [merged_entry])

    @loader.load_doc()
    def test_unmergeable(self, entries, errors, __):
        """
            plugin "beancount.plugins.auto_accounts"

            2015-01-01 *
              Expenses:Restaurant    1.11 USD
              Equity:Other

            2015-01-02 * "Account"
              Expenses:Grocery       1.11 USD
              Equity:Other

            2015-01-03 * "Currency"
              Expenses:Restaurant    1.11 CAD
              Equity:Other

            2015-01-04 * "Cost"
              Expenses:Restaurant    1.11 USD {5 ECUS}
              Equity:Other

            2015-01-05 * "Price"
              Expenses:Restaurant    1.11 USD @ 10 ECUS
              Equity:Other

            2015-01-05 * "Flag"
              ! Expenses:Restaurant  1.11 USD
              Equity:Other

        """
        txn = next(data.filter_txns(entries))
        merged_entry = compress.merge(entries, txn)

        self.assertEqualEntries("""

          2015-01-01 *
            Equity:Other            -1.11 CAD
            Equity:Other           -16.65 ECUS
            Equity:Other            -3.33 USD
            Expenses:Grocery         1.11 USD
            Expenses:Restaurant      1.11 CAD
            Expenses:Restaurant      1.11 USD @ 10 ECUS
            Expenses:Restaurant      1.11 USD {5 ECUS, 2015-01-04}
            Expenses:Restaurant      1.11 USD
            ! Expenses:Restaurant    1.11 USD

        """, [merged_entry])


if __name__ == '__main__':
    unittest.main()
