import re

from beancount.utils import test_utils
from beancount.scripts import query


class TestScriptCheck(test_utils.TestCase):

    @test_utils.docfile
    def test_success(self, filename):
        """

          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash         USD,CAD

          2014-02-15 price GOOG 500.00 USD

          2014-03-02 * "Something"
            Expenses:Restaurant   50.02 USD
            Assets:Cash

        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(query.main, [filename, 'ledger'])
        self.assertEqual(0, result)
        self.assertLines("""

          account Expenses:Restaurant

          account Assets:Cash
            assert commodity = "USD" | commodity = "CAD"

          P 2014/02/15 00:00:00 GOOG                   500.00 USD

          2014/03/02 * Something
            Expenses:Restaurant                                                     50.02 USD
            Assets:Cash                                                            -50.02 USD

        """, stdout.getvalue())
