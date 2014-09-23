import re
import textwrap

from beancount.utils import test_utils
from beancount.scripts import format


class TestScriptFormat(test_utils.TestCase):

    @test_utils.docfile
    def test_success(self, filename):
        """

          * Section header

          ;; Accounts (comments)
          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant   50.02 USD
            Assets:Cash

          2014-03-05 balance   Assets:Cash  -50.02 USD

          2014-03-10 * "Something"
            Assets:Other   10 GOOG {500.23} USD ; Bla
            Assets:Cash

        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(format.main, [filename])
        self.assertEqual(0, result)
        self.assertEqual(textwrap.dedent("""

          * Section header

          ;; Accounts (comments)
          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant              50.02 USD
            Assets:Cash

          2014-03-05 balance   Assets:Cash  -50.02 USD

          2014-03-10 * "Something"
            Assets:Other                        10 GOOG {500.23} USD ; Bla
            Assets:Cash

        """), stdout.getvalue())
