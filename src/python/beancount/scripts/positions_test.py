from beancount.scripts import TestCase, docfile, capture, run_with_args
from beancount.scripts import positions


class TestScriptPositions(TestCase):

    @docfile
    def test_success(self, filename):
        """
        2014-01-01 open Assets:Bank
        2014-01-01 open Assets:Ameritrade
        2014-01-01 open Assets:ETrade
        2014-01-01 open Assets:Vanguard
        2014-01-01 open Equity:Unknown

        2014-02-01 * "Enter some hypothetical positions"
          Assets:Bank                   29765.92 USD
          Assets:Ameritrade             1100 GOOG {567.01 USD}
          Assets:ETrade                  760 AAPL {110.22 USD}
          Assets:Vanguard                201 RGAGX {17.84 USD}
          Equity:Unknown
        """
        with capture() as stdout:
            run_with_args(positions.main, [filename])
        print(stdout.getvalue())
        #r = self.assertLines("", stdout.getvalue())
