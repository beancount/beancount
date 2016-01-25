__author__ = "Martin Blais <blais@furius.ca>"

from beancount.utils import test_utils
from beancount.scripts import query


class TestScriptQuery(test_utils.TestCase):

    @test_utils.docfile
    def test_success(self, filename):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Equity:Unknown

        2013-04-05 *
          Equity:Unknown
          Assets:Account1     5000 USD

        2013-04-05 *
          Assets:Account1     -3000 USD
          Assets:Account2        30 BOOG {100 USD}

        2013-04-05 *
          Assets:Account1     -1000 USD
          Assets:Account3       800 EUR @ 1.25 USD
        """
        with test_utils.capture('stdout', 'stderr') as (stdout, _):
            test_utils.run_with_args(query.main, [filename, "SELECT 1;"])
        self.assertTrue(stdout.getvalue())
