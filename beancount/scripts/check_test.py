__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.utils import test_utils
from beancount.scripts import check


class TestScriptCheck(test_utils.TestCase):

    @test_utils.docfile
    def test_success(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.capture('stdout', 'stderr') as (stdout, _):
            result = test_utils.run_with_args(check.main, [filename])
        self.assertEqual(0, result)
        self.assertLines("", stdout.getvalue())

    @test_utils.docfile
    def test_fail(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash

        2014-03-07 balance Assets:Cash  100 USD
        """
        with test_utils.capture('stderr') as stderr:
            result = test_utils.run_with_args(check.main, [filename])
        self.assertEqual(1, result)
        self.assertRegex(stderr.getvalue(), "Balance failed")
        self.assertRegex(stderr.getvalue(), "Assets:Cash")


if __name__ == '__main__':
    unittest.main()
