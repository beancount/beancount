__copyright__ = "Copyright (C) 2014, 2016-2017, 2019, 2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

import click.testing

from beancount.scripts import check
from beancount.utils import test_utils


class TestScriptCheck(test_utils.ClickTestCase):
    @test_utils.docfile
    def test_success(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        result = self.run_with_args(check.main, filename)
        self.assertLines("", result.stdout)

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
        # We should use mix_stderr=False and we check for the error
        # message on result.stderr, but it does not work. See
        # https://github.com/pallets/click/issues/1761
        runner = click.testing.CliRunner()
        result = runner.invoke(check.main, [filename])
        self.assertEqual(result.exit_code, 1)
        self.assertRegex(result.output, "Balance failed")
        self.assertRegex(result.output, "Assets:Cash")

    @test_utils.docfile
    def test_auto_plugins(self, filename):
        """
        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        # We should use mix_stderr=False and we check for the error
        # message on result.stderr, but it does not work. See
        # https://github.com/pallets/click/issues/1761
        runner = click.testing.CliRunner()
        result = runner.invoke(check.main, ["--auto", filename])
        self.assertEqual(result.exit_code, 0)
        self.assertEqual("", result.output.strip())


if __name__ == "__main__":
    unittest.main()
