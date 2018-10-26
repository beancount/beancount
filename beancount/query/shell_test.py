__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import re
import sys
import unittest
from os import path

from beancount.utils import test_utils
from beancount.query import shell
from beancount import loader


# pylint: disable=invalid-name
entries, errors, options_map = None, None, None


def setup_module():
    example_filename = path.join(test_utils.find_repository_root(__file__),
                                 'examples', 'example.beancount')
    global entries, errors, options_map  # pylint: disable=invalid-name
    entries, errors, options_map = loader.load_file(example_filename)
    assert not errors


def runshell(function):
    """Decorate a function to run the shell and return the output."""
    def test_function(self):
        def loadfun():
            return entries, errors, options_map
        with test_utils.capture('stdout') as stdout:
            shell_obj = shell.BQLShell(False, loadfun, sys.stdout)
            shell_obj.on_Reload()
            shell_obj.onecmd(function.__doc__)
        return function(self, stdout.getvalue())
    return test_function


class TestUseCases(unittest.TestCase):
    """Testing all the use cases from the proposal here.
    I'm hoping to replace reports by these queries instead."""

    @runshell
    def test_print_from(self, output):
        """
        PRINT FROM narration ~ 'alone'
        """
        self.assertRegex(output, 'Eating out alone')

    @runshell
    def test_accounts(self, output):
        """
        SELECT DISTINCT account, open_date(account)
        ORDER BY account_sortkey(account);
        """
        self.assertRegex(output, 'Assets:US:BofA:Checking *2013-01-01')
        self.assertRegex(output, 'Equity:Opening-Balances *1980-05-12')
        self.assertRegex(output, 'Expenses:Financial:Commissions *1980-05-12')

    @runshell
    def test_commodities(self, output):
        """
        SELECT DISTINCT currency ORDER BY 1;
        """
        self.assertRegex(output, 'RGAGX')
        self.assertRegex(output, 'ITOT')
        self.assertRegex(output, 'USD')
        self.assertRegex(output, 'IRAUSD')

    @runshell
    def test_commodities_cost(self, output):
        """
        SELECT DISTINCT cost_currency ORDER BY 1;
        """
        self.assertRegex(output, 'USD')
        self.assertNotRegex(output, 'ITOT')
        self.assertNotRegex(output, 'IRAUSD')
        self.assertNotRegex(output, 'RGAGX')

    @runshell
    def test_commodities_pairs(self, output):
        """
        SELECT DISTINCT currency, cost_currency ORDER BY 1, 2;
        """
        self.assertRegex(output, 'GLD *USD')
        self.assertRegex(output, 'VEA *USD')
        self.assertRegex(output, 'VACHR')

    @runshell
    def test_balances(self, output):
        """
        BALANCES AT cost;
        """
        self.assertRegex(output, r'Liabilities:US:Chase:Slate *-\d+\.\d+ USD')

    @runshell
    def test_balances_with_where(self, output):
        """
        JOURNAL 'Vanguard:Cash';
        """
        self.assertRegex(output, 'Hoogle Payroll')

    @runshell
    def test_balance_sheet(self, output):
        """
        BALANCES AT cost
        FROM OPEN ON 2014-01-01 CLOSE ON 2015-01-01 CLEAR;
        """
        self.assertRegex(output, r'Assets:US:ETrade:Cash * \d+\.\d+ USD')

    @runshell
    def test_income_statement(self, output):
        """
        SELECT account, cost(sum(position))
        FROM OPEN ON 2014-01-01 CLOSE ON 2015-01-01
        WHERE account ~ '(Income|Expenses):*'
        GROUP BY account, account_sortkey(account)
        ORDER BY account_sortkey(account);
        """
        self.assertRegex(
            output, 'Expenses:Taxes:Y2014:US:Federal:PreTax401k *17500.00 IRAUSD')

    @runshell
    def test_journal(self, output):
        """
        JOURNAL 'Assets:US:BofA:Checking'
        FROM OPEN ON 2014-07-01 CLOSE ON 2014-10-01;
        """
        self.assertRegex(
            output, "2014-06-30 S *Opening balance for 'Assets:US:BofA:Checking'")
        self.assertRegex(
            output, "Transfering accumulated savings to other account")

    @runshell
    def test_conversions(self, output):
        """
        SELECT date, payee, narration, position, balance
        FROM OPEN ON 2014-07-01 CLOSE ON 2014-10-01
        WHERE flag = 'C'
        """
        self.assertRegex(output, "2014-09-30 *Conversion for")

    @runshell
    def test_documents(self, output):
        """
        SELECT date, account, narration
        WHERE type = 'Document';
        """
        ## FIXME: Make this possible, we need an example with document entries.

    @runshell
    def test_holdings(self, output):
        """
        SELECT account, currency, cost_currency, sum(position)
        GROUP BY account, currency, cost_currency;
        """
        ## FIXME: Here we need to finally support FLATTEN to make this happen properly.


class TestRun(unittest.TestCase):

    @runshell
    def test_run_custom__list(self, output):
        """
        RUN
        """
        self.assertEqual("home taxes",
                         re.sub(r'[] \n\t]+', ' ', output).strip())

    @runshell
    def test_run_custom__query_not_exists(self, output):
        """
        RUN something
        """
        self.assertEqual("ERROR: Query 'something' not found", output.strip())

    @runshell
    def test_run_custom__query_id(self, output):
        """
        RUN taxes
        """
        self.assertRegex(output, 'date +description +position +balance')
        self.assertRegex(output, r'Hoogle \| Payroll')

    @runshell
    def test_run_custom__query_string(self, output):
        """
        RUN "taxes"
        """
        self.assertRegex(output, 'date +description +position +balance')
        self.assertRegex(output, r'Hoogle \| Payroll')

    @runshell
    def test_run_custom__all(self, output):
        """
        RUN *
        """
        self.assertRegex(output, 'date +description +position +balance')
        self.assertRegex(output, r'Hoogle \| Payroll')
        self.assertRegex(output, 'account +total')
        self.assertRegex(output, 'Expenses:Home:Rent')


class TestShell(test_utils.TestCase):

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
            test_utils.run_with_args(shell.main, [filename, "SELECT 1;"])
        self.assertTrue(stdout.getvalue())


__incomplete__ = True
