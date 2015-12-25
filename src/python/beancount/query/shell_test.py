__author__ = "Martin Blais <blais@furius.ca>"

import re
import sys
import unittest
from os import path

from beancount.utils import test_utils
from beancount.query import shell
from beancount import loader


# pylint: disable=invalid-name
entries, errors, options_map = None, None, None

def setUp(self):
    example_filename = path.join(test_utils.find_repository_root(__file__),
                                 'examples', 'example.beancount')
    global entries, errors, options_map  # pylint: disable=invalid-name
    entries, errors, options_map = loader.load(example_filename)
    assert not errors


class TestUseCases(unittest.TestCase):
    """Testing all the use cases from the proposal here.
    I'm hoping to replace reports by these queries instead."""

    def runshell(function):
        def test_function(self):
            def loadfun():
                return entries, errors, options_map
            with test_utils.capture('stdout') as stdout:
                shell_obj = shell.BQLShell(False, loadfun, sys.stdout)
                shell_obj.on_Reload()
                shell_obj.onecmd(function.__doc__)
            return function(self, stdout.getvalue())
        test_function.__name__ = function.__name__
        return test_function

    @runshell
    def test_print_from(self, output):
        """
        PRINT FROM narration ~ 'alone'
        """
        self.assertTrue(re.search('Eating out alone', output))

    @runshell
    def test_accounts(self, output):
        """
        SELECT DISTINCT account, open_date(account)
        ORDER BY account_sortkey(account);
        """
        self.assertRegexpMatches(output, 'Assets:US:BofA:Checking *2013-01-01')
        self.assertRegexpMatches(output, 'Equity:Opening-Balances *1980-05-12')
        self.assertRegexpMatches(output, 'Expenses:Financial:Commissions *1980-05-12')

    @runshell
    def test_commodities(self, output):
        """
        SELECT DISTINCT currency ORDER BY 1;
        """
        self.assertTrue(re.search('RGAGX', output))
        self.assertTrue(re.search('ITOT', output))
        self.assertTrue(re.search('USD', output))
        self.assertTrue(re.search('IRAUSD', output))

    @runshell
    def test_commodities_cost(self, output):
        """
        SELECT DISTINCT cost_currency ORDER BY 1;
        """
        self.assertTrue(re.search('USD', output))
        self.assertFalse(re.search('ITOT', output))
        self.assertFalse(re.search('IRAUSD', output))
        self.assertFalse(re.search('RGAGX', output))

    @runshell
    def test_commodities_pairs(self, output):
        """
        SELECT DISTINCT currency, cost_currency ORDER BY 1, 2;
        """
        self.assertTrue(re.search('GLD *USD', output))
        self.assertTrue(re.search('VEA *USD', output))
        self.assertTrue(re.search('VACHR', output))

    @runshell
    def test_balances(self, output):
        """
        BALANCES AT cost;
        """
        self.assertRegexpMatches(output, r'Liabilities:US:Chase:Slate *-\d+\.\d+ USD')

    @runshell
    def test_balances_with_where(self, output):
        """
        JOURNAL 'Vanguard:Cash';
        """
        self.assertRegexpMatches(output, 'Hoogle Payroll')

    @runshell
    def test_balance_sheet(self, output):
        """
        BALANCES AT cost
        FROM OPEN ON 2014-01-01 CLOSE ON 2015-01-01 CLEAR;
        """
        self.assertRegexpMatches(output, 'Assets:US:ETrade:Cash * \d+\.\d+ USD')

    @runshell
    def test_income_statement(self, output):
        """
        SELECT account, cost(sum(position))
        FROM OPEN ON 2014-01-01 CLOSE ON 2015-01-01
        WHERE account ~ '(Income|Expenses):*'
        GROUP BY account, account_sortkey(account)
        ORDER BY account_sortkey(account);
        """
        self.assertRegexpMatches(
            output, 'Expenses:Taxes:Y2014:US:Federal:PreTax401k *17500.00 IRAUSD')

    @runshell
    def test_journal(self, output):
        """
        JOURNAL 'Assets:US:BofA:Checking'
        FROM OPEN ON 2014-07-01 CLOSE ON 2014-10-01;
        """
        self.assertRegexpMatches(
            output, "2014-06-30 S *Opening balance for 'Assets:US:BofA:Checking'")
        self.assertRegexpMatches(
            output, "Transfering accumulated savings to other account")

    @runshell
    def test_conversions(self, output):
        """
        SELECT date, payee, narration, position, balance
        FROM OPEN ON 2014-07-01 CLOSE ON 2014-10-01
        WHERE flag = 'C'
        """
        self.assertRegexpMatches(output, "2014-09-30 *Conversion for")

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


__incomplete__ = True
