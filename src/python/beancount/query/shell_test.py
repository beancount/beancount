import datetime
import re
import io
import unittest
import functools
from os import path

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.query import query_parser
from beancount.query import query_compile as qc
from beancount.query import query_env as qe
from beancount.query import query_execute as qx
from beancount.parser import cmptest
from beancount.parser import parser
from beancount.utils import misc_utils
from beancount.utils import test_utils
from beancount.query import shell
from beancount import loader


def setUp(self):
    example_filename = path.join(test_utils.find_repository_root(__file__),
                                 'examples', 'tutorial', 'example.beancount')
    global entries, errors, options_map
    entries, errors, options_map = loader.load(example_filename)
    assert not errors


class TestUseCases(unittest.TestCase):
    """Testing all the use cases from the proposal here.
    I'm hoping to replace reports by these queries instead."""

    def runshell(function):
        def test_function(self):
            with test_utils.capture('stdout') as stdout:
                shell_obj = shell.BQLShell(False, entries, errors, options_map)
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
        self.assertTrue(re.search('Assets:US:BofA:Checking *2012-01-01', output))
        self.assertTrue(re.search('Equity:Opening-Balances *1980-05-12', output))
        self.assertTrue(re.search('Expenses:Financial:Commissions *1980-05-12', output))

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
        self.assertTrue(re.search('Liabilities:AccountsPayable *-3664.80 USD', output))

    @runshell
    def test_balances_with_where(self, output):
        """
        JOURNAL 'Vanguard:Cash';
        """
        self.assertTrue(re.search('Hoogle Payroll', output))

    @runshell
    def test_balance_sheet(self, output):
        """
        BALANCES AT cost
        FROM OPEN ON 2014-01-01 CLOSE ON 2015-01-01 CLEAR;
        """
        self.assertTrue(re.search('Assets:US:ETrade:Cash *9692.61 USD', output))

    @runshell
    def test_income_statement(self, output):
        """
        SELECT account, cost(sum(change))
        FROM OPEN ON 2014-01-01 CLOSE ON 2015-01-01
        WHERE account ~ '(Income|Expenses):*'
        GROUP BY account, account_sortkey(account)
        ORDER BY account_sortkey(account);
        """
        self.assertTrue(re.search(
            'Expenses:Taxes:Y2014:US:Federal:PreTax401k *17500.00 IRAUSD', output))

    @runshell
    def test_journal(self, output):
        """
        JOURNAL 'Assets:US:BofA:Checking'
        FROM OPEN ON 2014-07-01 CLOSE ON 2014-10-01;
        """
        self.assertTrue(re.search(
            "2014-06-30 S *Opening balance for 'Assets:US:BofA:Checking'", output))
        self.assertTrue(re.search(
            "Transfering accumulated savings to other account", output))

    @runshell
    def test_conversions(self, output):
        """
        SELECT date, payee, narration, change, balance
        FROM OPEN ON 2014-07-01 CLOSE ON 2014-10-01
        WHERE flag = 'C'
        """
        self.assertTrue(re.search("2014-09-30 *Conversion for", output))

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
        SELECT account, currency, cost_currency, sum(change)
        GROUP BY account, currency, cost_currency;
        """
        ## FIXME: Here we need to finally support FLATTEN to make this happen properly.







__incomplete__ = True
