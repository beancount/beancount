__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount import loader
from beancount.parser import cmptest


class TestIraContributions(cmptest.TestCase):

    @loader.load_doc()
    def test_ira_contribs(self, entries, errors, __):
        """
        plugin "beancount.plugins.implicit_prices"

        plugin "beancount.plugins.ira_contribs" "{
            'currency': 'IRAUSD',
            'flag': '&',
            'accounts': {
                'Income:US:Acme:Match401k': (
                    'Assets:US:Federal:Match401k',
                    'Expenses:Taxes:TY{year}:US:Federal:Match401k'),
                ('C', 'Assets:US:Fidelity:PreTax401k:Cash'): (
                    'Assets:US:Federal:PreTax401k',
                    'Expenses:Taxes:TY{year}:US:Federal:PreTax401k'),
             }
        }"

        2012-01-01 open Assets:US:BofA:Checking
        2012-01-01 open Assets:US:Federal:Match401k
        2012-01-01 open Assets:US:Federal:PreTax401k
        2012-01-01 open Assets:US:Fidelity:Match401k:SE2030
        2012-01-01 open Assets:US:Fidelity:PreTax401k:Cash
        2012-01-01 open Expenses:Taxes:TY2013:US:Federal
        2012-01-01 open Expenses:Taxes:TY2013:US:Federal:PreTax401k
        2012-01-01 open Expenses:Taxes:TY2013:US:Federal:Match401k
        2012-01-01 open Expenses:Taxes:TY2013:US:SocSec
        2012-01-01 open Expenses:Taxes:TY2013:US:StateNY
        2012-01-01 open Income:US:Acme:Match401k
        2012-01-01 open Income:US:Acme:Salary

        2013-02-15 * "ACME INC       PAYROLL"
          Income:US:Acme:Salary                          -4000.00 USD
          Assets:US:BofA:Checking                         1779.50 USD
          C Assets:US:Fidelity:PreTax401k:Cash             620.50 USD
          Expenses:Taxes:TY2013:US:Federal                 900.00 USD
          Expenses:Taxes:TY2013:US:StateNY                 400.00 USD
          Expenses:Taxes:TY2013:US:SocSec                  300.00 USD

        2013-02-17 * "BUYMF - MATCH" "Employer match, invested in SaveEasy 2030 fund"
          Assets:US:Fidelity:Match401k:SE2030              34.793 SE2030 {17.834 USD}
          Income:US:Acme:Match401k                        -620.50 USD

        2013-03-15 * "ACME INC       PAYROLL"
          Income:US:Acme:Salary                          -4000.00 USD
          Assets:US:BofA:Checking                         1779.50 USD
          Assets:US:Fidelity:PreTax401k:Cash               620.50 USD
          Expenses:Taxes:TY2013:US:Federal                 900.00 USD
          Expenses:Taxes:TY2013:US:StateNY                 400.00 USD
          Expenses:Taxes:TY2013:US:SocSec                  300.00 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

        2012-01-01 open Assets:US:BofA:Checking
        2012-01-01 open Assets:US:Federal:Match401k
        2012-01-01 open Assets:US:Federal:PreTax401k
        2012-01-01 open Assets:US:Fidelity:Match401k:SE2030
        2012-01-01 open Assets:US:Fidelity:PreTax401k:Cash
        2012-01-01 open Expenses:Taxes:TY2013:US:Federal
        2012-01-01 open Expenses:Taxes:TY2013:US:Federal:PreTax401k
        2012-01-01 open Expenses:Taxes:TY2013:US:Federal:Match401k
        2012-01-01 open Expenses:Taxes:TY2013:US:SocSec
        2012-01-01 open Expenses:Taxes:TY2013:US:StateNY
        2012-01-01 open Income:US:Acme:Match401k
        2012-01-01 open Income:US:Acme:Salary

        2013-02-15 * "ACME INC       PAYROLL"
          Income:US:Acme:Salary                          -4000.00 USD
          Assets:US:BofA:Checking                         1779.50 USD
          C Assets:US:Fidelity:PreTax401k:Cash             620.50 USD
          Expenses:Taxes:TY2013:US:Federal                 900.00 USD
          Expenses:Taxes:TY2013:US:StateNY                 400.00 USD
          Expenses:Taxes:TY2013:US:SocSec                  300.00 USD
          & Assets:US:Federal:PreTax401k                    -620.50 IRAUSD
          & Expenses:Taxes:TY2013:US:Federal:PreTax401k      620.50 IRAUSD

        2013-02-17 * "BUYMF - MATCH" "Employer match, invested in SaveEasy 2030 fund"
          Assets:US:Fidelity:Match401k:SE2030              34.793 SE2030 {17.834 USD}
          Income:US:Acme:Match401k                        -620.50 USD
          & Assets:US:Federal:Match401k                     -620.50 IRAUSD
          & Expenses:Taxes:TY2013:US:Federal:Match401k       620.50 IRAUSD

        2013-03-15 * "ACME INC       PAYROLL"
          Income:US:Acme:Salary                          -4000.00 USD
          Assets:US:BofA:Checking                         1779.50 USD
          Assets:US:Fidelity:PreTax401k:Cash               620.50 USD
          Expenses:Taxes:TY2013:US:Federal                 900.00 USD
          Expenses:Taxes:TY2013:US:StateNY                 400.00 USD
          Expenses:Taxes:TY2013:US:SocSec                  300.00 USD

        2013-02-17 price SE2030                             17.834 USD

        """, entries)
