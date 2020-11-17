__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import logging
import unittest

from beancount import loader
from beancount.core import getters
from beancount.projects import will
from beancount.utils import test_utils


def setUp(self):
    logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')


class TestWillFunctions(test_utils.TestCase):

    maxDiff = None

    @loader.load_doc()
    def test_group_accounts(self, entries, _, __):
        """
          ;; Absence of meta.
          2010-01-01 open Assets:US:BofA:Checking

          ;; Metadata on the parent.
          2010-01-01 open Assets:US:WellsFargo
            institution: "Wells Fargo Bank."
          2010-01-01 open Assets:US:WellsFargo:Checking

          ;; Ambiguous metadata.
          2010-01-01 open Assets:US:Chase
            institution: "Chase Manhattan Bank."
          2010-01-01 open Assets:US:Chase:Checking
            institution: "Chase Manhattan Bank Checking Division"

          ;; Two accounts joined by same institution.
          2010-01-01 open Assets:US:TDBank:Checking
            institution: "Toronto Dominion Bank."
          2010-01-01 open Liabilities:US:TDBank:CreditCard
            institution: "Toronto Dominion Bank."
        """
        open_close_map = getters.get_account_open_close(entries)
        accounts_map = {acc: open_entry
                        for acc, (open_entry, _) in open_close_map.items()}
        groups, ignored = will.group_accounts_by_metadata(accounts_map, "institution")
        self.assertEqual({
            'Chase Manhattan Bank Checking Division': ['Assets:US:Chase:Checking'],
            'Chase Manhattan Bank.': ['Assets:US:Chase'],
            'Toronto Dominion Bank.': ['Assets:US:TDBank:Checking',
                                       'Liabilities:US:TDBank:CreditCard'],
            'Wells Fargo Bank.': ['Assets:US:WellsFargo',
                                  'Assets:US:WellsFargo:Checking']},
                         groups)
        self.assertEqual({'Assets:US:BofA:Checking'}, ignored)

    @loader.load_doc()
    def test_find_institutions(self, entries, _, options_map):
        """
          ;; Closed account.
          2010-01-01 open  Assets:US:BofA:Checking
            institution: "Bank of America"

          2015-01-01 close Assets:US:BofA:Checking

          ;; Income statement account.
          2010-01-01 open Expenses:US:BofA:Fees
            institution: "Bank of America"
        """
        groups, ignored = will.find_institutions(entries, options_map)
        self.assertEqual({}, groups)
        self.assertEqual(set(), ignored)

    @loader.load_doc()
    def test_get_first_meta(self, entries, _, options_map):
        """
          2010-01-01 open  Assets:Something1
            animal: "Ape"

          2010-01-02 open  Assets:Something2
            animal: "Bonobo"
            food: "Banana"

          2010-01-03 open  Assets:Something3
            apple: "Bank of America"
            food: "Coconut"
            habitat: "Jungle"
        """
        self.assertEqual("Ape", will.get_first_meta(entries, 'animal'))
        self.assertEqual("Banana", will.get_first_meta(entries, 'food'))
        self.assertEqual("Jungle", will.get_first_meta(entries, 'habitat'))


class TestWillReport(test_utils.TestCase):

    @loader.load_doc()
    def test_create_report(self, entries, _, options_map):
        """
          option "title" "Report Creation Test"

          ;; Two accounts in the same institution.
          2010-01-01 open Assets:US:BofA
            institution: "Bank of America"
            address: "100 North Tryon Street,  Charlotte, NC 28255"
            phone: "1.800.933.6262"
            website: "https://www.bankofamerica.com"

          2010-01-01 open Assets:US:BofA:Checking
            type: "Checking Account"
            number: "43865450874"

          2010-01-01 open Assets:US:BofA:Savings
            type: "Savings Account"
            number: "83470650273"

          ;; An an account with a zero balance.
          2010-01-01 open Liabilities:US:BofA:CreditCard
            institution: "Bank of America"
            number: "3478.4744.2339.0011"

          ;; With a non-zeo liabilities account.
          2010-01-01 open Liabilities:US:Chase:CreditCard
            institution: "Chase Manhattan Bank"
            address: "National Bank By Mail, P O Box 36520, Louisville, KY 40233-6520"
            number: "7654.0754.9375.0489"

          2010-01-01 open Income:Misc

          2014-02-03 *
            Assets:US:BofA:Checking          3400.00 USD
            Assets:US:BofA:Savings           1200.00 USD
            Liabilities:US:Chase:CreditCard  -820.00 USD
            Income:Misc

        """
        report = will.create_report(entries, options_map)
        text = will.format_xhtml_report(report, options_map)
        with open('/tmp/index.html', 'w') as infile:
            pass # print(text, file=infile)


if __name__ == '__main__':
    unittest.main()
