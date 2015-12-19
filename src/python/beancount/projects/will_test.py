__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import re
import logging
import textwrap
import sys
import subprocess
from os import path
from unittest import mock
import pprint

from beancount import loader
from beancount.core import inventory
from beancount.core import getters
from beancount.ops import prices
from beancount.parser import cmptest
from beancount.parser import options
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
        groups = will.group_accounts_by_metadata(accounts_map, "institution")
        self.assertEqual({
            'Chase Manhattan Bank Checking Division': ['Assets:US:Chase:Checking'],
            'Chase Manhattan Bank.': ['Assets:US:Chase'],
            'Toronto Dominion Bank.': ['Assets:US:TDBank:Checking',
                                       'Liabilities:US:TDBank:CreditCard'],
            'Wells Fargo Bank.': ['Assets:US:WellsFargo',
                                  'Assets:US:WellsFargo:Checking']},
                         groups)


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
        groups = will.find_institutions(entries, options_map)
        self.assertEqual({}, groups)


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
    def test_group_accounts(self, entries, _, options_map):
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
        report = will.create_report(entries, options_map)
        pprint.pprint(report)
