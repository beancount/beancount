__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import re
import logging
import textwrap
from unittest import mock

from beancount.utils import test_utils
from beancount.projects import returns
from beancount.parser import options
from beancount.ops import prices
from beancount.core import data
from beancount.core import inventory
from beancount import loader


def setUp(self):
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')


class TestReturnsFunctions(test_utils.TestCase):

    INPUT = """

    2000-01-31 open Assets:US:TD:Checking                 USD
    2000-01-31 open Equity:Opening-Balances               USD,LENCLUB

    2013-06-10 open Assets:US:Prosper:Cash            USD
    2013-06-10 open Assets:US:Prosper:InFunding       USD
    2013-06-10 open Assets:US:Prosper:FundsLent       LENCLUB
    2013-06-10 open Income:US:Prosper:LoanInterest    USD
    2013-06-10 open Income:US:Prosper:ChargedOff      USD
    2013-06-10 open Income:US:Prosper:LateFeesRecv    USD
    2013-06-10 open Expenses:Financial:Fees:Prosper   USD


    2014-06-29 P "Opening balances"
      Assets:US:Prosper:Cash            802.30 USD
      Assets:US:Prosper:FundsLent     19954.84 LENCLUB {1 USD}
      Equity:Opening-Balances


    2014-06-30 balance Assets:US:Prosper:Cash            802.30 USD
    2014-06-30 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-06-30 balance Assets:US:Prosper:FundsLent     19954.84 LENCLUB



    2014-07-01 * "Monthly_Statement"
      Assets:US:Prosper:InFunding                               2575.00 USD
      Assets:US:Prosper:FundsLent                               1825.00 LENCLUB {1 USD}
      Assets:US:Prosper:FundsLent                               -774.05 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                     774.05 USD
      Income:US:Prosper:LoanInterest                            -184.55 USD
      Expenses:Financial:Fees:Prosper                              9.59 USD
      Assets:US:Prosper:Cash

    2014-07-30 balance Assets:US:Prosper:Cash           7351.31 USD
    2014-07-30 balance Assets:US:Prosper:InFunding      2575.00 USD
    2014-07-30 balance Assets:US:Prosper:FundsLent     21005.79 LENCLUB


    2014-07-10 * "PROSPER" | "Transfer money into it."
      Assets:US:TD:Checking                                               -10000.00 USD
      Assets:US:Prosper:Cash


    2014-07-31 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                               9325.00 LENCLUB {1 USD}
      Assets:US:Prosper:FundsLent                               -807.23 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                     807.23 USD
      Income:US:Prosper:LoanInterest                            -190.23 USD
      Income:US:Prosper:LateFeesRecv                              -0.04 USD
      Expenses:Financial:Fees:Prosper                              9.97 USD
      Assets:US:Prosper:InFunding                              -1775.00 USD
      Assets:US:Prosper:Cash

    2014-08-30 balance Assets:US:Prosper:Cash            788.84 USD
    2014-08-30 balance Assets:US:Prosper:InFunding       800.00 USD
    2014-08-30 balance Assets:US:Prosper:FundsLent     29523.56 LENCLUB


    2014-08-31 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                               1400.00 LENCLUB {1 USD}
      Assets:US:Prosper:FundsLent                              -1128.78 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                    1128.78 USD
      Income:US:Prosper:LoanInterest                            -257.59 USD
      Expenses:Financial:Fees:Prosper                             13.86 USD
      Assets:US:Prosper:InFunding                               -800.00 USD
      Assets:US:Prosper:Cash
      Assets:US:Prosper:FundsLent                                -21.61 LENCLUB {1 USD}
      Income:US:Prosper:ChargedOff                                21.61 USD

    2014-09-30 balance Assets:US:Prosper:Cash           1561.35 USD
    2014-09-30 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-09-30 balance Assets:US:Prosper:FundsLent     29773.17 LENCLUB


    2014-10-01 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                              -1227.38 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                    1227.37 USD
      Income:US:Prosper:LoanInterest                            -263.13 USD
      Income:US:Prosper:LateFeesRecv                              -0.08 USD
      Expenses:Financial:Fees:Prosper                             14.90 USD
      Assets:US:Prosper:Cash

    2014-10-31 balance Assets:US:Prosper:Cash           3037.04 USD
    2014-10-31 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-10-31 balance Assets:US:Prosper:FundsLent     28545.79 LENCLUB


    2014-11-01 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                              -1063.20 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                    1063.20 USD
      Income:US:Prosper:LoanInterest                            -277.26 USD
      Income:US:Prosper:LateFeesRecv                              -0.08 USD
      Expenses:Financial:Fees:Prosper                             13.15 USD
      Expenses:Financial:Fees:Prosper                              0.42 USD ;; Collection fees
      Assets:US:Prosper:Cash

    2014-11-06 * "PROSPER" | "Withdrawal"
      Assets:US:TD:Checking                                                 3200.00 USD
      Assets:US:Prosper:Cash


    2014-11-30 balance Assets:US:Prosper:Cash           1164.01 USD
    2014-11-30 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-11-30 balance Assets:US:Prosper:FundsLent     27482.59 LENCLUB


    2014-12-04 * "PROSPER"
      Assets:US:TD:Checking                                                       1180.00 USD
      Assets:US:Prosper:Cash

    """

    def setUp(self):
        self.entries, errors, self.options_map = loader.load_string(self.INPUT)
        self.assertFalse(errors)
        self.acc_types = options.get_account_types(self.options_map)

    def test_find_matching(self, ):
        matching_entries, (acc_assets,
                           acc_intflows,
                           acc_extflows) = returns.find_matching(
                               self.entries, self.acc_types, '.*:Prosper')

        self.assertEqual({'Assets:US:Prosper:InFunding',
                          'Assets:US:Prosper:Cash',
                          'Assets:US:Prosper:FundsLent'}, acc_assets)

        self.assertEqual({'Income:US:Prosper:LateFeesRecv',
                          'Income:US:Prosper:ChargedOff',
                          'Expenses:Financial:Fees:Prosper',
                          'Income:US:Prosper:LoanInterest'}, acc_intflows)

        self.assertEqual({'Equity:Opening-Balances',
                          'Assets:US:TD:Checking'},
                         acc_extflows)

    @mock.patch('beancount.projects.returns.compute_returns')
    def test_compute_returns_with_regexp(self, mock_obj):
        returns.compute_returns_with_regexp(self.entries, self.options_map, '.*:Prosper')
        self.assertTrue([list, dict, set, set], map(type, mock_obj.call_args))
        self.assertTrue(mock_obj.called)

    def test_compute_returns(self, ):
        price_map = prices.build_price_map(self.entries)
        matching_entries, (acc_assets, acc_intflows, _) = returns.find_matching(
            self.entries, self.acc_types, '.*:Prosper')
        returns.compute_returns(self.entries, self.options_map,
                                acc_assets, acc_intflows, price_map)


class TestReturnsPeriods(test_utils.TestCase):

    # Ensure that prelude and epiloge periods are correctly found and returned.
    @loader.loaddoc
    def test_segment_periods(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking

        2014-01-01 price ACME  90.00 USD

        2014-02-01 * "Deposit"
          Assets:US:Investments:Cash       10,000 USD
          Assets:US:Bank:Checking

        2014-08-01 balance Assets:US:Investments:Cash       10,000 USD
        2014-08-01 price ACME  100.00 USD
        """
        self.assertFalse(errors)
        assets = {'Assets:US:Investments:Cash'}
        is_external_flow_entry = lambda entry: (isinstance(entry, data.Transaction) and
                                                any(posting.account not in assets
                                                    for posting in entry.postings))
        periods = returns.segment_periods(entries, assets, assets)
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 2, 1),
             inventory.from_string(''), inventory.from_string('')),
            (datetime.date(2014, 2, 1), datetime.date(2014, 8, 1),
             inventory.from_string('10000 USD'), inventory.from_string('10000 USD'))
            ], periods)

    # Deposit, one investment, no other changes but a price change.
    @loader.loaddoc
    def test_returns_one_transfer(self, entries, errors, options_map):
        """
        plugin "beancount.ops.auto_accounts"

        2014-02-01 * "Deposit"
          Assets:US:Investments:Cash       10,000 USD
          Assets:US:Bank:Checking

        2014-02-01 * "Invest"
          Assets:US:Investments:ACME       100 ACME {90 USD}
          Assets:US:Investments:Cash       -9,000 USD

        2014-08-01 price ACME  100.00 USD

        2014-08-01 * "Withdrawal"
          Assets:US:Investments:Cash       -1,000 USD
          Assets:US:Bank:Checking

        2014-08-02 balance Assets:US:Investments:Cash    0 USD
        """
        self.assertFalse(errors)
        returns_dict = returns.compute_returns_with_regexp(entries, options_map,
                                                           '.*:Investments')
        self.assertEqual(({'USD': 1.1},
                          (datetime.date(2014, 2, 1), datetime.date(2014, 8, 2))),
                         returns_dict)

    # Dilute returns from a faraway initial date.
    @loader.loaddoc
    def test_returns_diluted(self, entries, errors, options_map):
        """
        1990-01-01 open Assets:US:Investments:Cash
        1990-01-01 open Assets:US:Investments:ACME
        1990-01-01 open Assets:US:Bank:Checking

        2014-02-01 * "Deposit"
          Assets:US:Investments:Cash       10,000 USD
          Assets:US:Bank:Checking

        2014-02-01 * "Invest"
          Assets:US:Investments:ACME       100 ACME {90 USD}
          Assets:US:Investments:Cash       -9,000 USD

        2014-08-01 price ACME  100.00 USD
        """
        self.assertFalse(errors)
        returns_dict = returns.compute_returns_with_regexp(entries, options_map,
                                                           '.*:Investments')
        #print(returns_dict)
        # self.assertEqual(({'USD': 1.1},
        #                   (datetime.date(2014, 2, 1), datetime.date(2014, 8, 2))),
        #                  returns_dict)
