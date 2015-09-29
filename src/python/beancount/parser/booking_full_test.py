__author__ = "Martin Blais <blais@furius.ca>"

from beancount.core.number import D
from beancount.core.inventory import from_string as I
from beancount.utils.misc_utils import dictmap
from beancount.parser import parser
from beancount.parser import booking_full
from beancount.parser import cmptest
from beancount import loader


class ___TestGroupPostings(cmptest.TestCase): # pylint: disable=invalid-name

    @parser.parse_doc()
    def test_group_postings__one_group(self, ientries, _, options_map):
        """
        2015-01-01 *
          Assets:Bank:Investing           5 GOOG {100 USD}
          Equity:Opening-Balances      -500 USD
        """
        groups = booking_full.group_postings_by_currency(ientries[0].postings, {})
        self.assertEqual({'USD': 2}, dictmap(groups, valfun=len))

    @parser.parse_doc()
    def test_group_postings__ambiguous_cost_no_choice(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 GOOG {}
          Equity:Opening-Balances       101 USD
        """
        groups, free = booking_full.group_postings_by_currency(
            ientries[0].postings, {'USD': I('1 GOOG {100 USD}')})
        self.assertEqual({'USD': 2}, dictmap(groups, valfun=len))
        self.assertFalse(free)

    @parser.parse_doc()
    def test_group_postings__ambiguous_cost_choose_lot(self, ientries, _, options_map):
        """
        ;; This should know to pick the USD leg because that's the only currency
        2015-01-01 *
          Assets:Bank:Investing          -1 GOOG {}
          Equity:Opening-Balances       101 USD
        """
        groups, free = booking_full.group_postings_by_currency(
            ientries[0].postings, {'USD': I('1 GOOG {100 USD}, '
                                            '1 GOOG {100 CAD}')})

    @parser.parse_doc()
    def test_group_postings__ambiguous_cost_choose_ccy(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 GOOG {}
          Equity:Opening-Balances       101 USD
          Equity:Opening-Balances       102 CAD
        """
        groups, free = booking_full.group_postings_by_currency(
            ientries[0].postings, {'USD': I('1 GOOG {100 USD}')})

    @parser.parse_doc()
    def test_group_postings__ambiguous_cost_no_choice(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 GOOG {}
          Equity:Opening-Balances       100 USD
        """
        groups, free = booking_full.group_postings_by_currency(
            ientries[0].postings, {'USD': I('1 GOOG {100 USD}')})

    @parser.parse_doc()
    def test_group_postings__ambiguous_cost_with_bal(self, ientries, _, options_map):
        """
        ;; This should know to pick the USD leg because that's the only that doesn't already
        ;; balance from the other postings.
        2015-01-01 *
          Assets:Bank:Investing          -1 GOOG {}
          Equity:Opening-Balances       101 USD
          Equity:Opening-Balances      -102 CAD
          Assets:Cash                   102 CAD
        """
        groups, free = booking_full.group_postings_by_currency(
            ientries[0].postings, {'USD': I('1 GOOG {100 USD}, '
                                               '1 GOOG {100 CAD}')})


    # FIXME: Come up with cases where we're able to infer an AUGMENTING leg

    # FIXME: Come up wiht a case that would be ambiguous if not for the fact
    # that one of the currencies already balances.




class __TestFullBooking(cmptest.TestCase): # pylint: disable=invalid-name

    @loader.load_doc()
    def test_full_booking(self, entries, _, options_map):
        """
          option "booking_method" "FULL"
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing           5 GOOG {501 USD}
            Equity:Opening-Balances     -2505 USD
        """
        self.assertEqual(D('-2505'), entries[-1].postings[-1].position.units.number)
