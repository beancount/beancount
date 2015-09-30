__author__ = "Martin Blais <blais@furius.ca>"

from beancount.core.number import D
from beancount.core.inventory import from_string as I
from beancount.utils.misc_utils import dictmap
from beancount.parser import parser
from beancount.parser import booking_full
from beancount.parser import cmptest
from beancount import loader


class TestGroupPostings(cmptest.TestCase):

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize_by_currency__no_cost_with_currency(self, entries, _, options_map):
        """
        2015-01-01 *
          Assets:Bank:Investing           5 HOOL {100 USD}
          Equity:Opening-Balances      -500 USD

        2015-01-01 *
          Assets:Bank:Investing           5 HOOL {100 USD}
          Equity:Opening-Balances           USD
        """
        for entry in entries:
            groups, errors = booking_full.categorize_by_currency(entry, {})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0,1}}, groups)

## FIXME: Continue here.





    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_no_choice(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})
        self.assertEqual({'USD': 2}, dictmap(groups, valfun=len))
        self.assertFalse(free)

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_choose_lot(self, ientries, _, options_map):
        """
        ;; This should know to pick the USD leg because that's the only currency
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}, '
                                            '1 HOOL {100 CAD}')})

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_choose_ccy(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
          Equity:Opening-Balances       102 CAD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_no_choice(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       100 USD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_with_bal(self, ientries, _, options_map):
        """
        ;; This should know to pick the USD leg because that's the only that doesn't already
        ;; balance from the other postings.
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
          Equity:Opening-Balances      -102 CAD
          Assets:Cash                   102 CAD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}, '
                                               '1 HOOL {100 CAD}')})


    # FIXME: Come up with cases where we're able to infer an AUGMENTING leg

    # FIXME: Come up wiht a case that would be ambiguous if not for the fact
    # that one of the currencies already balances.




class TestFullBooking(cmptest.TestCase):

    @loader.load_doc()
    def __test_full_booking(self, entries, _, options_map):
        """
          option "booking_method" "FULL"
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing           5 HOOL {501 USD}
            Equity:Opening-Balances     -2505 USD
        """
        self.assertEqual(D('-2505'), entries[-1].postings[-1].position.units.number)
