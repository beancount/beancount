__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import unittest

from beancount.core.number import D
from beancount.core.amount import A
from beancount.core.data import create_simple_posting as P
from beancount.core.data import create_simple_posting_with_cost as PCost
from beancount.core import interpolate
from beancount.core import data
from beancount.core import inventory
from beancount.core import position
from beancount.parser import parser
from beancount.parser import booking_simple
from beancount.parser import cmptest
from beancount import loader


# True if errors are generated on residual by get_incomplete_postings().
ERRORS_ON_RESIDUAL = False


# A default options map just to provide the tolerances.
OPTIONS_MAP = {'inferred_tolerance_default': {},
               'inferred_tolerance_multiplier': D('0.5'),
               'use_legacy_fixed_tolerances': False,
               'account_rounding': None,
               'infer_tolerance_from_cost': False}


class TestBalance(cmptest.TestCase):

    def test_get_posting_weight(self):

        # Entry without cost, without price.
        posting = P(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertEqual(A("105.50 USD"),
                         interpolate.get_posting_weight(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=A("0.90 CAD"))
        self.assertEqual(A("94.95 CAD"),
                         interpolate.get_posting_weight(posting))

        # Entry with cost, without price.
        posting = PCost(None, "Assets:Bank:Checking", "105.50", "USD", "0.80", "EUR")
        self.assertEqual(A("84.40 EUR"),
                         interpolate.get_posting_weight(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=A("2.00 CAD"))
        self.assertEqual(A("84.40 EUR"),
                         interpolate.get_posting_weight(posting))

    def test_has_nontrivial_balance(self):

        # Entry without cost, without price.
        posting = P(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertFalse(interpolate.has_nontrivial_balance(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=A("0.90 CAD"))
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

        # Entry with cost, without price.
        posting = PCost(None, "Assets:Bank:Checking", "105.50", "USD", "0.80", "EUR")
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=A("2.00 CAD"))
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

    def test_compute_residual(self):

        # Try with two accounts.
        residual = interpolate.compute_residual([
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Checking", "-194.50", "USD"),
            ])
        self.assertEqual(inventory.from_string("-89 USD"), residual.units())

        # Try with more accounts.
        residual = interpolate.compute_residual([
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Checking", "-194.50", "USD"),
            P(None, "Assets:Bank:Investing", "5", "AAPL"),
            P(None, "Assets:Bank:Savings", "89.00", "USD"),
            ])
        self.assertEqual(inventory.from_string("5 AAPL"), residual.units())

    @loader.load_doc(expect_errors=True)
    def test_fill_residual_posting(self, entries, _, __):
        """
        2001-01-01 open Assets:Account1
        2001-01-01 open Assets:Other

        2014-01-01 *
          Assets:Account1      100.00 USD
          Assets:Other        -100.00 USD

        2014-01-02 *
          Assets:Account1      100.00 USD
          Assets:Other        -100.00 USD

        2014-01-03 *
          Assets:Account1      100.00 USD
          Assets:Other        -100.0000001 USD

        2014-01-04 *
          Assets:Account1      100.00 USD
          Assets:Other        -112.69 CAD @ 0.8875 USD
        """
        account = 'Equity:Rounding'
        entries = [entry for entry in entries if isinstance(entry, data.Transaction)]
        for index in 0, 1:
            entry = interpolate.fill_residual_posting(entries[index], account)
            self.assertEqualEntries([entries[index]], [entry])
            residual = interpolate.compute_residual(entry.postings)
            self.assertTrue(residual.is_empty())

        entry = interpolate.fill_residual_posting(entries[2], account)
        self.assertEqualEntries("""

        2014-01-03 *
          Assets:Account1      100.00 USD
          Assets:Other        -100.0000001 USD
          Equity:Rounding        0.0000001 USD

        """, [entry])
        residual = interpolate.compute_residual(entry.postings)
        # Note: The residual calcualtion ignores postings inserted by the
        # rounding account.
        self.assertFalse(residual.is_empty())
        self.assertEqual(inventory.from_string('-0.0000001 USD'), residual)

        entry = interpolate.fill_residual_posting(entries[3], account)
        self.assertEqualEntries("""

        2014-01-04 *
          Assets:Account1     100.00 USD
          Assets:Other       -112.69 CAD @ 0.8875 USD
          Equity:Rounding   0.012375 USD

        """, [entry])
        residual = interpolate.compute_residual(entry.postings)
        # Same as above.
        self.assertFalse(residual.is_empty())
        self.assertEqual(inventory.from_string('-0.012375 USD'), residual)

    def test_get_incomplete_postings_pathological(self):
        meta = data.new_metadata(__file__, 0)

        # Test with no entries.
        entry = data.Transaction(meta, None, None, None, None, None, None, [])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(0, len(new_postings))
        self.assertEqual(0, len(errors))

        # Test with only a single leg (and check that it does not balance).
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            ])
        (new_postings, has_inserted, errors,
         residual, tolerances) = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(1, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))
        self.assertIsInstance(tolerances, dict)

        # Test with two legs that balance.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-105.50", "USD"),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(2, len(new_postings))
        self.assertEqual(0, len(errors))

        # Test with two legs that do not balance.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-115.50", "USD"),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(2, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

        # Test with only one auto-posting.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(0, len(new_postings))
        self.assertEqual(1, len(errors))

        # Test with an auto-posting where there is no residual.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-105.50", "USD"),
            P(None, "Assets:Bank:Balancing", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1, len(errors))

        # Test with too many empty postings.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-106.50", "USD"),
            P(None, "Assets:Bank:BalancingA", None, None),
            P(None, "Assets:Bank:BalancingB", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1, len(errors))

    def test_get_incomplete_postings_normal(self):
        meta = data.new_metadata(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-115.50", "USD"),
            P(None, "Assets:Bank:Balancing", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(0, len(errors))
        self.assertTrue(interpolate.AUTOMATIC_META in new_postings[2].meta)

    def test_get_incomplete_postings_residual(self):
        meta = data.new_metadata(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-115.501", "USD"),
            P(None, "Assets:Bank:Balancing", "10.00", "USD"),
            ])
        _, __, ___, residual, ____ = interpolate.get_incomplete_postings(entry, OPTIONS_MAP)
        self.assertEqual(inventory.from_string('-0.001 USD'), residual)

    def test_get_residual_postings(self):
        residual = inventory.from_string('0.001 USD, -0.00002 CAD')
        account_rounding = 'Equity:RoundingError'
        postings = interpolate.get_residual_postings(residual, account_rounding)
        self.assertEqual(2, len(postings))
        self.assertEqual([
            P(None, "Equity:RoundingError", "-0.001", "USD"),
            P(None, "Equity:RoundingError", "0.00002", "CAD"),
            ], [posting._replace(meta=None) for posting in postings])

    def test_balance_with_large_amount(self):
        meta = data.new_metadata(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Income:US:Anthem:InsurancePayments", "-275.81", "USD"),
            P(None, "Income:US:Anthem:InsurancePayments", "-23738.54", "USD"),
            P(None, "Assets:Bank:Checking", "24014.45", "USD"),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

    def test_balance_with_zero_posting(self):
        meta = data.new_metadata(__file__, 0)
        entry = data.Transaction(meta, None, None, None, None, None, None, [
            P(None, "Income:US:Anthem:InsurancePayments", "0", "USD"),
            P(None, "Income:US:Anthem:InsurancePayments", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = interpolate.get_incomplete_postings(
            entry, OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(1, len(new_postings))
        self.assertEqual(0, len(errors))

    @loader.load_doc()
    def test_cost_basis(self, entries, _, __):
        """
        2001-01-01 open Assets:Account1

        2014-01-01 *
          Assets:Account1        20 HOOL {40.00 USD}
          Assets:Account1        20 HOOL {60.00 USD} @ 70.00 USD
          Assets:Account1     -2000.00 USD
          Assets:Account1         3 HOOL {30.00 CAD}
          Assets:Account1       -90.00 CAD
        """
        postings = next(data.filter_txns(entries)).postings
        self.assertEqual(inventory.from_string('2000.00 USD, 90.00 CAD'),
                         interpolate.compute_cost_basis(postings))


class TestBalanceIncompletePostings(cmptest.TestCase):

    def get_incomplete_entry(self, string):
        """Parse a single incomplete entry and convert its CostSpec to a Cost.

        Args:
          string: The input string to parse.
        Returns:
          A pair of (entry, list of errors).
        """
        entries, errors, options_map = parser.parse_string(string, dedent=True)
        self.assertFalse(errors)
        self.assertEqual(1, len(entries))
        (entries_with_lots, errors) = booking_simple.convert_lot_specs_to_lots(entries)
        self.assertEqual(1, len(entries))
        entry = entries_with_lots[0]
        errors = interpolate.balance_incomplete_postings(entry, options_map)
        return entry, errors

    def test_balance_incomplete_postings__noop(self):
        entry, errors = self.get_incomplete_entry("""
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Expenses:Restaurant         50 USD
        """)
        self.assertFalse(errors)
        self.assertEqual(2, len(entry.postings))

    def test_balance_incomplete_postings__fill1(self):
        entry, errors = self.get_incomplete_entry("""
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Expenses:Restaurant
        """)
        self.assertFalse(errors)
        self.assertEqual(2, len(entry.postings))
        self.assertEqual(position.get_position(entry.postings[1]),
                         position.from_string('50 USD'))

    def test_balance_incomplete_postings__fill2(self):
        entry, errors = self.get_incomplete_entry("""
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Liabilities:CreditCard     -50 CAD
            Expenses:Restaurant
        """)
        self.assertFalse(errors)
        self.assertEqual(4, len(entry.postings))
        self.assertEqual(entry.postings[2].account, 'Expenses:Restaurant')
        self.assertEqual(entry.postings[3].account, 'Expenses:Restaurant')
        self.assertEqual(position.get_position(entry.postings[2]),
                         position.from_string('50 USD'))
        self.assertEqual(position.get_position(entry.postings[3]),
                         position.from_string('50 CAD'))

    def test_balance_incomplete_postings__cost(self):
        entry, errors = self.get_incomplete_entry("""
          2013-02-23 * "Something"
            Assets:Invest     10 MSFT {43.23 USD}
            Assets:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(2, len(entry.postings))
        self.assertEqual(entry.postings[1].account, 'Assets:Cash')
        self.assertEqual(position.get_position(entry.postings[1]),
                         position.from_string('-432.30 USD'))

    def test_balance_incomplete_postings__insert_rounding(self):
        entry, errors = self.get_incomplete_entry("""
          option "account_rounding" "RoundingError"

          2013-02-23 * "Something"
            Assets:Invest     1.245 RGAGX {43.23 USD}
            Assets:Cash      -53.82 USD
        """)
        self.assertFalse(errors)
        self.assertEqual(3, len(entry.postings))
        self.assertEqual(entry.postings[2].account, 'Equity:RoundingError')
        self.assertEqual(position.get_position(entry.postings[2]),
                         position.from_string('-0.00135 USD'))

    def test_balance_incomplete_postings__quantum(self):
        entry, errors = self.get_incomplete_entry("""
          option "inferred_tolerance_default" "USD:0.01"

          2013-02-23 * "Something"
            Assets:Invest     1.245 RGAGX {43.23 USD}
            Assets:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(D('-53.82'), entry.postings[1].units.number)

        entry, errors = self.get_incomplete_entry("""
          option "inferred_tolerance_default" "USD:0.001"

          2013-02-23 * "Something"
            Assets:Invest     1.245 RGAGX {43.23 USD}
            Assets:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(D('-53.821'), entry.postings[1].units.number)

    def test_balance_incomplete_postings__rounding_and_quantum(self):
        entry, errors = self.get_incomplete_entry("""
          option "account_rounding" "RoundingError"
          option "inferred_tolerance_default" "USD:0.01"

          2013-02-23 * "Something"
            Assets:Invest     1.245 RGAGX {43.23 USD}
            Assets:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(3, len(entry.postings))
        self.assertEqual(D('-53.82'), entry.postings[1].units.number)
        self.assertEqual('Equity:RoundingError', entry.postings[2].account)
        self.assertEqual(D('-0.00135'), entry.postings[2].units.number)

        entry, errors = self.get_incomplete_entry("""
          option "account_rounding" "RoundingError"
          option "inferred_tolerance_default" "USD:0.01"

          2014-05-06 * "Buy mutual fund"
            Assets:Investments:RGXGX       4.27 RGAGX {53.21 USD}
            Assets:Investments:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(3, len(entry.postings))
        self.assertEqual(D('-227.2100'), entry.postings[1].units.number)
        self.assertEqual('Equity:RoundingError', entry.postings[2].account)
        self.assertEqual(D('0.0033'), entry.postings[2].units.number)

    def test_balance_incomplete_postings__rounding_with_error(self):
        # Here we want to verify that auto-inserting rounding postings does not
        # disable non-balancing transactions. This is rather an important check!
        entries, errors, options_map = loader.load_string("""
          option "account_rounding" "RoundingError"

          2000-01-01 open Assets:Investments:MutualFunds:XXX
          2000-01-01 open Assets:Cash:Checking
          2000-01-01 open Equity:RoundingError

          ;; This transaction does not balance.
          2002-02-08 * "Mutual fund purchase"
            Assets:Investments:MutualFunds:XXX    51.031 XXX {97.98 USD}
            Assets:Cash:Checking                -5000.00 USD
        """, dedent=True)
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'does not balance')

    @loader.load_doc(expect_errors=True)
    def test_balance_incomplete_postings__superfluous_unneeded(self, entries, errors, _):
        """
          2000-01-01 open Assets:Account1
          2000-01-01 open Assets:Account2
          2000-01-01 open Assets:Account3

          2016-04-23 * ""
            Assets:Account1   100.00 USD
            Assets:Account2  -100.00 USD
            Assets:Account3
        """
        # Note: this raises an error because the auto-posting is superfluous.
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'Useless auto-posting')
        self.assertEqual(3, len(entries[-1].postings))

    @loader.load_doc()
    def test_balance_incomplete_postings__superfluous_unused(self, entries, errors, _):
        """
          2000-01-01 open Assets:Account1
          2000-01-01 open Assets:Account2

          2016-04-23 * ""
            Assets:Account1     0.00 USD
            Assets:Account2
        """
        # This does not raise an error, but it ought to; do this in the full
        # booking method since we're deprecating this code.
        self.assertEqual(1, len(entries[-1].postings))


class TestComputeBalance(unittest.TestCase):

    @loader.load_doc()
    def test_compute_entries_balance_currencies(self, entries, _, __):
        """
        2014-01-01 open Assets:Bank:Checking
        2014-01-01 open Assets:Bank:Savings
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Other

        2014-06-01 *
          Assets:Bank:Checking  111.23 USD
          Assets:Other

        2014-06-02 *
          Assets:Bank:Savings   222.74 USD
          Assets:Other

        2014-06-03 *
          Assets:Bank:Savings   17.23 CAD
          Assets:Other

        2014-06-04 *
          Assets:Investing      10000 EUR
          Assets:Other

        """
        computed_balance = interpolate.compute_entries_balance(entries)
        expected_balance = inventory.Inventory()
        self.assertEqual(expected_balance, computed_balance)

    @loader.load_doc()
    def test_compute_entries_balance_at_cost(self, entries, _, __):
        """
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Other

        2014-06-05 *
          Assets:Investing      30 HOOL {40 USD}
          Assets:Other

        2014-06-05 *
          Assets:Investing      -20 HOOL {40 USD}
          Assets:Other

        """
        computed_balance = interpolate.compute_entries_balance(entries)
        expected_balance = inventory.Inventory()
        expected_balance.add_amount(A('-400 USD'))
        expected_balance.add_amount(A('10 HOOL'),
                                    position.Cost(D('40'), 'USD', None, None))

        self.assertEqual(expected_balance, computed_balance)

    @loader.load_doc()
    def test_compute_entries_balance_conversions(self, entries, _, __):
        """
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Other

        2014-06-06 *
          Assets:Investing          1000 EUR @ 1.78 GBP
          Assets:Other

        2014-06-07 *
          Assets:Investing          1000 EUR @@ 1780 GBP
          Assets:Other
        """
        computed_balance = interpolate.compute_entries_balance(entries)
        expected_balance = inventory.Inventory()
        expected_balance.add_amount(A('2000.00 EUR'))
        expected_balance.add_amount(A('-3560.00 GBP'))
        self.assertEqual(expected_balance, computed_balance)

    @loader.load_doc()
    def test_compute_entry_context(self, entries, _, __):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Assets:Account3
        2014-01-01 open Assets:Account4
        2014-01-01 open Assets:Other

        2014-02-10 *
          Assets:Account1      100.00 USD
          Assets:Other

        2014-02-11 *
          Assets:Account2       80.00 USD
          Assets:Other

        2014-02-12 *
          Assets:Account3       60.00 USD
          Assets:Account3       40.00 USD
          Assets:Other

        2014-02-20 * #context
          Assets:Account1       5.00 USD
          Assets:Account2      -5.00 USD

        2014-02-21 balance  Assets:Account1   105.00 USD

        2014-02-25 *
          Assets:Account3       5.00 USD
          Assets:Account4      -5.00 USD

        """
        for entry in entries:
            if (isinstance(entry, data.Transaction) and
                entry.tags and
                'context' in entry.tags):
                break
        balance_before, balance_after = interpolate.compute_entry_context(entries, entry)

        self.assertEqual(inventory.from_string('100.00 USD'),
                         balance_before['Assets:Account1'])
        self.assertEqual(inventory.from_string('80.00 USD'),
                         balance_before['Assets:Account2'])

        self.assertEqual(inventory.from_string('105.00 USD'),
                         balance_after['Assets:Account1'])
        self.assertEqual(inventory.from_string('75.00 USD'),
                         balance_after['Assets:Account2'])

        # Get the context for an entry that is not a Transaction and ensure that
        # the before and after context is the same.
        for entry in entries:
            if isinstance(entry, data.Balance):
                break
        balance_before, balance_after = interpolate.compute_entry_context(entries, entry)
        self.assertEqual(balance_before, balance_after)


class TestInferTolerances(cmptest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_tolerances__no_precision(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account1       500 USD
          Assets:Account2      -120 USD
          Assets:Account3      -380 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({}, tolerances)

    @loader.load_doc()
    def test_tolerances__dubious_precision(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Assets:Account3
        2014-01-01 open Assets:Account4

        2014-02-25 *
          Assets:Account1       5.0000 USD
          Assets:Account2       5.000 USD
          Assets:Account3       5.00 USD
          Assets:Account4      -5.0 USD
          Assets:Account4      -5 USD
          Assets:Account4      -5 USD
        """
        tolerances = interpolate.infer_tolerances(entries[-1].postings, options_map)
        self.assertEqual({'USD': D('0.05')}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__ignore_price(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT @ 102.2340 USD
          Assets:Account4      -511.11 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'USD': D('0.005')}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__ignore_cost(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT {102.2340 USD}
          Assets:Account4      -511.11 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'USD': D('0.005')}, tolerances)


    @loader.load_doc(expect_errors=True)
    def test_tolerances__ignore_cost_and_price(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT {102.2340 USD} @ 103.45237239 USD
          Assets:Account4      -511.11 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'USD': D('0.005')}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__cost_and_number_ignored(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT {102.2340 USD}
          Assets:Account4      -511 USD
        """
        tolerances = interpolate.infer_tolerances(entries[-1].postings, options_map)
        self.assertEqual({}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__number_on_cost_used(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.111 VHT {102.2340 USD}
          Assets:Account4      -511 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'VHT': D('0.0005')}, tolerances)
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map, True)
        self.assertEqual({'VHT': D('0.0005'), 'USD': D('0.051117')}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__number_on_cost_used_overrides(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.111 VHT {102.2340 USD}
          Assets:Account4      -511.0 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'VHT': D('0.0005'), 'USD': D('0.05')}, tolerances)
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map, True)
        self.assertEqual({'VHT': D('0.0005'), 'USD': D('0.051117')}, tolerances)

    def test_tolerances__number_on_cost_fail_to_succ(self):
        # An example of a transaction that would fail without the inferred
        # tolerances and succeed with them.
        input_string = textwrap.dedent("""
          plugin "beancount.plugins.auto_accounts"

          2014-02-25 *
            Assets:Account3       5.111 VHT {1000.00 USD}
            Assets:Account4      -5110.80 USD
        """)
        input_option = textwrap.dedent("""
          option "infer_tolerance_from_cost" "True"
        """)

        entries, errors, options_map = loader.load_string(input_string)
        self.assertFalse(options_map["infer_tolerance_from_cost"])
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'Transaction does not balance:.*0.20000 USD')

        entries, errors, options_map = loader.load_string(input_option + input_string)
        self.assertTrue(options_map["infer_tolerance_from_cost"])
        self.assertFalse(errors)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__minimum_on_costs(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.11111   VHT {102.2340 USD}
          Assets:Account3       5.111111  VHT {102.2340 USD}
          Assets:Account3       5.1111111 VHT {102.2340 USD}
          Assets:Account4  -1564.18 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'VHT': D('0.000005'), 'USD': D('0.005')}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__with_inference(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.1111   VHT {102.2340 USD}
          Assets:Account4
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({'VHT': D('0.00005')},
                         tolerances)
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map, True)
        self.assertEqual({'VHT': D('0.00005'), 'USD': D('0.005111700')},
                         tolerances)

    @loader.load_doc()
    def test_tolerances__capped_inference(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account3
        2014-01-01 open Assets:Account4

        2014-02-25 *
          Assets:Account3       5.1   VHT {102.2340 USD}
          Assets:Account4
        """
        tolerances = interpolate.infer_tolerances(entries[-1].postings, options_map)
        self.assertEqual({'VHT': D('0.05')},
                         tolerances)
        tolerances = interpolate.infer_tolerances(entries[-1].postings, options_map, True)
        self.assertEqual({'VHT': D('0.05'), 'USD': D('0.5')},
                         tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__multiplier(self, entries, errors, options_map):
        """
        option "inferred_tolerance_multiplier" "1.1"

        1970-01-01 open Assets:B1
        1970-01-01 open Assets:B2

        2010-01-01 * "Balances"
          Assets:B1      -200.00 EUR
          Assets:B2       200.011 EUR

        2010-01-02 * "Does not balance"
          Assets:B1      -200.00 EUR
          Assets:B2       200.012 EUR
        """
        self.assertEqual(1, len(errors))
        self.assertTrue(errors[0].entry is entries[-1])

    @loader.load_doc(expect_errors=False)
    def test_tolerances__legacy(self, entries, _, __):
        """
        ;; issue/47
        option "use_legacy_fixed_tolerances" "TRUE"

        1970-01-01 open Assets:B1
        1970-01-01 open Assets:B2

        2010-01-01 * "something"
          Assets:B1      -200 EUR
          Assets:B2
        """

    @loader.load_doc()
    def test_tolerances__bug(self, entries, errors, _):
        """
        option "operating_currency" "USD"
        option "infer_tolerance_from_cost" "TRUE"

        2000-01-01 open Assets:CAAPX
        2000-01-01 open Income:Match

        2006-11-02 * "Misc"
          Assets:CAAPX  -1.729 CAAPX {{521.67787 USD}} @ 49.65 USD
          Income:Match
        """
        self.assertFalse(errors)

    @loader.load_doc()
    def test_tolerances__bug53a(self, entries, errors, _):
        """
        option "operating_currency" "USD"
        option "infer_tolerance_from_cost" "TRUE"

        2000-01-01 open Assets:Investments:VWELX
        2000-01-01 open Assets:Investments:Cash

        2006-01-17 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -575.00 USD
        """
        self.assertFalse(errors)

    @loader.load_doc()
    def test_tolerances__bug53b(self, entries, errors, _):
        """
        option "operating_currency" "USD"
        option "infer_tolerance_from_cost" "TRUE"

        2000-01-01 open Assets:Investments:VWELX
        2000-01-01 open Assets:Investments:Cash

        2006-01-02 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -575.00 USD
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -575.00 USD

        2006-01-03 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -575.00 USD
          Assets:Investments:Cash -575.00 USD
          Assets:Investments:Cash -575.00 USD

        2006-01-03 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -1725.00 USD

        2006-01-16 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -9200.00 USD
        """
        transactions = [entry
                        for entry in entries
                        if isinstance(entry, data.Transaction)]
        self.assertEqual({'USD': D('0.03096'), 'VWELX': D('0.0005')},
                         transactions[0].meta['__tolerances__'])
        self.assertEqual({'USD': D('0.04644'), 'VWELX': D('0.0005')},
                         transactions[1].meta['__tolerances__'])
        self.assertEqual({'USD': D('0.04644'), 'VWELX': D('0.0005')},
                         transactions[2].meta['__tolerances__'])
        self.assertEqual({'USD': D('0.247680'), 'VWELX': D('0.0005')},
                         transactions[3].meta['__tolerances__'])
        self.assertFalse(errors)

    @loader.load_doc()
    def test_tolerances__bug53_price(self, entries, errors, _):
        """
        option "operating_currency" "USD"
        option "infer_tolerance_from_cost" "TRUE"

        2000-01-01 open Assets:Investments:VWELX
        2000-01-01 open Assets:Investments:Cash

        2006-01-02 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX @ 20.40 USD
          Assets:Investments:Cash
        """
        transactions = [entry
                        for entry in entries
                        if isinstance(entry, data.Transaction)]
        self.assertEqual({'USD': D('0.02568'), 'VWELX': D('0.0005')},
                         transactions[0].meta['__tolerances__'])
        self.assertFalse(errors)

    @loader.load_doc()
    def test_tolerances__ignore_from_auto_postings(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.split_expenses" "Martin Caroline Sheila"

        option "inferred_tolerance_default" "USD:0.005"

        1970-01-01 open Expenses:Food
        1970-01-01 open Assets:Caroline

        2010-01-01 * "Balances"
          Expenses:Food      -8.00 USD
          Assets:Caroline
        """
        # Interesting case: The Assets leg is filled in with 8.00 USD
        # automatically here, so it is not used in inference. Further forward,
        # the split_expenses plugin splits the first leg as well, and that is
        # also marked as automatic, so if cannot use inference there either. So
        # all legs end up being automatic... and we have to fall back on the
        # default tolerance.
        pass
