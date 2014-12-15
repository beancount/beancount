__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import copy

from beancount.core.data import create_simple_posting as P
from beancount.core.data import create_simple_posting_with_cost as PCost
from beancount.core import interpolate
from beancount.core import data
from beancount.core import inventory
from beancount.core import amount
from beancount.parser import parser
from beancount.parser import cmptest


# True if errors are generated on residual by get_incomplete_postings().
ERRORS_ON_RESIDUAL = False


class TestBalance(cmptest.TestCase):

    def test_get_posting_weight(self):

        # Entry without cost, without price.
        posting = P(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertEqual(amount.Amount("105.50", "USD"),
                         interpolate.get_posting_weight(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=amount.Amount("0.90", "CAD"))
        self.assertEqual(amount.Amount("94.95", "CAD"),
                         interpolate.get_posting_weight(posting))

        # Entry with cost, without price.
        posting = PCost(None, "Assets:Bank:Checking", "105.50", "USD", "0.80", "EUR")
        self.assertEqual(amount.Amount("84.40", "EUR"),
                         interpolate.get_posting_weight(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=amount.Amount("2.00", "CAD"))
        self.assertEqual(amount.Amount("84.40", "EUR"),
                         interpolate.get_posting_weight(posting))

    def test_has_nontrivial_balance(self):

        # Entry without cost, without price.
        posting = P(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertFalse(interpolate.has_nontrivial_balance(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=amount.Amount("0.90", "CAD"))
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

        # Entry with cost, without price.
        posting = PCost(None, "Assets:Bank:Checking", "105.50", "USD", "0.80", "EUR")
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=amount.Amount("2.00", "CAD"))
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

    @parser.parsedoc
    def test_fill_residual_posting(self, entries, _, __):
        """
        2014-01-01 *
          Assets:Account1      100.00 USD
          Assets:Other

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
        for index in 0, 1:
            entry = interpolate.fill_residual_posting(entries[index], account)
            self.assertEqualEntries([entries[index]], [entry])
            self.assertTrue(interpolate.compute_residual(entry.postings).is_empty())

        entry = interpolate.fill_residual_posting(entries[2], account)
        self.assertEqualEntries("""

        2014-01-03 *
          Assets:Account1      100.00 USD
          Assets:Other        -100.0000001 USD
          Equity:Rounding        0.0000001 USD

        """, [entry])
        self.assertTrue(interpolate.compute_residual(entry.postings).is_empty())

        entry = interpolate.fill_residual_posting(entries[3], account)
        self.assertEqualEntries("""

        2014-01-04 *
          Assets:Account1     100.00 USD
          Assets:Other       -112.69 CAD @ 0.8875 USD
          Equity:Rounding   0.012375 USD

        """, [entry])
        self.assertTrue(interpolate.compute_residual(entry.postings).is_empty())

    def test_get_incomplete_postings_pathological(self):
        source = data.Source(__file__, 0)

        # Test with no entries.
        entry = data.Transaction(source, None, None, None, None, None, None, [])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(0, len(new_postings))
        self.assertEqual(0, len(errors))

        # Test with only a single leg (and check that it does not balance).
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(1, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

        # Test with two legs that balance.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-105.50", "USD"),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(2, len(new_postings))
        self.assertEqual(0, len(errors))

        # Test with two legs that do not balance.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-115.50", "USD"),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(2, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

        # Test with only one auto-posting.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", None, None),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(0, len(new_postings))
        self.assertEqual(1, len(errors))

        # Test with an auto-posting where there is no residual.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-105.50", "USD"),
            P(None, "Assets:Bank:Balancing", None, None),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1, len(errors))

        # Test with too many empty postings.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-106.50", "USD"),
            P(None, "Assets:Bank:BalancingA", None, None),
            P(None, "Assets:Bank:BalancingB", None, None),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1, len(errors))

    def test_get_incomplete_postings_normal(self):
        source = data.Source(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            P(None, "Assets:Bank:Savings", "-115.50", "USD"),
            P(None, "Assets:Bank:Balancing", None, None),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(0, len(errors))

    def test_balance_with_large_amount(self):
        source = data.Source(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Income:US:Anthem:InsurancePayments", "-275.81", "USD"),
            P(None, "Income:US:Anthem:InsurancePayments", "-23738.54", "USD"),
            P(None, "Assets:Bank:Checking", "24014.45", "USD"),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

    def test_balance_with_zero_posting(self):
        source = data.Source(__file__, 0)
        entry = data.Transaction(source, None, None, None, None, None, None, [
            P(None, "Income:US:Anthem:InsurancePayments", "0", "USD"),
            P(None, "Income:US:Anthem:InsurancePayments", None, None),
            ])
        new_postings, has_inserted, errors = interpolate.get_incomplete_postings(entry)
        self.assertFalse(has_inserted)
        self.assertEqual(1, len(new_postings))
        self.assertEqual(0, len(errors))

    def balance_incomplete_postings(self):
        entry = parser.parse_string("""
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Expenses:Restaurant         50 USD
        """)[0][0]
        orig_entry = copy.deepcopy(entry)
        errors = interpolate.balance_incomplete_postings(entry)
        self.assertFalse(errors)
        self.assertEqual(orig_entry, entry)

        entry = parser.parse_string("""
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Expenses:Restaurant
        """)[0][0]
        orig_entry = copy.deepcopy(entry)
        errors = interpolate.balance_incomplete_postings(entry)
        self.assertFalse(errors)
        self.assertEqual(strip_recursive(orig_entry),
                         strip_recursive(entry))

        entry = parser.parse_string("""
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Liabilities:CreditCard     -50 CAD
            Expenses:Restaurant
        """)[0][0]
        errors = interpolate.balance_incomplete_postings(entry)
        self.assertFalse(errors)
        self.assertEqual(4, len(entry.postings))


class TestComputeBalance(unittest.TestCase):

    @parser.parsedoc
    def test_compute_postings_balance(self, entries, _, __):
        """
        2014-01-01 open Assets:Bank:Checking
        2014-01-01 open Assets:Bank:Savings
        2014-01-01 open Assets:Investing

        2014-05-26 note Assets:Investing "Buying some shares"

        2014-05-30 *
          Assets:Bank:Checking  111.23 USD
          Assets:Bank:Savings   222.74 USD
          Assets:Bank:Savings   17.23 CAD
          Assets:Investing      10000 EUR
          Assets:Investing      32 GOOG {45.203 USD}
          Assets:Other          1000 EUR @ 1.78 GBP
          Assets:Other          1000 EUR @@ 1780 GBP
        """
        postings = entries[:-1] + entries[-1].postings
        computed_balance = interpolate.compute_postings_balance(postings)

        expected_balance = inventory.Inventory()
        expected_balance.add_amount(amount.Amount('333.97', 'USD'))
        expected_balance.add_amount(amount.Amount('17.23', 'CAD'))
        expected_balance.add_amount(amount.Amount('32', 'GOOG'),
                                    amount.Amount('45.203', 'USD'))
        expected_balance.add_amount(amount.Amount('12000', 'EUR'))
        self.assertEqual(expected_balance, computed_balance)

    @parser.parsedoc
    def test_compute_entries_balance_currencies(self, entries, _, __):
        """
        2014-01-01 open Assets:Bank:Checking
        2014-01-01 open Assets:Bank:Savings
        2014-01-01 open Assets:Investing

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

    @parser.parsedoc
    def test_compute_entries_balance_at_cost(self, entries, _, __):
        """
        2014-01-01 open Assets:Bank:Checking
        2014-01-01 open Assets:Bank:Savings
        2014-01-01 open Assets:Investing

        2014-06-05 *
          Assets:Investing      30 GOOG {40 USD}
          Assets:Other

        2014-06-05 *
          Assets:Investing      -20 GOOG {40 USD}
          Assets:Other

        """
        computed_balance = interpolate.compute_entries_balance(entries)
        expected_balance = inventory.Inventory()
        expected_balance.add_amount(amount.Amount('-400', 'USD'))
        expected_balance.add_amount(amount.Amount('10', 'GOOG'), amount.Amount('40', 'USD'))
        self.assertEqual(expected_balance, computed_balance)

    @parser.parsedoc
    def test_compute_entries_balance_conversions(self, entries, _, __):
        """
        2014-01-01 open Assets:Bank:Checking
        2014-01-01 open Assets:Bank:Savings
        2014-01-01 open Assets:Investing

        2014-06-06 *
          Assets:Investing          1000 EUR @ 1.78 GBP
          Assets:Other

        2014-06-07 *
          Assets:Investing          1000 EUR @@ 1780 GBP
          Assets:Other
        """
        computed_balance = interpolate.compute_entries_balance(entries)
        expected_balance = inventory.Inventory()
        expected_balance.add_amount(amount.Amount('2000.00', 'EUR'))
        expected_balance.add_amount(amount.Amount('-3560.00', 'GBP'))
        self.assertEqual(expected_balance, computed_balance)

    @parser.parsedoc
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

        2014-02-21 balance  Assets:Account1   100.00 USD

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
