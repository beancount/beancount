__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core.number import D
from beancount.core.data import create_simple_posting as P

from beancount import loader
from beancount.core import data
from beancount.core import interpolate
from beancount.core import inventory
from beancount.core import position
from beancount.parser import cmptest
from beancount.parser import parser
from beancount.parser import booking_simple


# True if errors are generated on residual by get_incomplete_postings().
ERRORS_ON_RESIDUAL = False


class TestGetIncompletePostings(cmptest.TestCase):

    OPTIONS_MAP = {'inferred_tolerance_default': {},
                   'inferred_tolerance_multiplier': D('0.5'),
                   'account_rounding': None,
                   'infer_tolerance_from_cost': False}

    def test_get_incomplete_postings_pathological(self):
        meta = data.new_metadata(__file__, 0)

        # Test with no entries.
        entry = data.Transaction(meta, None, None, None, None,
                                 data.EMPTY_SET, data.EMPTY_SET, [])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(0, len(new_postings))
        self.assertEqual(0, len(errors))

        # Test with only a single leg (and check that it does not balance).
        entry = data.Transaction(meta, None, None, None, None,
                                 data.EMPTY_SET, data.EMPTY_SET, [
            P(None, "Assets:Bank:Checking", "105.50", "USD"),
            ])
        (new_postings, has_inserted, errors,
         residual, tolerances) = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(1, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))
        self.assertIsInstance(tolerances, dict)

        # Test with two legs that balance.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Savings", "-105.50", "USD"),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(2, len(new_postings))
        self.assertEqual(0, len(errors))

        # Test with two legs that do not balance.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Savings", "-115.50", "USD"),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(2, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

        # Test with only one auto-posting.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(0, len(new_postings))
        self.assertEqual(1, len(errors))

        # Test with an auto-posting where there is no residual.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Savings", "-105.50", "USD"),
                P(None, "Assets:Bank:Balancing", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1, len(errors))

        # Test with too many empty postings.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Savings", "-106.50", "USD"),
                P(None, "Assets:Bank:BalancingA", None, None),
                P(None, "Assets:Bank:BalancingB", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1, len(errors))

    def test_get_incomplete_postings_normal(self):
        meta = data.new_metadata(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Savings", "-115.50", "USD"),
                P(None, "Assets:Bank:Balancing", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertTrue(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(0, len(errors))
        self.assertTrue(interpolate.AUTOMATIC_META in new_postings[2].meta)

    def test_get_incomplete_postings_residual(self):
        meta = data.new_metadata(__file__, 0)

        # Test with a single auto-posting with a residual.
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Savings", "-115.501", "USD"),
                P(None, "Assets:Bank:Balancing", "10.00", "USD"),
            ])
        _, __, ___, residual, ____ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
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
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Income:US:Anthem:InsurancePayments", "-275.81", "USD"),
                P(None, "Income:US:Anthem:InsurancePayments", "-23738.54", "USD"),
                P(None, "Assets:Bank:Checking", "24014.45", "USD"),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(3, len(new_postings))
        self.assertEqual(1 if ERRORS_ON_RESIDUAL else 0, len(errors))

    def test_balance_with_zero_posting(self):
        meta = data.new_metadata(__file__, 0)
        entry = data.Transaction(
            meta, None, None, None, None, data.EMPTY_SET, data.EMPTY_SET, [
                P(None, "Income:US:Anthem:InsurancePayments", "0", "USD"),
                P(None, "Income:US:Anthem:InsurancePayments", None, None),
            ])
        new_postings, has_inserted, errors, _, __ = booking_simple.get_incomplete_postings(
            entry, self.OPTIONS_MAP)
        self.assertFalse(has_inserted)
        self.assertEqual(1, len(new_postings))
        self.assertEqual(0, len(errors))


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
        errors = booking_simple.balance_incomplete_postings(entry, options_map)
        return entry, errors

    def test_balance_incomplete_postings__noop(self):
        entry, errors = self.get_incomplete_entry("""
          option "booking_algorithm" "SIMPLE"
          2013-02-23 * "Something"
            Liabilities:CreditCard     -50 USD
            Expenses:Restaurant         50 USD
        """)
        self.assertFalse(errors)
        self.assertEqual(2, len(entry.postings))

    def test_balance_incomplete_postings__fill1(self):
        entry, errors = self.get_incomplete_entry("""
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"
          option "inferred_tolerance_default" "USD:0.01"

          2013-02-23 * "Something"
            Assets:Invest     1.245 RGAGX {43.23 USD}
            Assets:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(D('-53.82'), entry.postings[1].units.number)

        entry, errors = self.get_incomplete_entry("""
          option "booking_algorithm" "SIMPLE"
          option "inferred_tolerance_default" "USD:0.001"

          2013-02-23 * "Something"
            Assets:Invest     1.245 RGAGX {43.23 USD}
            Assets:Cash
        """)
        self.assertFalse(errors)
        self.assertEqual(D('-53.821'), entry.postings[1].units.number)

    def test_balance_incomplete_postings__rounding_and_quantum(self):
        entry, errors = self.get_incomplete_entry("""
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"
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
          option "booking_algorithm" "SIMPLE"

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
          option "booking_algorithm" "SIMPLE"

          2000-01-01 open Assets:Account1
          2000-01-01 open Assets:Account2

          2016-04-23 * ""
            Assets:Account1     0.00 USD
            Assets:Account2
        """
        # This does not raise an error, but it ought to; do this in the full
        # booking method since we're deprecating this code.
        self.assertEqual(1, len(entries[-1].postings))


class TestSimpleBooking(cmptest.TestCase):

    @loader.load_doc()
    def test_simple_booking_algorithm(self, entries, _, options_map):
        """
          option "booking_algorithm" "SIMPLE"

          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {502 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {500 # 10 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {# 2510 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 2510 USD
            Equity:Opening-Balances
        """
        for entry in entries:
            if not isinstance(entry, data.Transaction):
                continue
            self.assertEqual(D('-2510'), entry.postings[-1].units.number)

    @loader.load_doc(expect_errors=True)
    def test_simple_booking_algorithm__invalid(self, _, errors, __):
        """
          option "booking_algorithm" "XXX"
        """
        self.assertEqual(1, len(errors))

    @loader.load_doc(expect_errors=True)
    def test_simple_booking_algorithm__issue139(self, _, errors, __):
        """
          option "booking_algorithm" "SIMPLE"
          plugin "beancount.plugins.auto_accounts"

          2016-08-06 * "Test"
            Expenses:Other                          57.30 BRL @@ 16.18 EUR
            Expenses:Food                             21.90 BRL @@ 6.18
            Assets:Something
        """
        self.assertRegex(errors[0].message,
                         'Missing number or currency on price not handled')

    @loader.load_doc(expect_errors=True)
    def test_no_cost_is_rejected(self, _, errors, __):
        """
          option "booking_algorithm" "SIMPLE"
          plugin "beancount.plugins.auto_accounts"

          2014-10-15 * "buy widgets"
            Assets:Inventory     10 WIDGET {} ;; Not supported.
            Assets:Cash         -80 GBP
        """
        # See https://groups.google.com/d/msg/beancount/9NBcT-SXZMQ/sy7Z3dIdBQAJ
        self.assertRegex(errors[0].message,
                         'Cost syntax not supported')
