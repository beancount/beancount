__copyright__ = "Copyright (C) 2013-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import textwrap
import unittest

from beancount import loader
from beancount.core import convert
from beancount.core import data
from beancount.core import interpolate
from beancount.core import inventory
from beancount.core import position
from beancount.core.amount import ZERO
from beancount.core.amount import A
from beancount.core.data import create_simple_posting as P
from beancount.core.data import create_simple_posting_with_cost as PCost
from beancount.core.number import D
from beancount.parser import cmptest
from beancount.parser import parser
from beancount.utils import defdict

# A default options map just to provide the tolerances.
OPTIONS_MAP = {
    "inferred_tolerance_default": {},
    "inferred_tolerance_multiplier": D("0.5"),
    "account_rounding": None,
    "infer_tolerance_from_cost": False,
}


def create_some_test_transaction() -> data.Transaction:
    meta = data.new_metadata("___test__", 0)
    return data.Transaction(
        meta, datetime.date(2017, 12, 16), "?", None, "", data.EMPTY_SET, data.EMPTY_SET, []
    )


class TestBalance(cmptest.TestCase):
    def test_has_nontrivial_balance(self):
        # Entry without cost, without price.
        posting = P(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertFalse(interpolate.has_nontrivial_balance(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=A("0.90 CAD"))
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

        # Entry with cost, without price.
        txn = create_some_test_transaction()
        posting = PCost(txn, "Assets:Bank:Checking", "105.50", "USD", "0.80", "EUR")
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=A("2.00 CAD"))
        self.assertTrue(interpolate.has_nontrivial_balance(posting))

    def test_compute_residual(self):
        # Try with two accounts.
        residual = interpolate.compute_residual(
            [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Checking", "-194.50", "USD"),
            ]
        )
        self.assertEqual(
            inventory.from_string("-89 USD"), residual.reduce(convert.get_units)
        )

        # Try with more accounts.
        residual = interpolate.compute_residual(
            [
                P(None, "Assets:Bank:Checking", "105.50", "USD"),
                P(None, "Assets:Bank:Checking", "-194.50", "USD"),
                P(None, "Assets:Bank:Investing", "5", "AAPL"),
                P(None, "Assets:Bank:Savings", "89.00", "USD"),
            ]
        )
        self.assertEqual(
            inventory.from_string("5 AAPL"), residual.reduce(convert.get_units)
        )

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
        account = "Equity:Rounding"
        entries = [entry for entry in entries if isinstance(entry, data.Transaction)]
        for index in 0, 1:
            entry = interpolate.fill_residual_posting(entries[index], account)
            self.assertEqualEntries([entries[index]], [entry])
            residual = interpolate.compute_residual(entry.postings)
            self.assertTrue(residual.is_empty())

        entry = interpolate.fill_residual_posting(entries[2], account)
        self.assertEqualEntries(
            """

        2014-01-03 *
          Assets:Account1      100.00 USD
          Assets:Other        -100.0000001 USD
          Equity:Rounding        0.0000001 USD

        """,
            [entry],
        )
        residual = interpolate.compute_residual(entry.postings)
        # Note: The residual calculation ignores postings inserted by the
        # rounding account.
        self.assertFalse(residual.is_empty())
        self.assertEqual(inventory.from_string("-0.0000001 USD"), residual)

        entry = interpolate.fill_residual_posting(entries[3], account)
        self.assertEqualEntries(
            """

        2014-01-04 *
          Assets:Account1     100.00 USD
          Assets:Other       -112.69 CAD @ 0.8875 USD
          Equity:Rounding   0.012375 USD

        """,
            [entry],
        )
        residual = interpolate.compute_residual(entry.postings)
        # Same as above.
        self.assertFalse(residual.is_empty())
        self.assertEqual(inventory.from_string("-0.012375 USD"), residual)


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
        expected_balance.add_amount(A("-400 USD"))
        expected_balance.add_amount(
            A("10 HOOL"), position.Cost(D("40"), "USD", datetime.date(2014, 6, 5), None)
        )
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
        expected_balance.add_amount(A("2000.00 EUR"))
        expected_balance.add_amount(A("-3560.00 GBP"))
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
            if (
                isinstance(entry, data.Transaction)
                and entry.tags
                and "context" in entry.tags
            ):
                break
        balance_before, balance_after = interpolate.compute_entry_context(entries, entry)

        self.assertEqual(
            inventory.from_string("100.00 USD"), balance_before["Assets:Account1"]
        )
        self.assertEqual(
            inventory.from_string("80.00 USD"), balance_before["Assets:Account2"]
        )

        self.assertEqual(
            inventory.from_string("105.00 USD"), balance_after["Assets:Account1"]
        )
        self.assertEqual(
            inventory.from_string("75.00 USD"), balance_after["Assets:Account2"]
        )

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
        self.assertEqual({"USD": D("0.05")}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__ignore_price(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT @ 102.2340 USD
          Assets:Account4      -511.11 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({"USD": D("0.005")}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__ignore_cost(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT {102.2340 USD}
          Assets:Account4      -511.11 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({"USD": D("0.005")}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__ignore_cost_and_price(self, entries, errors, options_map):
        """
        2014-02-25 *
          Assets:Account3       5 VHT {102.2340 USD} @ 103.45237239 USD
          Assets:Account4      -511.11 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({"USD": D("0.005")}, tolerances)

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
        self.assertEqual({"VHT": D("0.0005")}, tolerances)
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map, True)
        self.assertEqual({"VHT": D("0.0005"), "USD": D("0.051117")}, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_tolerances__number_on_cost_used_overrides(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.111 VHT {102.2340 USD}
          Assets:Account4      -511.0 USD
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({"VHT": D("0.0005"), "USD": D("0.05")}, tolerances)
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map, True)
        self.assertEqual({"VHT": D("0.0005"), "USD": D("0.051117")}, tolerances)

    def test_tolerances__number_on_cost_fail_to_succ(self):
        # An example of a transaction that would fail without the inferred
        # tolerances and succeed with them.
        input_string = textwrap.dedent("""
          2014-01-01 open Assets:Account3
          2014-01-01 open Assets:Account4

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
        self.assertRegex(errors[0].message, "Transaction does not balance:.*0.20000 USD")

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
        self.assertEqual({"VHT": D("0.000005"), "USD": D("0.005")}, tolerances)

    @parser.parse_doc(allow_incomplete=True)
    def test_tolerances__with_inference(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.1111   VHT {102.2340 USD}
          Assets:Account4
        """
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map)
        self.assertEqual({"VHT": D("0.00005")}, tolerances)
        tolerances = interpolate.infer_tolerances(entries[0].postings, options_map, True)
        self.assertEqual({"VHT": D("0.00005"), "USD": D("0.005111700")}, tolerances)

    @parser.parse_doc(allow_incomplete=True)
    def test_tolerances__capped_inference(self, entries, _, options_map):
        """
        2014-02-25 *
          Assets:Account3       5.1   VHT {102.2340 USD}
          Assets:Account4
        """
        tolerances = interpolate.infer_tolerances(entries[-1].postings, options_map)
        self.assertEqual({"VHT": D("0.05")}, tolerances)
        tolerances = interpolate.infer_tolerances(entries[-1].postings, options_map, True)
        self.assertEqual({"VHT": D("0.05"), "USD": D("0.5")}, tolerances)

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
        transactions = [entry for entry in entries if isinstance(entry, data.Transaction)]
        self.assertEqual(
            {"USD": D("0.03096"), "VWELX": D("0.0005")},
            transactions[0].meta["__tolerances__"],
        )
        self.assertEqual(
            {"USD": D("0.04644"), "VWELX": D("0.0005")},
            transactions[1].meta["__tolerances__"],
        )
        self.assertEqual(
            {"USD": D("0.04644"), "VWELX": D("0.0005")},
            transactions[2].meta["__tolerances__"],
        )
        self.assertEqual(
            {"USD": D("0.247680"), "VWELX": D("0.0005")},
            transactions[3].meta["__tolerances__"],
        )
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
        transactions = [entry for entry in entries if isinstance(entry, data.Transaction)]
        self.assertEqual(
            {"USD": D("0.02568"), "VWELX": D("0.0005")},
            transactions[0].meta["__tolerances__"],
        )
        self.assertFalse(errors)

    @loader.load_doc()
    def test_tolerances__missing_units_only(self, entries, errors, options_map):
        """
        2017-01-01 open Assets:Checking USD
        2017-01-01 open Assets:Cash     CAD

        2017-06-23 * "Taking out cash from RBC machine"
          Assets:Checking     USD @ 1.32 CAD
          Assets:Cash     400 CAD
        """


class TestQuantize(unittest.TestCase):
    def test_quantize_with_tolerance(self):
        tolerances = defdict.ImmutableDictWithDefault(
            {"USD": D("0.01")}, default=D("0.000005")
        )
        self.assertEqual(
            D("100.12"),
            interpolate.quantize_with_tolerance(tolerances, "USD", D("100.123123123")),
        )
        self.assertEqual(
            D("100.12312"),
            interpolate.quantize_with_tolerance(tolerances, "CAD", D("100.123123123")),
        )

        tolerances = defdict.ImmutableDictWithDefault({"USD": D("0.01")}, default=ZERO)
        self.assertEqual(
            D("100.12"),
            interpolate.quantize_with_tolerance(tolerances, "USD", D("100.123123123")),
        )
        self.assertEqual(
            D("100.123123123"),
            interpolate.quantize_with_tolerance(tolerances, "CAD", D("100.123123123")),
        )


if __name__ == "__main__":
    unittest.main()
