__copyright__ = "Copyright (C) 2014, 2016-2017, 2019, 2021, 2024-2025  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import random
import tempfile
import textwrap
import unittest
from os import path

import click.testing

from beancount import loader
from beancount.core import data
from beancount.core import getters
from beancount.core.number import D
from beancount.ops import validation
from beancount.scripts import example
from beancount.utils import test_utils


class TestScriptExample(test_utils.ClickTestCase):
    def test_generate(self):
        # For some reason rv.stdout includes stderr output when run from Bazel,
        # so you have to disable that from the program.
        rv = self.run_with_args(example.main)
        self.assertTrue(rv.stdout)

        loaded_entries, errors, _ = loader.load_string(
            rv.stdout, extra_validations=validation.HARDCORE_VALIDATIONS
        )
        self.assertFalse(errors)

    def test_generate_with_date(self):
        # For some reason rv.stdout includes stderr output when run from Bazel,
        # so you have to disable that from the program.
        rv = self.run_with_args(example.main, "--date-birth", "1988-07-01")
        self.assertTrue(rv.stdout)

        loaded_entries, errors, _ = loader.load_string(
            rv.stdout, extra_validations=validation.HARDCORE_VALIDATIONS
        )
        self.assertFalse(errors)

    def test_generate_density(self):
        args = ("--seed", "1", "--date-begin", "2024-01-01", "--date-end", "2025-01-01")
        rv_default = self.run_with_args(example.main, *args)
        rv_one = self.run_with_args(example.main, *args, "--density", "1")
        self.assertEqual(rv_default.stdout, rv_one.stdout)

        rv = self.run_with_args(example.main, *args, "--density", "5")
        entries, errors, _ = loader.load_string(
            rv.stdout, extra_validations=validation.HARDCORE_VALIDATIONS
        )
        self.assertFalse(errors)
        default_entries, _, _ = loader.load_string(rv_default.stdout)
        self.assertGreater(
            len(list(data.filter_txns(entries))),
            2 * len(list(data.filter_txns(default_entries))),
        )

    def test_generate_density_invalid(self):
        runner = click.testing.CliRunner()
        for density in "0", "-1", "1e6":
            rv = runner.invoke(example.main, ["--density", density])
            self.assertEqual(2, rv.exit_code)
            self.assertIn("Invalid value for '--density'", rv.output)

    def test_generate_households(self):
        rv = self.run_with_args(
            example.main,
            *("--seed", "2", "--date-begin", "2024-01-01", "--date-end", "2025-01-01"),
            *("--households", "2", "--density", "0.5"),
        )
        entries, errors, _ = loader.load_string(
            rv.stdout, extra_validations=validation.HARDCORE_VALIDATIONS
        )
        self.assertFalse(errors)
        accounts = getters.get_accounts(entries)
        self.assertIn("Assets:H1:US:BofA:Checking", accounts)
        self.assertIn("Assets:H2:US:BofA:Checking", accounts)
        self.assertNotIn("Assets:US:BofA:Checking", accounts)
        self.assertEqual(1, rv.stdout.count("\n* Prices\n"))

    def test_generate_transactions(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            filename = path.join(tmpdir, "example.beancount")
            rv = self.run_with_args(
                example.main,
                *("--seed", "3", "--date-begin", "2023-01-01", "--date-end", "2025-01-01"),
                *("--transactions", "4000", "--no-validate", "--output", filename),
            )
            entries, errors, _ = loader.load_file(filename)
        self.assertFalse(errors)
        num_transactions = len(list(data.filter_txns(entries)))
        self.assertIn(f"Generated {num_transactions} transactions", rv.output)
        self.assertLess(abs(num_transactions / 4000 - 1), 0.25)

    def test_generate_transactions_invalid(self):
        runner = click.testing.CliRunner()
        rv = runner.invoke(example.main, ["--transactions", "10"])
        self.assertEqual(2, rv.exit_code)
        self.assertIn("Too few transactions", rv.output)
        rv = runner.invoke(example.main, ["--transactions", "10000", "--density", "2"])
        self.assertEqual(2, rv.exit_code)
        self.assertIn("mutually exclusive", rv.output)
        rv = runner.invoke(example.main, ["--transactions", "1000000000"])
        self.assertEqual(2, rv.exit_code)
        self.assertIn("Too many transactions", rv.output)
        for date_end in "2024-01-01", "2023-06-01":
            rv = runner.invoke(
                example.main,
                [
                    "--date-begin",
                    "2024-01-01",
                    "--date-end",
                    date_end,
                    "--transactions",
                    "100",
                ],
            )
            self.assertEqual(2, rv.exit_code)
            self.assertIn("must be before the end date", rv.output)


class TestExampleHelpers(unittest.TestCase):
    def test_date_random_seq_density(self):
        date_begin = datetime.date(2020, 1, 1)
        date_end = datetime.date(2021, 1, 1)
        random.seed(0)
        dates = list(example.date_random_seq(date_begin, date_end, 1, 5))
        random.seed(0)
        self.assertEqual(
            dates, list(example.date_random_seq(date_begin, date_end, 1, 5, 1.0))
        )
        dates_dense = list(example.date_random_seq(date_begin, date_end, 1, 5, 10.0))
        self.assertGreater(len(dates_dense), 8 * len(dates))
        self.assertEqual(dates_dense, sorted(dates_dense))
        self.assertTrue(all(date_begin <= date < date_end for date in dates_dense))

    def test_date_random_seq_tiny_density(self):
        date_begin = datetime.date(2020, 1, 1)
        date_end = datetime.date(2021, 1, 1)
        self.assertEqual(
            [], list(example.date_random_seq(date_begin, date_end, 1, 5, 1e-6))
        )
        self.assertEqual(
            [], list(example.date_random_seq(date_begin, date_end, 1, 5, 1e-300))
        )

    def test_random_count(self):
        random.seed(0)
        self.assertTrue(all(example.random_count(0.0, 1.0) == 0 for _ in range(100)))
        self.assertTrue(all(example.random_count(1.0, 3.0) == 3 for _ in range(100)))
        total = sum(example.random_count(0.3, 2.5) for _ in range(10000))
        self.assertAlmostEqual(0.75, total / 10000, delta=0.05)

    def test_scale_amount(self):
        self.assertEqual(D("12.34"), example.scale_amount(D("12.34"), 1))
        self.assertAlmostEqual(1.5, example.scale_amount(15.0, 10.0))
        self.assertEqual(0.01, example.scale_amount(15.0, 10000.0))
        self.assertAlmostEqual(-1.5, example.scale_amount(-15.0, 10.0))
        self.assertEqual(-0.01, example.scale_amount(-15.0, 10000.0))
        self.assertEqual(-15.0, example.scale_amount(-15.0, 1))

    def test_prefix_accounts(self):
        self.assertEqual(
            textwrap.dedent("""\
              2020-01-01 open Assets:H7:US:BofA:Checking
              2020-01-02 * "Equity: not an account" "Income"
                Expenses:H7:Food:Restaurant   1.00 USD
                Liabilities:H7:US:Chase:Slate
            """),
            example.prefix_accounts(
                textwrap.dedent("""\
                  2020-01-01 open Assets:US:BofA:Checking
                  2020-01-02 * "Equity: not an account" "Income"
                    Expenses:Food:Restaurant   1.00 USD
                    Liabilities:US:Chase:Slate
                """),
                "H7",
            ),
        )


if __name__ == "__main__":
    unittest.main()
