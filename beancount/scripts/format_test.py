__copyright__ = "Copyright (C) 2014-2022, 2024-2025  Martin Blais"
__license__ = "GNU GPLv2"

import io
import os
import stat
import tempfile
import textwrap
import unittest
from pathlib import Path
from unittest import mock

from click.testing import CliRunner

from beancount.scripts import format
from beancount.utils import test_utils


class TestFormatWrites(test_utils.ClickTestCase):
    def setUp(self):
        super().setUp()
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.directory = Path(directory.name)
        self.filename = self.directory / "ledger.beancount"
        self.contents = '2024-01-01 * "Lunch"\n  Expenses:Food 10 USD\n  Assets:Cash\n'
        self.filename.write_text(self.contents, encoding="utf-8")
        self.arguments = (
            [str(self.filename), "--in-place"],
            [str(self.filename), "--output", str(self.filename)],
        )

    def test_successful_replacement(self):
        for arguments in self.arguments:
            with self.subTest(arguments=arguments):
                self.filename.write_text(self.contents, encoding="utf-8")
                self.run_with_args(format.main, *arguments)
                self.assertEqual(
                    self.filename.read_text(encoding="utf-8"),
                    format.align_beancount(self.contents),
                )

    def test_failed_write_preserves_input(self):
        for written in (0, 10):
            with self.subTest(written=written):
                self.assert_failed_output_preserves_input("write", written)

    def test_failed_close_preserves_input(self):
        self.assert_failed_output_preserves_input("close")

    def assert_failed_output_preserves_input(self, failure, written=10):
        real_open = io.open

        def failing_open(filename, mode="r", *args, **kwargs):
            stream = real_open(filename, mode, *args, **kwargs)
            if "w" not in mode:
                return stream
            self.addCleanup(stream.close)
            wrapper = mock.MagicMock(wraps=stream)
            wrapper.__enter__.return_value = wrapper
            wrapper.__exit__.side_effect = stream.__exit__

            def write(contents):
                stream.write(contents[:written])
                stream.flush()
                raise OSError("Simulated write failure")

            def close(*args):
                if not stream.closed:
                    stream.close()
                    raise OSError("Simulated close failure")

            if failure == "write":
                wrapper.write.side_effect = write
            else:
                wrapper.close.side_effect = close
                wrapper.__exit__.side_effect = close
            return wrapper

        for arguments in self.arguments:
            with self.subTest(arguments=arguments):
                self.filename.write_text(self.contents, encoding="utf-8")
                with (
                    mock.patch("builtins.open", side_effect=failing_open),
                    mock.patch("io.open", side_effect=failing_open),
                ):
                    result = CliRunner().invoke(format.main, arguments)
                self.assertNotEqual(result.exit_code, 0)
                self.assertIn(f"Simulated {failure} failure", str(result.exception))
                self.assertEqual(self.filename.read_text(encoding="utf-8"), self.contents)
                self.assertEqual(list(self.directory.iterdir()), [self.filename])

    def test_failed_replace_preserves_input(self):
        for arguments in self.arguments:
            with self.subTest(arguments=arguments):
                self.filename.write_text(self.contents, encoding="utf-8")
                with mock.patch("os.replace", side_effect=OSError("Replace failed")):
                    result = CliRunner().invoke(format.main, arguments)
                self.assertNotEqual(result.exit_code, 0)
                self.assertEqual(self.filename.read_text(encoding="utf-8"), self.contents)
                self.assertEqual(list(self.directory.iterdir()), [self.filename])

    def test_separate_output(self):
        output = self.directory / "formatted.beancount"
        self.run_with_args(format.main, str(self.filename), "--output", str(output))
        self.assertEqual(
            output.read_text(encoding="utf-8"), format.align_beancount(self.contents)
        )
        self.assertEqual(self.filename.read_text(encoding="utf-8"), self.contents)

    def test_multiple_files(self):
        other = self.directory / "other.beancount"
        other.write_text(self.contents, encoding="utf-8")
        self.run_with_args(format.main, str(self.filename), str(other), "--in-place")
        for filename in (self.filename, other):
            self.assertEqual(
                filename.read_text(encoding="utf-8"), format.align_beancount(self.contents)
            )

    def test_standard_streams(self):
        result = CliRunner().invoke(format.main, ["-"], input=self.contents)
        self.assertEqual(result.exit_code, 0)
        self.assertEqual(result.stdout, format.align_beancount(self.contents))

    def test_device_output(self):
        self.run_with_args(format.main, str(self.filename), "--output", os.devnull)
        self.assertEqual(self.filename.read_text(encoding="utf-8"), self.contents)

    def test_standard_input_cannot_be_edited_in_place(self):
        result = CliRunner().invoke(format.main, ["-", "--in-place"], input=self.contents)
        self.assertEqual(result.exit_code, 2)
        self.assertIn("Cannot format standard input in place", result.output)

    def test_read_only_input(self):
        self.filename.chmod(stat.S_IREAD)
        self.addCleanup(self.filename.chmod, stat.S_IREAD | stat.S_IWRITE)
        if os.access(self.filename, os.W_OK):
            self.skipTest("Current user can write read-only files")
        for arguments in self.arguments:
            with self.subTest(arguments=arguments):
                result = CliRunner().invoke(format.main, arguments)
                self.assertNotEqual(result.exit_code, 0)
                self.assertEqual(self.filename.read_text(encoding="utf-8"), self.contents)

    @unittest.skipIf(os.name == "nt", "POSIX permissions")
    def test_preserves_permissions(self):
        self.filename.chmod(0o640)
        for arguments in self.arguments:
            with self.subTest(arguments=arguments):
                self.run_with_args(format.main, *arguments)
                self.assertEqual(stat.S_IMODE(self.filename.stat().st_mode), 0o640)

    @unittest.skipIf(os.name == "nt", "POSIX permissions")
    def test_new_output_uses_umask(self):
        output = self.directory / "formatted.beancount"
        self.addCleanup(os.umask, os.umask(0o022))
        self.run_with_args(format.main, str(self.filename), "--output", str(output))
        self.assertEqual(stat.S_IMODE(output.stat().st_mode), 0o644)

    @unittest.skipIf(os.name == "nt", "POSIX permissions")
    def test_write_only_output(self):
        output = self.directory / "formatted.beancount"
        output.write_text("Previous contents", encoding="utf-8")
        output.chmod(0o200)
        self.addCleanup(output.chmod, 0o600)
        if os.access(output, os.R_OK):
            self.skipTest("Current user can read write-only files")
        self.run_with_args(format.main, str(self.filename), "--output", str(output))
        self.assertEqual(stat.S_IMODE(output.stat().st_mode), 0o200)
        output.chmod(0o600)
        self.assertEqual(
            output.read_text(encoding="utf-8"), format.align_beancount(self.contents)
        )

    def test_preserves_symlink(self):
        link = self.directory / "linked.beancount"
        try:
            link.symlink_to(self.filename.name)
        except OSError as exc:
            self.skipTest(f"Cannot create a symlink: {exc}")
        for options in (["--in-place"], ["--output", str(link)]):
            with self.subTest(options=options):
                self.run_with_args(format.main, str(link), *options)
                self.assertTrue(link.is_symlink())
                self.assertEqual(
                    self.filename.read_text(encoding="utf-8"),
                    format.align_beancount(self.contents),
                )

    def test_hard_link_keeps_previous_contents(self):
        link = self.directory / "linked.beancount"
        try:
            os.link(self.filename, link)
        except OSError as exc:
            self.skipTest(f"Cannot create a hard link: {exc}")
        self.run_with_args(format.main, *self.arguments[0])
        self.assertEqual(link.read_text(encoding="utf-8"), self.contents)
        self.assertEqual(
            self.filename.read_text(encoding="utf-8"), format.align_beancount(self.contents)
        )


class TestScriptFormat(test_utils.ClickTestCase):
    @test_utils.docfile
    def test_success(self, filename):
        """

        * Section header

        ;; Accounts (comments)
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash

        2014-03-05 balance   Assets:Cash  -50.02 USD

        2014-03-10 * "Something"
          Assets:Other   10 HOOL {500.23} USD ; Bla
          Assets:Cash

        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""

          * Section header

          ;; Accounts (comments)
          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant              50.02 USD
            Assets:Cash

          2014-03-05 balance   Assets:Cash  -50.02 USD

          2014-03-10 * "Something"
            Assets:Other                        10 HOOL {500.23} USD ; Bla
            Assets:Cash

        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_align_posting_starts(self, filename):
        """
        2014-03-01 * "Something"
          Expenses:Restaurant   50.01 USD
          Assets:Cash

        2014-03-02 * "Something"
         Expenses:Restaurant    50.02 USD
            Assets:Cash

        2014-03-03 * "Something"
          Expenses:Restaurant   50.03 USD
          Assets:Cash
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
          2014-03-01 * "Something"
            Expenses:Restaurant  50.01 USD
            Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant  50.02 USD
            Assets:Cash

          2014-03-03 * "Something"
            Expenses:Restaurant  50.03 USD
            Assets:Cash
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_open_only_issue80(self, filename):
        """
        2015-07-16 open Assets:BoA:checking USD
        """
        _result = self.run_with_args(format.main, filename)
        with open(filename, encoding="utf-8") as infile:
            actual = infile.read()
        self.assertEqual(
            """
          2015-07-16 open Assets:BoA:checking USD
        """.strip(),
            actual.strip(),
        )

    @test_utils.docfile
    def test_commas(self, filename):
        """

        * Section header

        ;; Accounts (comments)
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   1,050.02 USD
          Assets:Cash

        2014-03-05 balance   Assets:Cash  -1,050.02 USD

        2014-03-10 * "Something"
          Assets:Other   10 HOOL {5,000.23 USD} ; Bla
          Assets:Cash

        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""

          * Section header

          ;; Accounts (comments)
          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant              1,050.02 USD
            Assets:Cash

          2014-03-05 balance   Assets:Cash  -1,050.02 USD

          2014-03-10 * "Something"
            Assets:Other                           10 HOOL {5,000.23 USD} ; Bla
            Assets:Cash

        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_currency_issue146(self, filename):
        """
        1970-01-01 open Equity:Opening-balances
        1970-01-01 open Assets:Investments

        2014-03-31 * "opening"
          Assets:Investments                 1.23 FOO_BAR
          Equity:Opening-balances
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
          1970-01-01 open Equity:Opening-balances
          1970-01-01 open Assets:Investments

          2014-03-31 * "opening"
            Assets:Investments  1.23 FOO_BAR
            Equity:Opening-balances
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_fixed_width(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.00 USD
          Assets:Test
        """
        result = self.run_with_args(format.main, filename, "--prefix-width=40")
        self.assertEqual(
            textwrap.dedent("""
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test                           10.00 USD
          Assets:Test
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_fixed_column(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test
        2016-08-01 balance Assets:Test  0.00 USD

        2016-08-02 * "" ""
          Expenses:Test     10.00 USD
          Assets:Test
        """
        result = self.run_with_args(format.main, filename, "--currency-column=50")
        self.assertEqual(
            textwrap.dedent("""
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test
        2016-08-01 balance Assets:Test              0.00 USD

        2016-08-02 * "" ""
          Expenses:Test                            10.00 USD
          Assets:Test
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_metadata_issue400(self, filename):
        """
        2020-01-01 open Assets:Test

        2020-11-10 * Test
          payment_amount: 20.00 EUR
          Assets:Test   10.00 EUR
          Assets:Test  -10.00 EUR
        """
        result = self.run_with_args(format.main, filename, "--currency-column=50")
        self.assertEqual(
            textwrap.dedent("""
        2020-01-01 open Assets:Test

        2020-11-10 * Test
          payment_amount: 20.00 EUR
          Assets:Test                              10.00 EUR
          Assets:Test                             -10.00 EUR
        """),
            result.stdout,
        )

    @unittest.skip(
        "Eventually we will want to support arithmetic expressions. "
        "It will require to invoke the expression parser because "
        "expressions are not guaranteed to be surrounded by matching "
        "parentheses."
    )
    @test_utils.docfile
    def test_arithmetic_expressions(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.0/2 USD
          Assets:Test
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""

        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.0/2 USD
          Assets:Test

        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_parenthesized_binary_expressions(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.0 USD
          Expenses:Test (10.0/2) USD
          Expenses:Test (10.0+2) USD
          Expenses:Test (10.0-2) USD
          Expenses:Test (10.0*2) USD
          Expenses:Test (-10.0*+2) USD
          Expenses:Test (-1,000.0*+2.0) USD
          Assets:Test

        2016-08-03 balance Assets:Test 12.27 USD
        2016-08-03 balance Assets:Test (12.27 + 1.00) USD
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test                            10.0 USD
          Expenses:Test                        (10.0/2) USD
          Expenses:Test                        (10.0+2) USD
          Expenses:Test                        (10.0-2) USD
          Expenses:Test                        (10.0*2) USD
          Expenses:Test                      (-10.0*+2) USD
          Expenses:Test                 (-1,000.0*+2.0) USD
          Assets:Test

        2016-08-03 balance Assets:Test            12.27 USD
        2016-08-03 balance Assets:Test   (12.27 + 1.00) USD
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_multiline_narration_like_account(self, filename):
        """
        2023-02-02 * "payee" "multiline
        1                                                                                   :    1234 TEXT"
          Assets:BIBEssen:Checking                                                             -39.99 EUR
          Assets:ZeroSum:Transfers                                                              39.99 EUR
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
        2023-02-02 * "payee" "multiline
        1                                                                                   :    1234 TEXT"
          Assets:BIBEssen:Checking  -39.99 EUR
          Assets:ZeroSum:Transfers   39.99 EUR
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_escaped_quote_in_narration(self, filename):
        """
        2023-02-02 * "narration with \\" quote"
          Assets:Checking                                                             -39.99 EUR
          Expenses:Test                                                                39.99 EUR
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
        2023-02-02 * "narration with \\" quote"
          Assets:Checking  -39.99 EUR
          Expenses:Test     39.99 EUR
        """),
            result.stdout,
        )


if __name__ == "__main__":
    unittest.main()
