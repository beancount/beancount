"""Support for implementing regression tests on sample files using pytest.

This module provides definitions for testing a custom importer against a set of
existing downloaded files, running the various importer interface methods on it,
and comparing the output to an expected text file. (Expected test files can be
auto-generated using the --generate option). You use it like this:

  from beancount.ingest import regression_pytest
  ...
  import mymodule
  ...

  # Create your importer instance used for testing.
  importer = mymodule.Importer(...)

  # Select a directory where your test files are to be located.
  directory = ...

  # Create a test case using the base in this class.

  @regression_pytest.with_importer(importer)
  @regression_pytest.with_testdir(directory)
  class TestImporter(regtest.ImporterTestBase):
      pass

Also, to add the --generate option to 'pytest', you must create a conftest.py
somewhere in one of the roots above your importers with this module as a plugin:

  pytest_plugins = "beancount.ingest.regression_pytest"

See beancount/example/ingest for a full working example.

How to invoke the tests:

Via pytest. First run your test with the --generate option to generate all the
expected files. Then inspect them visually for correctness. Finally, check them
in to preserve them. You should be able to regress against those correct outputs
in the future. Use version control to your advantage to visualize the
differences.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import os
import re
import io
import pytest

from beancount.ingest import cache
from beancount.ingest import extract
from beancount.parser import printer


def pytest_addoption(parser):
    """Add an option to generate the expected files for the tests."""
    group = parser.getgroup("beancount")
    group.addoption("--generate", "--gen", action="store_true",
                    help="Don't test; rather, generate the expected files")


def with_importer(importer):
    """Parametrizing fixture that provides the importer to test."""
    return pytest.mark.parametrize("importer", [importer])


def with_testdir(directory):
    """Parametrizing fixture that provides files from a directory."""
    return pytest.mark.parametrize(
        "file", [cache.get_file(fn) for fn in find_input_files(directory)])


def find_input_files(directory):
    """Find the input files in the module where the class is defined.

    Args:
      directory: A string, the path to a root directory to check for.
    Yields:
      Strings, the absolute filenames of sample input and expected files.
    """
    for sroot, dirs, files in os.walk(directory):
        for filename in files:
            if re.match(r'.*\.(extract|file_date|file_name|file_account|py|pyc|DS_Store)$',
                        filename):
                continue
            yield path.join(sroot, filename)


def compare_contents_or_generate(actual_string, expect_fn, generate):
    """Compare a string to the contents of an expect file.

    Assert if different; auto-generate otherwise.

    Args:
      actual_string: The expected string contents.
      expect_fn: The filename whose contents to read and compare against.
      generate: A boolean, true if we are to generate the tests.
    """
    if generate:
        with open(expect_fn, 'w', encoding='utf-8') as expect_file:
            expect_file.write(actual_string)
            if actual_string and not actual_string.endswith('\n'):
                expect_file.write('\n')
        pytest.skip("Generated '{}'".format(expect_fn))
    else:
        # Run the test on an existing expected file.
        assert path.exists(expect_fn), (
            "Expected file '{}' is missing. Generate it?".format(expect_fn))
        with open(expect_fn, encoding='utf-8') as infile:
            expect_string = infile.read()
        assert expect_string.strip() == actual_string.strip()


class ImporterTestBase:

    def test_identify(self, importer, file):
        """Attempt to identify a file and expect results to be true.

        This method does not need to check against an existing expect file. It
        is just assumed it should return True if your test is setup well (the
        importer should always identify the test file).
        """
        assert importer.identify(file)

    def test_extract(self, importer, file, pytestconfig):
        """Extract entries from a test file and compare against expected output."""
        entries = extract.extract_from_file(file.name, importer, None, None)
        oss = io.StringIO()
        printer.print_entries(entries, file=oss)
        string = oss.getvalue()
        compare_contents_or_generate(string, '{}.extract'.format(file.name),
                                     pytestconfig.getoption("generate", False))

    def test_file_date(self, importer, file, pytestconfig):
        """Compute the imported file date and compare to an expected output."""
        date = importer.file_date(file)
        string = date.isoformat() if date else ''
        compare_contents_or_generate(string, '{}.file_date'.format(file.name),
                                     pytestconfig.getoption("generate", False))

    def test_file_name(self, importer, file, pytestconfig):
        """Compute the imported file name and compare to an expected output."""
        filename = importer.file_name(file) or ''
        compare_contents_or_generate(filename, '{}.file_name'.format(file.name),
                                     pytestconfig.getoption("generate", False))

    def test_file_account(self, importer, file, pytestconfig):
        """Compute the selected filing account and compare to an expected output."""
        account = importer.file_account(file) or ''
        compare_contents_or_generate(account, '{}.file_account'.format(file.name),
                                     pytestconfig.getoption("generate", False))
