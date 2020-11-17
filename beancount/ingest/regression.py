"""Support for implementing regression tests on sample files using nose.

NOTE: This itself is not a regression test. It's a library used to create
regression tests for your importers. Use it like this in your own importer code:

   def test():
       importer = Importer([], {
           'FILE'  : 'Assets:US:MyBank:Main',
       })
       yield from regression.compare_sample_files(importer, __file__)

WARNING: This is deprecated. Nose itself has been deprecated for a while and
Beancount is now using only pytest. Ignore this and use
beancount.ingest.regression_ptest instead.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import io
import os
import re
import sys
import unittest
from os import path

from beancount.ingest.importer import ImporterProtocol
from beancount.parser import printer
from beancount.utils import test_utils
from beancount.utils.misc_utils import deprecated
from beancount.ingest import extract
from beancount.ingest import cache


class ToolNotInstalled(OSError):
    """An error to be used by converters when necessary software isn't there.

    Raising this exception from your converter code when the tool is not
    installed will make the tests defined in this file skipped instead of
    failing. This will happen when you test your converters on different
    computers and/or platforms.
    """


class ImportFileTestCase(unittest.TestCase):
    """Base class for importer tests that compare output to an expected output
    text."""

    # Allow for large diff output size.
    maxDiff = None

    def __init__(self, importer):
        super().__init__()
        self.importer = importer

    @test_utils.skipIfRaises(ToolNotInstalled)
    def test_expect_identify(self, filename, msg):
        """Attempt to identify a file and expect results to be true.

        Args:
          filename: A string, the name of the file to import using self.importer.
        Raises:
          AssertionError: If the contents differ from the expected file.
        """
        file = cache.get_file(filename)
        matched = self.importer.identify(file)
        self.assertTrue(matched)

    @test_utils.skipIfRaises(ToolNotInstalled)
    def test_expect_extract(self, filename, msg):
        """Extract entries from a test file and compare against expected output.

        If an expected file (as <filename>.extract) is not present, we issue a
        warning. Missing expected files can be written out by removing them
        before running the tests.

        Args:
          filename: A string, the name of the file to import using self.importer.
        Raises:
          AssertionError: If the contents differ from the expected file.

        """
        # Import the file.
        entries = extract.extract_from_file(filename, self.importer, None, None)

        # Render the entries to a string.
        oss = io.StringIO()
        printer.print_entries(entries, file=oss)
        string = oss.getvalue()

        expect_filename = '{}.extract'.format(filename)
        if path.exists(expect_filename):
            expect_string = open(expect_filename, encoding='utf-8').read()
            self.assertEqual(expect_string.strip(), string.strip())
        else:
            # Write out the expected file for review.
            open(expect_filename, 'w', encoding='utf-8').write(string)
            self.skipTest("Expected file not present; generating '{}'".format(
                expect_filename))

    @test_utils.skipIfRaises(ToolNotInstalled)
    def test_expect_file_date(self, filename, msg):
        """Compute the imported file date and compare to an expected output.

        If an expected file (as <filename>.file_date) is not present, we issue a
        warning. Missing expected files can be written out by removing them
        before running the tests.

        Args:
          filename: A string, the name of the file to import using self.importer.
        Raises:
          AssertionError: If the contents differ from the expected file.
        """
        # Import the date.
        file = cache.get_file(filename)
        date = self.importer.file_date(file)
        if date is None:
            self.fail("No date produced from {}".format(file.name))

        expect_filename = '{}.file_date'.format(file.name)
        if path.exists(expect_filename) and path.getsize(expect_filename) > 0:
            expect_date_str = open(expect_filename, encoding='utf-8').read().strip()
            expect_date = datetime.datetime.strptime(expect_date_str, '%Y-%m-%d').date()
            self.assertEqual(expect_date, date)
        else:
            # Write out the expected file for review.
            with open(expect_filename, 'w', encoding='utf-8') as outfile:
                print(date.strftime('%Y-%m-%d'), file=outfile)
            self.skipTest("Expected file not present; generating '{}'".format(
                expect_filename))

    @test_utils.skipIfRaises(ToolNotInstalled)
    def test_expect_file_name(self, filename, msg):
        """Compute the imported file name and compare to an expected output.

        If an expected file (as <filename>.file_name) is not present, we issue a
        warning. Missing expected files can be written out by removing them
        before running the tests.

        Args:
          filename: A string, the name of the file to import using self.importer.
        Raises:
          AssertionError: If the contents differ from the expected file.
        """
        # Import the date.
        file = cache.get_file(filename)
        generated_basename = self.importer.file_name(file)
        if generated_basename is None:
            self.fail("No filename produced from {}".format(filename))

        # Check that we're getting a non-null relative simple filename.
        self.assertFalse(path.isabs(generated_basename), generated_basename)
        self.assertNotRegex(generated_basename, os.sep)

        expect_filename = '{}.file_name'.format(file.name)
        if path.exists(expect_filename) and path.getsize(expect_filename) > 0:
            expect_filename = open(expect_filename, encoding='utf-8').read().strip()
            self.assertEqual(expect_filename, generated_basename)
        else:
            # Write out the expected file for review.
            with open(expect_filename, 'w', encoding='utf-8') as file:
                print(generated_basename, file=file)
            self.skipTest("Expected file not present; generating '{}'".format(
                expect_filename))


def find_input_files(directory):
    """Find the input files in the module where the class is defined.

    Args:
      directory: A string, the path to a root directory to check for.
    Yields:
      Strings, the absolute filenames of sample input and expected files.
    """
    for sroot, dirs, files in os.walk(directory):
        for filename in files:
            if re.match(r'.*\.(extract|file_date|file_name|py|pyc|DS_Store)$', filename):
                continue
            yield path.join(sroot, filename)


@deprecated("Use beancount.ingest.regression_pytest instead")
def compare_sample_files(importer, directory=None, ignore_cls=None):
    """Compare the sample files under a directory.

    Args:
      importer: An instance of an Importer.
      directory: A string, the directory to scour for sample files or a filename
          in that directory. If a directory is not provided, the directory of
          the file from which the importer class is defined is used.
      ignore_cls: An optional base class of the importer whose methods should
        not trigger the addition of a test. For example, if you are deriving
        from a base class which is already well-tested, you may not want to have
        a regression test case generated for those methods. This was used to
        ignore methods provided from a common backwards compatibility support
        class.
    Yields:
      Generated tests as per nose's requirements (a callable and arguments for
      it).
    """
    # If the directory is not specified, use the directory where the importer
    # class was defined.
    if not directory:
        directory = sys.modules[type(importer).__module__].__file__
    if path.isfile(directory):
        directory = path.dirname(directory)

    for filename in find_input_files(directory):
        # For each of the methods to be tested, check if there is an actual
        # implementation and if so, run a comparison with an expected file.
        for name in ['identify',
                     'extract',
                     'file_date',
                     'file_name']:
            # Check if the method has been overridden from the protocol
            # interface. If so, even if it's provided by concretely inherited
            # method, we want to require a test against that method.
            func = getattr(importer, name).__func__
            if (func is not getattr(ImporterProtocol, name) and
                (ignore_cls is None or (func is not getattr(ignore_cls, name, None)))):
                method = getattr(ImportFileTestCase(importer),
                                 'test_expect_{}'.format(name))
                yield (method, filename, name)
