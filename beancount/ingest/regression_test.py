"""TD Ameritrade PDF statement importer.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import datetime
import unittest
import textwrap
import functools
import warnings
import sys
from unittest import mock
from os import path

from beancount.utils import test_utils
from beancount.parser import parser
from beancount.ingest import importer
from beancount.ingest import regression


def suppress_deprecation(fun):
    @functools.wraps(fun)
    def wrapped(*args, **kw):
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', module=r'.*\.regression',
                                    category=DeprecationWarning)
            return fun(*args, **kw)
    return wrapped


class _DummyImporter(importer.ImporterProtocol):
    "A dummy importer for testing."


class TestImporterTests(test_utils.TestTempdirMixin, unittest.TestCase):

    @suppress_deprecation
    @mock.patch('beancount.ingest.extract.extract_from_file')
    def test_test_expect_extract(self, extract_mock):
        importer = _DummyImporter()

        string = textwrap.dedent("""
          2013-01-28 * "Something"
            Assets:Bank:Checking  656.00 USD
            Expenses:BobaLife
        """)
        entry = parser.parse_one(string)

        filename = path.join(self.tempdir, 'input.csv')
        expect_filename = path.join(self.tempdir, 'input.csv.extract')
        with open(filename, 'w'):
            pass
        with open(expect_filename, 'w') as file:
            file.write(string)

        # Required to trigger the test creation.
        with mock.patch.object(importer.__class__, 'extract') as imp_meth:
            imp_meth.__func__ = True
            imp_meth.return_value = []

            # Test actual working import.
            extract_mock.return_value = [entry]
            for method, *args in regression.compare_sample_files(importer, self.tempdir):
                try:
                    method(*args)
                except Exception as exc:
                    self.fail("Unexpected exception: {}".format(exc))
            extract_mock.assert_called_once_with(filename, importer, None, None)

            # Check a missing file.
            extract_mock.reset_mock()
            os.remove(expect_filename)
            for method, *args in regression.compare_sample_files(importer, self.tempdir):
                with self.assertRaises(unittest.case.SkipTest):
                    method(*args)
            self.assertEqual(1, extract_mock.call_count)

    @suppress_deprecation
    def test_test_expect_file_date(self):
        importer = _DummyImporter()

        date = datetime.date(2013, 1, 28)
        filename = path.join(self.tempdir, 'input.csv')
        expect_filename = path.join(self.tempdir, 'input.csv.file_date')
        with open(filename, 'w'):
            pass
        with open(expect_filename, 'w') as file:
            file.write("{:%Y-%m-%d}\n".format(date))

        # Required to trigger the test creation.
        with mock.patch.object(importer.__class__, 'file_date') as imp_meth:
            imp_meth.__func__ = True
            imp_meth.return_value = date

            # Test actual working import.
            for method, *args in regression.compare_sample_files(importer, self.tempdir):
                try:
                    method(*args)
                except Exception as exc:
                    self.fail("Unexpected exception: {}".format(exc))
            self.assertEqual(1, imp_meth.call_count)

            # Check a missing file.
            imp_meth.reset_mock()
            os.remove(expect_filename)
            for method, *args in regression.compare_sample_files(importer, self.tempdir):
                with self.assertRaises(unittest.case.SkipTest):
                    method(*args)
            self.assertEqual(1, imp_meth.call_count)
            self.assertTrue(path.exists(expect_filename))

    @suppress_deprecation
    def test_test_expect_file_name(self):
        importer = _DummyImporter()

        filename = path.join(self.tempdir, 'input.csv')
        expect_filename = path.join(self.tempdir, 'input.csv.file_name')
        with open(filename, 'w'):
            pass
        renamed_filename = "renamed.csv"
        with open(expect_filename, 'w') as file:
            file.write("{}\n".format(renamed_filename))

        # Required to trigger the test creation.
        with mock.patch.object(importer.__class__, 'file_name') as imp_meth:
            imp_meth.__func__ = True
            imp_meth.return_value = renamed_filename

            # Test actual working import.
            for method, *args in regression.compare_sample_files(importer, self.tempdir):
                try:
                    method(*args)
                except Exception as exc:
                    self.fail("Unexpected exception: {}".format(exc))
            self.assertEqual(1, imp_meth.call_count)

            # Check a missing file.
            imp_meth.reset_mock()
            os.remove(expect_filename)
            for method, *args in regression.compare_sample_files(importer, self.tempdir):
                with self.assertRaises(unittest.case.SkipTest):
                    method(*args)
            self.assertEqual(1, imp_meth.call_count)
            self.assertTrue(path.exists(expect_filename))


class TestImporterTestGenerators(test_utils.TestTempdirMixin, unittest.TestCase):

    def test_find_input_files(self):
        for filename in ['something.extract',
                         'something.file_date',
                         'something.file_name',
                         'something.py',
                         'something.pyc',
                         'something.other']:
            open(path.join(self.tempdir, filename), 'w')
        files = list(regression.find_input_files(self.tempdir))
        self.assertEqual([path.join(self.tempdir, 'something.other')], files)

    @suppress_deprecation
    def test_compare_sample_files__no_directory(self):
        importer = _DummyImporter()
        this_module = sys.modules[type(importer).__module__]
        with mock.patch.object(this_module, '__file__', new=self.tempdir):
            with mock.patch.object(importer.__class__, 'file_date') as imp_meth:
                imp_meth.__func__ = True
                open(path.join(self.tempdir, 'something.csv'), 'w')
                open(path.join(self.tempdir, 'something.csv.file_date'), 'w')
                tests = list(regression.compare_sample_files(importer))
                self.assertEqual(1, len(tests))

    @suppress_deprecation
    def test_compare_sample_files__with_directory(self):
        importer = _DummyImporter()
        with mock.patch.object(importer.__class__, 'file_date') as imp_meth:
            imp_meth.__func__ = True
            filename = path.join(self.tempdir, 'something.csv')
            open(filename, 'w')
            open(path.join(self.tempdir, 'something.csv.file_date'), 'w')

            # Test with a directory.
            tests = list(regression.compare_sample_files(importer, self.tempdir))
            self.assertEqual(1, len(tests))

            # Test with a filename.
            tests = list(regression.compare_sample_files(importer, filename))
            self.assertEqual(1, len(tests))


if __name__ == '__main__':
    unittest.main()
