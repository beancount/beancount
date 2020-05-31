__copyright__ = "Copyright (C) 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
from unittest import mock
import re
import os
import logging
import datetime
import shutil
import unittest
import functools

from beancount.utils import test_utils
from beancount.utils import file_utils
from beancount.ingest import file
from beancount.ingest import scripts_utils


file_main = functools.partial(scripts_utils.trampoline_to_ingest, file)


class TestScriptFile(scripts_utils.TestScriptsBase, test_utils.TestCase):

    def setUp(self):
        super().setUp()
        self.downloads = path.join(self.tempdir, 'Downloads')
        self.documents = path.join(self.tempdir, 'Documents')
        os.mkdir(self.documents)

    @mock.patch.object(file, 'file')
    def test_file_main__default_output_dir(self, file_mock):
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(file_main, [
                '--dry-run',
                path.join(self.tempdir, 'test.import'),
                self.tempdir],
                                     file.__file__)
        self.assertEqual(self.tempdir, file_mock.call_args[0][2])

    def test_file_main__output_dir_does_not_exist(self):
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                test_utils.run_with_args(file_main, [
                    '--output', path.join(self.documents, "Bogus"),
                    path.join(self.tempdir, 'test.import'),
                    self.tempdir],
                                         file.__file__)

    def test_move_xdev_file(self):
        file.move_xdev_file(
            path.join(self.tempdir, 'Downloads/ofxdownload.ofx'),
            path.join(self.tempdir, 'other.ofx'),
            False)
        self.assertFalse(path.exists(path.join(self.tempdir, 'Downloads/ofxdownload.ofx')))
        self.assertTrue(path.exists(path.join(self.tempdir, 'other.ofx')))

        with self.assertRaises(OSError):
            file.move_xdev_file(
                path.join(self.tempdir, 'other.ofx'),
                path.join(self.tempdir, 'Some/New/Dir/File.ofx'),
                False)

        file.move_xdev_file(
            path.join(self.tempdir, 'other.ofx'),
            path.join(self.tempdir, 'Some/New/Dir/File.ofx'),
            True)
        self.assertFalse(path.exists(path.join(self.tempdir, 'other.ofx')))
        self.assertTrue(path.exists(path.join(self.tempdir, 'Some/New/Dir/File.ofx')))

    @mock.patch.object(file, 'move_xdev_file')
    def test_file__no_match(self, move_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=False)
        file.file([imp], self.downloads, self.documents)
        self.assertEqual(0, move_mock.call_count)

    @mock.patch.object(logging, 'warning')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__ambiguous_accounts(self, move_mock, error_mock):
        imp1 = mock.MagicMock()
        imp1.identify = mock.MagicMock(return_value=True)
        imp1.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp1.file_date = mock.MagicMock(return_value=datetime.date.today())
        imp1.file_name = mock.MagicMock(return_value='filename_account1')
        imp2 = mock.MagicMock()
        imp2.identify = mock.MagicMock(return_value=True)
        imp2.file_account = mock.MagicMock(return_value='Assets:Account2')
        imp2.file_date = mock.MagicMock(return_value=datetime.date.today())
        imp2.file_name = mock.MagicMock(return_value='filename_account2')
        file.file([imp1, imp2], self.downloads, self.documents)
        self.assertEqual(0, move_mock.call_count)
        self.assertEqual(3, error_mock.call_count)
        self.assertTrue(all(re.match('Ambiguous accounts', call[1][0])
                            for call in error_mock.mock_calls))

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__two_importers_same_accounts(self, move_mock, error_mock):
        account = 'Assets:Account1'
        imp1 = mock.MagicMock()
        imp1.identify = mock.MagicMock(return_value=True)
        imp1.file_account = mock.MagicMock(return_value=account)
        imp1.file_date = mock.MagicMock(return_value=None)
        imp1.file_name = mock.MagicMock(return_value=None)
        imp2 = mock.MagicMock()
        imp2.identify = mock.MagicMock(return_value=True)
        imp2.file_account = mock.MagicMock(return_value=account)
        imp2.file_name = mock.MagicMock(return_value=None)
        file.file([imp1, imp2], self.downloads, self.documents, mkdirs=True)
        self.assertEqual(3, move_mock.call_count)
        self.assertEqual(0, error_mock.call_count)

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__date_uses_mtime(self, move_mock, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=None)
        imp.file_name = mock.MagicMock(return_value=None)
        file.file([imp], path.join(self.downloads, 'ofxdownload.ofx'), self.documents,
                  mkdirs=True)
        self.assertEqual(1, move_mock.call_count)
        self.assertEqual(1, imp.file_date.call_count)
        dest_filename = path.basename(move_mock.mock_calls[0][1][1])
        self.assertEqual(
            '{0:%Y-%m-%d}.ofxdownload.ofx'.format(datetime.date.today()),
            dest_filename)

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__date_uses_extracted(self, move_mock, error_mock):
        date = datetime.date(2015, 2, 3)
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=date)
        imp.file_name = mock.MagicMock(return_value=None)
        file.file([imp], path.join(self.downloads, 'ofxdownload.ofx'), self.documents,
                  mkdirs=True)
        self.assertEqual(1, move_mock.call_count)
        self.assertEqual(1, imp.file_date.call_count)
        dest_filename = path.basename(move_mock.mock_calls[0][1][1])
        self.assertRegex(dest_filename, r'{}\.ofxdownload\.ofx'.format(date))

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__file_name(self, move_mock, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=None)
        imp.file_name = mock.MagicMock(return_value='final.ofx')
        file.file([imp], path.join(self.downloads, 'ofxdownload.ofx'), self.documents,
                  mkdirs=True)
        self.assertEqual(1, move_mock.call_count)
        self.assertEqual(1, imp.file_date.call_count)
        dest_filename = path.basename(move_mock.mock_calls[0][1][1])
        self.assertRegex(dest_filename, r'\d\d\d\d-\d\d-\d\d\.final\.ofx')

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__idify(self, move_mock, error_mock):
        filename = path.join(self.downloads, 'Some thing-To remember.OFX')
        with open(filename, 'w'):
            pass
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=None)
        imp.file_name = mock.MagicMock(return_value=None)
        file.file([imp], filename, self.documents, mkdirs=True, idify=True)
        self.assertEqual(1, move_mock.call_count)
        self.assertEqual(1, imp.file_date.call_count)
        dest_filename = path.basename(move_mock.mock_calls[0][1][1])
        self.assertEqual(
            '{0:%Y-%m-%d}.Some_thing-To_remember.OFX'.format(datetime.date.today()),
            dest_filename)

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__dest_dir_does_not_exist(self, move_mock, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=None)
        imp.file_name = mock.MagicMock(return_value=None)
        file.file([imp], self.downloads, self.documents)
        self.assertEqual(0, move_mock.call_count)
        self.assertEqual(3, error_mock.call_count)
        self.assertTrue(all(re.match('Destination directory .* does not exist', call[1][0])
                            for call in error_mock.mock_calls))

        move_mock.reset_mock()
        error_mock.reset_mock()

        file.file([imp], self.downloads, self.documents, mkdirs=True)
        self.assertEqual(3, move_mock.call_count)
        self.assertEqual(0, error_mock.call_count)

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__overwrite(self, move_mock, error_mock):
        date = datetime.date(2015, 1, 2)
        dest_filename = path.join(self.documents, 'Assets', 'Account1',
                                  '{0:%Y-%m-%d}.ofxdownload.ofx'.format(date))
        os.makedirs(path.dirname(dest_filename))
        with open(dest_filename, 'w'):
            pass
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=date)
        imp.file_name = mock.MagicMock(return_value=None)
        file.file([imp], path.join(self.downloads, 'ofxdownload.ofx'), self.documents)
        self.assertEqual(0, move_mock.call_count)
        self.assertEqual(1, error_mock.call_count)
        self.assertTrue(all(re.match('Destination file .* already exist', call[1][0])
                            for call in error_mock.mock_calls))

        move_mock.reset_mock()
        error_mock.reset_mock()

        file.file([imp], path.join(self.downloads, 'ofxdownload.ofx'), self.documents,
                  overwrite=True)
        self.assertEqual(1, move_mock.call_count)
        self.assertEqual(0, error_mock.call_count)

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__collision_in_renamed_files(self, move_mock, error_mock):
        destination = path.join(self.documents, 'Assets', 'Account1')
        os.makedirs(destination)

        # Make two different files with the same contents.
        file1 = path.join(self.downloads, 'ofxdownload.ofx')
        file2 = path.join(self.downloads, 'ofxdownload2.ofx')
        shutil.copyfile(file1, file2)

        # The importer matches both files, and attempts to move them to the same
        # destination filename; an error should be generated.
        date = datetime.date(2015, 1, 2)
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=date)
        imp.file_name = mock.MagicMock(return_value='mybank')
        file.file([imp], [file1, file2], self.documents)
        self.assertEqual(0, move_mock.call_count)
        self.assertEqual(1, error_mock.call_count)
        self.assertTrue(all(re.match('Collision in destination filenames', call[1][0])
                            for call in error_mock.mock_calls))

        move_mock.reset_mock()
        error_mock.reset_mock()

        # Test the case where the importer generates two distinct filenames via
        # file_name() while we're at it.
        imp.file_name = mock.MagicMock(side_effect=['bank1', 'bank2'])
        file.file([imp], [file1, file2], self.documents)
        self.assertEqual(2, move_mock.call_count)
        self.assertEqual(0, error_mock.call_count)

    @mock.patch.object(logging, 'error')
    @mock.patch.object(file, 'move_xdev_file')
    def test_file__dry_run(self, move_mock, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.file_account = mock.MagicMock(return_value='Assets:Account1')
        imp.file_date = mock.MagicMock(return_value=None)
        imp.file_name = mock.MagicMock(return_value=None)
        file.file([imp], self.downloads, self.documents, mkdirs=True, dry_run=True)
        self.assertEqual(0, error_mock.call_count)
        self.assertEqual(0, move_mock.call_count)

    @mock.patch.object(logging, 'error')
    def test_file__file_account_raises_exception(self, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        exc = ValueError("Unexpected error!")
        imp.file_account = mock.MagicMock(side_effect=exc)
        imp.name = mock.MagicMock(return_value="SomeImporter")
        file.file_one_file(path.join(self.downloads, 'ofxdownload.ofx'), [imp],
                           self.documents)

        self.assertEqual(2, error_mock.call_count)

        args = error_mock.mock_calls[0][1]
        self.assertRegex(args[0], 'raised an unexpected error')
        self.assertEqual(args[1], 'SomeImporter')
        self.assertEqual(args[2], exc)

        args = error_mock.mock_calls[1][1]
        self.assertRegex(args[0], 'No account provided by importers')

    @mock.patch.object(logging, 'error')
    def test_file__file_date_raises_exception(self, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        exc = ValueError("Unexpected error!")
        imp.file_date = mock.MagicMock(side_effect=exc)
        imp.file_name = mock.MagicMock(side_effect='ofxdownload.ofx')
        imp.file_account = mock.MagicMock(side_effect='dir')
        imp.name = mock.MagicMock(return_value="SomeImporter")
        file.file_one_file(path.join(self.downloads, 'ofxdownload.ofx'), [imp],
                           self.documents)
        self.assertEqual(1, error_mock.call_count)
        args = error_mock.mock_calls[0][1]
        self.assertRegex(args[0], 'raised an unexpected error')
        self.assertEqual(args[1], 'SomeImporter')
        self.assertEqual(args[2], exc)

    @mock.patch.object(logging, 'error')
    def test_file__file_name_raises_exception(self, error_mock):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        exc = ValueError("Unexpected error!")
        imp.file_date = mock.MagicMock(return_value=None)
        imp.file_name = mock.MagicMock(side_effect=exc)
        imp.file_account = mock.MagicMock(side_effect='dir')
        imp.name = mock.MagicMock(return_value="SomeImporter")
        file.file_one_file(path.join(self.downloads, 'ofxdownload.ofx'), [imp],
                           self.documents)
        self.assertEqual(1, error_mock.call_count)
        args = error_mock.mock_calls[0][1]
        self.assertRegex(args[0], 'raised an unexpected error')
        self.assertEqual(args[1], 'SomeImporter')
        self.assertEqual(args[2], exc)

    def test_file(self):
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(file_main, [
                '--output', self.documents,
                path.join(self.tempdir, 'test.import'),
                path.join(self.tempdir, 'Downloads')],
                                     file.__file__)
        expected_res = [
            path.join(self.documents, x)
            for x in [r'Liabilities/CreditCard/\d\d\d\d-\d\d-\d\d\.bank\.csv',
                      r'Assets/Checking/\d\d\d\d-\d\d-\d\d\.ofxdownload\.ofx']]
        moved_files = list(file_utils.find_files([self.documents]))
        for regexp in expected_res:
            self.assertTrue(any(re.match(regexp, filename) for filename in moved_files))

    def test_file_examples(self):
        config_filename = path.join(test_utils.find_repository_root(__file__),
                                    'examples', 'ingest', 'office', 'example.import')
        with test_utils.capture('stdout', 'stderr') as (_, stderr):
            result = test_utils.run_with_args(file_main, [
                config_filename,
                path.join(self.tempdir, 'Downloads'),
                '--output={}'.format(self.tempdir)],
                                              file.__file__)
        self.assertEqual(0, result)
        self.assertEqual("", stderr.getvalue())

        filed_files = []
        for root, dirs, files in os.walk(self.tempdir):
            filed_files.extend(files)
        self.assertEqual(5, len(filed_files))
        self.assertEqual(set(filed_files), set(['test.import',
                                                'ofxdownload.ofx',
                                                'bank.csv',
                                                'readme.txt',
                                                'testimport.py']))


if __name__ == '__main__':
    unittest.main()
