__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
from unittest import mock
import os
import re
import subprocess
import sys
import textwrap
import unittest
import functools

from beancount.utils import test_utils
from beancount.ingest.importer import ImporterProtocol
from beancount.ingest import identify
from beancount.ingest import scripts_utils



identify_main = functools.partial(scripts_utils.trampoline_to_ingest, identify)


class _TestImporter(ImporterProtocol):

    def __init__(self, filename):
        self.filename = filename

    def identify(self, file):
        return file.name == self.filename


class TestScriptIdentifyFunctions(test_utils.TestTempdirMixin, unittest.TestCase):

    def test_find_imports(self):
        file1 = path.join(self.tempdir, 'file1.test')
        file2 = path.join(self.tempdir, 'file2.test')
        file3 = path.join(self.tempdir, 'file3.test')
        for filename in [file1, file2, file3]:
            open(filename, 'w')

        imp1a = _TestImporter(file1)
        imp1b = _TestImporter(file1)
        imp2 = _TestImporter(file2)

        config = [imp1a, imp1b, imp2]
        imports = list(identify.find_imports(config, self.tempdir))
        self.assertEqual([(file1, [imp1a, imp1b]),
                          (file2, [imp2]),
                          (file3, [])],
                         imports)

    @mock.patch.object(identify, 'FILE_TOO_LARGE_THRESHOLD', 128)
    def test_find_imports__file_too_large(self):
        file1 = path.join(self.tempdir, 'file1.test')
        file2 = path.join(self.tempdir, 'file2.test')
        with open(file1, 'w') as file:
            file.write('*' * 16)
        with open(file2, 'w') as file:
            file.write('*' * 256)

        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)

        imports = list(identify.find_imports([imp], self.tempdir))
        self.assertEqual([(file1, [imp])], imports)

    def test_find_imports__raises_exception(self):
        file1 = path.join(self.tempdir, 'file1.test')
        with open(file1, 'w'):
            pass
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(side_effect=ValueError("Unexpected error!"))
        imports = list(identify.find_imports([imp], self.tempdir))
        self.assertEqual([(file1, [])], imports)


class TestScriptIdentify(scripts_utils.TestScriptsBase):

    def test_identify(self):
        regexp = textwrap.dedent("""\
            \\*\\*\\*\\* .*/Downloads/ofxdownload.ofx
            Importer: +mybank-checking-ofx
            Account: +Assets:Checking

            \\*\\*\\*\\* .*/Downloads/Subdir/bank.csv
            Importer: +mybank-credit-csv
            Account: +Liabilities:CreditCard

            \\*\\*\\*\\* .*/Downloads/Subdir/readme.txt

            """).strip()

        # Invoke with new-style imports as script, with an ingest() call in the script.
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            env = os.environ.copy()
            env['PYTHONPATH'] = ':'.join(sys.path)
            output = subprocess.check_output(
                [path.join(self.tempdir, 'testimport.py'),
                 '--downloads', path.join(self.tempdir, 'Downloads'),
                 'identify'], shell=False, env=env)
        self.assertTrue(re.match(regexp, output.decode().strip()))

        # Invoke with old-style imports script via tool (with no ingest() call).
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(identify_main,
                                     [path.join(self.tempdir, 'test.import'),
                                      path.join(self.tempdir, 'Downloads')],
                                     identify.__file__)
        output = stdout.getvalue().strip()
        self.assertTrue(re.match(regexp, output))

        # Invoke with new-style imports script via tool (with an ingest() call).
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(identify_main,
                                     [path.join(self.tempdir, 'testimport.py'),
                                      path.join(self.tempdir, 'Downloads')],
                                     identify.__file__)
        output = stdout.getvalue().strip()
        self.assertTrue(re.match(regexp, output))

    def test_identify_examples(self):
        example_dir = path.join(
            test_utils.find_repository_root(__file__), 'examples', 'ingest')
        config_filename = path.join(example_dir, 'office', 'example.import')
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            result = test_utils.run_with_args(
                identify_main,
                [config_filename, path.join(example_dir, 'Downloads')],
                identify.__file__)

        self.assertEqual(0, result)
        output = stdout.getvalue()
        errors = stderr.getvalue()
        self.assertTrue(not errors or re.search('ERROR.*pdf2txt', errors))

        self.assertRegex(output, 'Downloads/UTrade20160215.csv')
        self.assertRegex(output, 'Importer:.*importers.utrade.utrade_csv.Importer')
        self.assertRegex(output, 'Account:.*Assets:US:UTrade')

        self.assertRegex(output, 'Downloads/ofxdownload.ofx')
        self.assertRegex(output, 'Importer:.*beancount.ingest.importers.ofx.Importer')
        self.assertRegex(output, 'Account:.*Liabilities:US:CreditCard')


if __name__ == '__main__':
    unittest.main()
