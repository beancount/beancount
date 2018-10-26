__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import argparse
import os
import unittest

from beancount.utils import test_utils
from beancount.ingest import scripts_utils
from beancount.ingest import extract


def run(args, parser, importers_list, files_or_directories):
    pass


class TestParseArguments(scripts_utils.TestScriptsBase):

    def test_test_scripts_base(self):
        self.assertTrue(path.exists(self.tempdir))
        self.assertTrue(any(filename.endswith('.import')
                            for filename in os.listdir(self.tempdir)))

    def test_parse_arguments__insufficient(self):
        # Test with insufficient arguments.
        parser = scripts_utils.create_legacy_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                parser.parse_args(args=[])

        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                parser.parse_args(args=[path.join(self.tempdir, 'test.import')])

    def test_parse_arguments__invalid(self):
        parser = scripts_utils.create_legacy_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                scripts_utils.run_import_script_and_ingest(parser, argv=[
                    path.join(self.tempdir, 'test.import'),
                    path.join(self.tempdir, 'Non-existent'),
                ])

    def test_parse_arguments__sufficient(self):
        parser = scripts_utils.create_legacy_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            args = parser.parse_args(args=[
                path.join(self.tempdir, 'test.import'),
                path.join(self.tempdir, 'Downloads'),
            ])
        self.assertIsInstance(args, argparse.Namespace)
        self.assertEqual(path.join(self.tempdir, 'test.import'), args.config)
        self.assertEqual([path.join(self.tempdir, 'Downloads')], args.downloads)

    def test_parse_arguments__multiple(self):
        parser = scripts_utils.create_legacy_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            args = parser.parse_args(args=[
                path.join(self.tempdir, 'test.import'),
                path.join(self.tempdir, 'Downloads/ofxdownload.ofx'), # File
                path.join(self.tempdir, 'Downloads/Subdir'),          # Directory
            ])
        self.assertIsInstance(args, argparse.Namespace)
        self.assertEqual(path.join(self.tempdir, 'test.import'), args.config)
        self.assertEqual(2, len(args.downloads))


INGEST_MAIN_WITH_DUPS = """\
def find_duplicates(new_entries_list, existing_entries):
    print('{60248d66-f3f2-41c2-96af-039341eafd25}')
    return new_entries_list

scripts_utils.ingest(CONFIG, detect_duplicates_func=find_duplicates)
"""

class TestImplicitInvocationMethods(scripts_utils.TestScriptsBase):

    FILES = {
        'test.import': scripts_utils.IMPORT_FILE,
        'test2.import': scripts_utils.IMPORT_FILE + INGEST_MAIN_WITH_DUPS,
        'Downloads/ofxdownload.ofx': scripts_utils.OFX_FILE,
        }

    def test_implicit_invocation(self):
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(extract.main,
                                     [path.join(self.tempdir, 'test.import'),
                                      path.join(self.tempdir, 'Downloads')])
        self.assertRegex(stdout.getvalue(), r'\*.*ofxdownload.ofx')
        self.assertFalse(stderr.getvalue())

    def test_implicit_invocation_with_ingest_call(self):
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            test_utils.run_with_args(extract.main,
                                     [path.join(self.tempdir, 'test2.import'),
                                      path.join(self.tempdir, 'Downloads')])
        self.assertRegex(stdout.getvalue(), r'\*.*ofxdownload.ofx')
        self.assertFalse(stderr.getvalue())

        # Make sure the duplicates function was called.
        self.assertRegex(stdout.getvalue(),
                         r'{60248d66-f3f2-41c2-96af-039341eafd25}')
