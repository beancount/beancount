__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import argparse
from os import path

from beancount.utils import test_utils
from beancount.ingest import scripts_utils


def run(args, parser, importers_list, files_or_directories):
    pass


class TestTestScriptsBase(scripts_utils.TestScriptsBase):

    def test_test_scripts_base(self):
        self.assertTrue(path.exists(self.tempdir))
        self.assertTrue(any(filename.endswith('.import')
                            for filename in os.listdir(self.tempdir)))

    def test_parse_arguments__insufficient(self):
        # Test with insufficient arguments.
        parser = scripts_utils.create_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                parser.parse_args(args=[])

        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                parser.parse_args(args=[path.join(self.tempdir, 'test.import')])

    def test_parse_arguments__invalid(self):
        parser = scripts_utils.create_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            with self.assertRaises(SystemExit):
                scripts_utils.run_import_script_and_ingest(parser, argv=[
                    path.join(self.tempdir, 'test.import'),
                    path.join(self.tempdir, 'Non-existent'),
                ])

    def test_parse_arguments__sufficient(self):
        parser = scripts_utils.create_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            args = parser.parse_args(args=[
                path.join(self.tempdir, 'test.import'),
                path.join(self.tempdir, 'Downloads'),
            ])
        self.assertIsInstance(args, argparse.Namespace)
        self.assertEqual(path.join(self.tempdir, 'test.import'), args.config)
        self.assertEqual([path.join(self.tempdir, 'Downloads')], args.downloads)

    def test_parse_arguments__multiple(self):
        parser = scripts_utils.create_arguments_parser("Test script", run)
        with test_utils.capture('stdout', 'stderr') as (stdout, stderr):
            args = parser.parse_args(args=[
                path.join(self.tempdir, 'test.import'),
                path.join(self.tempdir, 'Downloads/ofxdownload.ofx'), # File
                path.join(self.tempdir, 'Downloads/Subdir'),          # Directory
            ])
        self.assertIsInstance(args, argparse.Namespace)
        self.assertEqual(path.join(self.tempdir, 'test.import'), args.config)
        self.assertEqual(2, len(args.downloads))
