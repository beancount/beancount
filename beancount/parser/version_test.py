__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import unittest

import beancount
from beancount.parser import version
from beancount.utils import test_utils


class TestVersion(unittest.TestCase):

    def test_version_exists(self):
        self.assertRegex(getattr(beancount, "__version__", ""), r"\d+\.\d+\.\d+")

    def test_compute_version_string(self):
        self.assertEqual(
            'Beancount 2.1.0 (hg:e291c91b37e1; 2018-05-12)',
            version.compute_version_string(
                '2.1.0', 'hg:e291c91b37e1d21c29645d93473a7917bb726699', 1526099371))

        self.assertEqual(
            'Beancount 2.1.0 (git:e291c91b; 2018-05-12)',
            version.compute_version_string(
                '2.1.0', 'git:e291c91b37e1d21c29645d93473', 1526099371))

        self.assertEqual(
            'Beancount 2.1.0',
            version.compute_version_string(
                '2.1.0', '', 0))

    def test_version(self):
        parser = version.ArgumentParser()
        self.assertIsInstance(parser, argparse.ArgumentParser)

        with test_utils.capture('stdout') as stdout, self.assertRaises(SystemExit):
            parser.parse_args(['--version'])
        self.assertRegex(stdout.getvalue(), r'Beancount \d+\.\d+\.\d+')


if __name__ == '__main__':
    unittest.main()
