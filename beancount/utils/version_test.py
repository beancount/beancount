__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import unittest

from beancount.utils import version
from beancount.utils import test_utils


class TestVersion(unittest.TestCase):

    def test_version(self):
        parser = version.ArgumentParser()
        self.assertIsInstance(parser, argparse.ArgumentParser)

        with test_utils.capture('stdout') as stdout, self.assertRaises(SystemExit):
            parser.parse_args(['--version'])
        self.assertRegex(stdout.getvalue(), 'Beancount.*changeset')
