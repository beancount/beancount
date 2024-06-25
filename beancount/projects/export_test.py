__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import unittest
import click.testing

from beancount.utils import test_utils
from beancount.projects import export


class TestExport(test_utils.TestCase):
    def test_export_basic(self):
        rootdir = test_utils.find_repository_root(__file__)
        example_beancount = path.join(rootdir, "examples", "example.beancount")

        runner = click.testing.CliRunner()
        result = runner.invoke(export.main, [example_beancount, "-o", "-"])
        self.assertEqual(0, result.exit_code)


if __name__ == "__main__":
    unittest.main()
