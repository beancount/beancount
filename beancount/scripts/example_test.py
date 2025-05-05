__copyright__ = "Copyright (C) 2014, 2016-2017, 2019, 2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.ops import validation
from beancount.scripts import example
from beancount.utils import test_utils


class TestScriptExample(test_utils.ClickTestCase):
    def test_generate(self):
        # For some reason rv.stdout includes stderr output when run from Bazel,
        # so you have to disable that from the program.
        rv = self.run_with_args(example.main)
        self.assertTrue(rv.stdout)

        loaded_entries, errors, _ = loader.load_string(
            rv.stdout, extra_validations=validation.HARDCORE_VALIDATIONS
        )
        self.assertFalse(errors)

    def test_generate_with_date(self):
        # For some reason rv.stdout includes stderr output when run from Bazel,
        # so you have to disable that from the program.
        rv = self.run_with_args(example.main, "--date-birth", "1988-07-01")
        self.assertTrue(rv.stdout)

        loaded_entries, errors, _ = loader.load_string(
            rv.stdout, extra_validations=validation.HARDCORE_VALIDATIONS
        )
        self.assertFalse(errors)


if __name__ == "__main__":
    unittest.main()
