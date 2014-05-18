import unittest
import tempfile

from beancount import loader


TEST_FILE = """

2014-01-01 open Assets:MyBank:Checking   USD
2014-01-01 open Expenses:Restaurant   USD

2014-02-22 * "Something happened."
  Assets:MyBank:Checking       100.00 USD
  Expenses:Restaurant

2015-01-01 close Assets:MyBank:Checking
2015-01-01 close Expenses:Restaurant

"""


class TestLoader(unittest.TestCase):

    def test_load(self):
        with tempfile.NamedTemporaryFile('w') as f:
            f.write(TEST_FILE)
            f.flush()
            entries, errors, options_map = loader.load(f.name)
            self.assertTrue(isinstance(entries, list))
            self.assertTrue(isinstance(errors, list))
            self.assertTrue(isinstance(options_map, dict))

    def test_loaddoc(self):
        def test_function(self_, entries, errors, options_map):
            self.assertTrue(isinstance(entries, list))
            self.assertTrue(isinstance(errors, list))
            self.assertTrue(isinstance(options_map, dict))

        test_function.__doc__ = TEST_FILE
        test_function = loader.loaddoc(test_function)
        test_function(self)

    @loader.loaddoc
    def test_loaddoc_inline(self, entries, errors, options_map):
        """
        """
        self.assertTrue(isinstance(entries, list))
        self.assertTrue(isinstance(errors, list))
        self.assertTrue(isinstance(options_map, dict))
