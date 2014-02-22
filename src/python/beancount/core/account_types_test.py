import unittest

from beancount.core import account_types


class TestAccountGlobals(unittest.TestCase):

    def test_reset_globals(self):
        account_types.update_valid_account_names()

    def test_basics(self):
        self.assertEqual(5, len(account_types.DEFAULT_ACCOUNT_TYPES))
        self.assertTrue(account_types.ACCOUNT_TYPES is not None)
        self.assertTrue(account_types.TYPES_ORDER is not None)
