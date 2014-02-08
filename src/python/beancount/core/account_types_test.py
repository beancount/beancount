import unittest

from beancount.core import account_types

class TestAccountGlobals(unittest.TestCase):

    def test_reset_globals(self):
        account_types.update_valid_account_names()
