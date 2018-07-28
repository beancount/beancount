__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.utils import import_utils
from beancount.core import account

class TestImportSymbol(unittest.TestCase):

    def test_import_symbol(self):
        with self.assertRaises(ImportError):
            import_utils.import_symbol('beancount.nothing.data')
        with self.assertRaises(AttributeError):
            import_utils.import_symbol('beancount.core.data.i_dont_exist')
        func = import_utils.import_symbol('beancount.core.account.join')
        self.assertIs(account.join, func)
