__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.utils import import_utils
from beancount.utils import defdict


class TestImportSymbol(unittest.TestCase):

    def test_import_symbol(self):
        with self.assertRaises(ImportError):
            import_utils.import_symbol('beancount.nothing.data')
        with self.assertRaises((AttributeError, ModuleNotFoundError)):
            import_utils.import_symbol('beancount.utils.i_dont_exist')
        cls = import_utils.import_symbol('beancount.utils.defdict.DefaultDictWithKey')
        self.assertIs(defdict.DefaultDictWithKey, cls)


if __name__ == '__main__':
    unittest.main()
