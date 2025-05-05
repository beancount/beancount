__copyright__ = "Copyright (C) 2018-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.utils import defdict
from beancount.utils import import_utils


class TestImportSymbol(unittest.TestCase):
    def test_import_symbol(self):
        with self.assertRaises(ImportError):
            import_utils.import_symbol("beancount.nothing.data")
        with self.assertRaises((AttributeError, ModuleNotFoundError)):
            import_utils.import_symbol("beancount.utils.i_dont_exist")
        cls = import_utils.import_symbol("beancount.utils.defdict.ImmutableDictWithDefault")
        self.assertIs(defdict.ImmutableDictWithDefault, cls)


if __name__ == "__main__":
    unittest.main()
