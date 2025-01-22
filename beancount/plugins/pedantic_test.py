__copyright__ = "Copyright (C) 2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader


class TestPedantic(unittest.TestCase):
    @loader.load_doc()
    def test_plugins_pedantic(self, entries, _, options_map):
        """
        plugin "beancount.plugins.pedantic"

        2010-01-01 commodity USD
        2010-01-01 commodity CAD

        2012-01-01 open Assets:US:Bank:Checking  USD
        2012-01-01 open Assets:US:Bank:Savings   CAD

        2014-02-01 *
          Assets:US:Bank:Checking     100 USD @ 1.01 CAD
          Assets:US:Bank:Savings     -101 CAD
        """


if __name__ == "__main__":
    unittest.main()
