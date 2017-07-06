__copyright__ = "Copyright (C) 2017  Martin Blais"
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

        2014-02-01 *
          Assets:US:Bank:Checking     100 USD @ 1.01 CAD
          Assets:US:Bank:Savings     -101 CAD
        """
