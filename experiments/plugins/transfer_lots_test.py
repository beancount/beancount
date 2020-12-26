__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import data
from beancount.parser import cmptest
from beancount.parser import printer
from beancount import loader
from experiments.plugins import transfer_lots


class TestFillAccountOpen(cmptest.TestCase):

    @loader.load_doc()
    def test_transfer_lots(self, entries, errors, _):
        """
        plugin "experiments.plugins.transfer_lots" "transfer"

        2020-01-01 open Assets:Bank:Checking
        2020-01-01 open Assets:Coinbase  "FIFO"
        2020-01-01 open Assets:Binance

        2020-12-25 * "Fill up account with crypto"
          Assets:Coinbase         0.2 BTC {15000 USD}
          Assets:Coinbase         0.3 BTC {16000 USD}
          Assets:Coinbase         0.1 BTC {17000 USD}
          Assets:Coinbase         0.4 BTC {18000 USD, "wow"}
          Assets:Bank:Checking

        2020-12-26 * "Transfer lots" #transfer
          Assets:Coinbase        -0.4 BTC {}
          Assets:Binance
        """
        self.assertEqualEntries("""

        2020-12-25 * "Fill up account with crypto"
          Assets:Coinbase            0.2 BTC {15000 USD, 2020-12-25}
          Assets:Coinbase            0.3 BTC {16000 USD, 2020-12-25}
          Assets:Coinbase            0.1 BTC {17000 USD, 2020-12-25}
          Assets:Coinbase            0.4 BTC {18000 USD, 2020-12-25, "wow"}
          Assets:Bank:Checking  -16700.0 USD

        2020-12-26 * "Transfer lots" #transfer
          Assets:Coinbase  -0.2 BTC {15000 USD, 2020-12-25}
          Assets:Coinbase  -0.2 BTC {16000 USD, 2020-12-25}
          Assets:Binance    0.2 BTC {15000 USD, 2020-12-25}
          Assets:Binance    0.2 BTC {16000 USD, 2020-12-25}

        """, list(data.filter_txns(entries)))


if __name__ == '__main__':
    unittest.main()
