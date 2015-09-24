__author__ = "Martin Blais <blais@furius.ca>"

from beancount import loader
from beancount.parser import cmptest
from beancount.parser import printer


class TestRebookFIFO(cmptest.TestCase):

    @loader.load_doc()
    def test_exclude_tag(self, entries, errors, __):
        """
            plugin "beancount.plugins.fifo"

            2015-01-01 open Assets:Bitcoin   "NONE"
            2015-01-01 open Assets:Bank
            2015-01-01 open Expenses:Something

            2015-09-04 *
              Assets:Bank          -1000.00 USD
              Assets:Bitcoin       4.333507 BTC {230.76 USD}

            2015-09-05 *
              Assets:Bank          -1000.00 USD
              Assets:Bitcoin       4.345747 BTC {230.11 USD}

            2015-09-20 *
              Assets:Bitcoin       -1.000000 BTC
              Expenses:Something
        """
        print('-' * 120)
        printer.print_entries(entries)
