__author__ = "Martin Blais <blais@furius.ca>"

from beancount.core.number import D
from beancount.core import data
from beancount.parser import cmptest
from beancount import loader


class TestSimpleBooking(cmptest.TestCase):

    @loader.load_doc()
    def test_simple_booking(self, entries, _, options_map):
        """
          option "booking_method" "SIMPLE"

          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {502 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {500 # 10 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {# 2510 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 2510 USD
            Equity:Opening-Balances
        """
        for entry in entries:
            if not isinstance(entry, data.Transaction):
                continue
            self.assertEqual(D('-2510'), entry.postings[-1].units.number)
