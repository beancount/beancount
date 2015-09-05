__author__ = "Martin Blais <blais@furius.ca>"

import re
import textwrap

from beancount.core.number import D
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.parser import booking
from beancount import loader


class TestFullBooking(cmptest.TestCase):

    @loader.load_doc()
    def test_full_booking(self, entries, _, options_map):
        """
          option "booking_method" "FULL"
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing           5 GOOG {501 USD}
            Equity:Opening-Balances     -2505 USD
        """
        self.assertEqual(D('-2505'), entries[-1].postings[-1].position.number)
