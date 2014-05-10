import unittest
import pprint
import datetime

from beancount.core.amount import to_decimal
from beancount.ops import positions
from beancount.parser import parsedoc


class TestPositionEntries(unittest.TestCase):

    @parsedoc
    def test_get_latest_positions(self, entries, _, __):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1             15 GOOG {518.73 USD}
          Assets:Other

        2013-04-02 *
          Assets:Account1             10 GOOG {523.46 USD}
          Assets:Other

        2013-04-03 *
          Assets:Account1             -4 GOOG {518.73 USD}
          Assets:Other

        2013-04-02 *
          Assets:Account2            20 ITOT {85.l95 USD}
          Assets:Other

        2013-04-03 *
          Assets:Account1             500 GOOG {540 USD} @ 560 USD
          Assets:Other
        """
        for entry in entries:
            print(entry)

        pos = positions.get_latest_positions(entries)
        print(pos)
