__author__ = 'Martin Blais <blais@furius.ca>'

import unittest
from beancount.core import amount
from beancount.query import numberify
A = amount.from_string


class TestNumerify(unittest.TestCase):

    def test_one_amount(self):
        rtypes = [('pos', amount.Amount)]
        rrows = [A("24.17 CAD"),
                    A("-77.02 CAD"),
                    A("11.39 CAD"),
                    A("800.00 USD"),
                    A("41.17 CAD"),
                    A("950.00 USD"),
                    A("-947.00 USD")]
        etype, erows = numberify.numberify_results(rtypes, rrows)
        [('pos (CAD)', Decimal), ('pos (USD)', Decimal)]
