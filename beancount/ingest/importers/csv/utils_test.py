"""
csv.py tests.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

from beancount.ingest.importers import csv
from beancount.parser import cmptest
from beancount.core import amount


class TestCSVFunctions(cmptest.TestCase):

    def test_func_amountnum_dbcr(self):
        func = csv.func_amountnum_dbcr()
        self.assertEqual(func(("1", "")), amount.Decimal(-1))
        self.assertEqual(func(("1", "0")), amount.Decimal(-1))
        self.assertEqual(func(("", "1")), amount.Decimal(1))
        self.assertEqual(func(("0", "1")), amount.Decimal(1))
        with self.assertRaises(Exception):
            _ = func(("", ""))
        with self.assertRaises(Exception):
            _ = func(("0", "0"))

        func = csv.func_amountnum_dbcr(debit_negative=False)
        self.assertEqual(func(("1", "")), amount.Decimal(1))

        func = csv.func_amountnum_dbcr(allow_zero_amounts=True)
        self.assertEqual(func(("", "")), amount.Decimal(0))
        self.assertEqual(func(("0", "0")), amount.Decimal(0))

        # This assertion fails
        # self.assertEqual(func(("-0", "1")), amount.Decimal(1))

    def test_func_amount_dbcr(self):
        func = csv.func_amount_dbcr()
        self.assertEqual(func(("1", "", "BEAN")), amount.Amount(amount.Decimal(-1), "BEAN"))
        self.assertEqual(func(("0", "1", "COUNT")), amount.Amount(amount.Decimal(1), "COUNT"))
