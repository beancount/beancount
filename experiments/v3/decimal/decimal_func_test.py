"""Test for cross-language Decimal serialization.
"""

from decimal import Decimal
from os import path
import unittest

from experiments.v3.decimal import decimal_func as mod


class DecimalFuncTests(unittest.TestCase):

  def test_accept_decimal(self):
      mod.accept_decimal(Decimal("0.01"))


if __name__ == '__main__':
  unittest.main()
