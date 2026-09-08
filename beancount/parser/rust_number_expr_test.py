import textwrap
import unittest
from decimal import Decimal

from beancount.parser import parser as parser_mod


class TestRustNumberExpr(unittest.TestCase):
    def parse(self, src: str):
        return parser_mod.parse_string(textwrap.dedent(src))

    def test_amount_allows_unary_minus_parentheses(self):
        entries, errors, _ = self.parse(
            """
            2014-01-01 * \"Expr\"
              Assets:Cash  -(1-2) CNY
              Equity:Opening-Balances
            """
        )

        self.assertFalse(errors)
        self.assertEqual(1, len(entries))

        txn = entries[0]
        self.assertEqual(2, len(txn.postings))

        units = txn.postings[0].units
        self.assertEqual("CNY", units.currency)
        self.assertEqual(Decimal("1"), units.number)


if __name__ == "__main__":
    unittest.main()
