import unittest
import re

from beancount.core.balance import *
from beancount.core.data import Posting, create_simple_posting, create_simple_posting_with_cost
from beancount.core.account import account_from_name
from beancount.core.amount import Amount


class TestBalance(unittest.TestCase):

    def test_get_balance_amount(self):
        "Test the balance requirements."

        # Entry without cost, without price.
        posting = create_simple_posting(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertEqual(Amount("105.50", "USD"), get_balance_amount(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=Amount("0.90", "CAD"))
        self.assertEqual(Amount("94.95", "CAD"), get_balance_amount(posting))

        # Entry with cost, without price.
        posting = create_simple_posting_with_cost(None, "Assets:Bank:Checking",
                                                  "105.50", "USD", "0.80", "EUR")
        self.assertEqual(Amount("84.40", "EUR"), get_balance_amount(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=Amount("2.00", "CAD"))
        self.assertEqual(Amount("84.40", "EUR"), get_balance_amount(posting))

    def test_compute_residual(self):

        # Try with two accounts.
        balance = compute_residual([
            create_simple_posting(None, "Assets:Bank:Checking", "105.50", "USD"),
            create_simple_posting(None, "Assets:Bank:Checking", "-194.50", "USD"),
            ])
        self.assertEqual([Amount("-89", "USD")], balance.get_amounts())

        # Try with more accounts.
        balance = compute_residual([
            create_simple_posting(None, "Assets:Bank:Checking", "105.50", "USD"),
            create_simple_posting(None, "Assets:Bank:Checking", "-194.50", "USD"),
            create_simple_posting(None, "Assets:Bank:Investing", "5", "AAPL"),
            create_simple_posting(None, "Assets:Bank:Savings", "89.00", "USD"),
            ])
        self.assertEqual([Amount("5", "AAPL")], balance.get_amounts())


## FIXME: TODO - Balance incomplete postings.


unittest.main()
