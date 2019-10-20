__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.parser import cmptest
from beancount.plugins import fix_payees
from beancount import loader


class TestFixPayees(cmptest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, options_map):
        """
        2014-01-01 open Liabilities:CreditCard
        2014-01-01 open Expenses:Car-Rental

        2014-02-01 * "BIGAP  NEW YO" "Another Luxury Rental"
          Liabilities:CreditCard     -293.23 USD
          Expenses:Car-Rental

        2014-02-01 * "Big Apple Rental Co" "Another Luxury Rental"
          Liabilities:CreditCard     -293.23 USD
          Expenses:Car-Rental
        """
        self.options_map = options_map
        self.in_entries = list(entries)
        self.exp_entries = list(entries)
        del self.in_entries[3]
        del self.exp_entries[2]

    def fix(self, config):
        "Apply the fixing process."
        return fix_payees.fix_payees(self.in_entries, self.options_map, config)

    def test_config_syntax_errors(self):
        entries, errors = self.fix("")
        self.assertEqual([], errors)
        self.assertEqualEntries(entries, self.in_entries)

        entries, errors = self.fix("NOT SOME VALID PYTHON EXPR")
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Syntax error in config")
        self.assertEqualEntries(entries, self.in_entries)

    def test_account_rule(self):
        entries, errors = self.fix("""[
          ('Big Apple Rental Co', 'A:Expenses:Car-Rental'),
        ]""")
        self.assertFalse(errors)
        self.assertEqualEntries(entries, self.exp_entries)

    def test_match_payee(self):
        entries, errors = self.fix("""[
          ('Big Apple Rental Co', 'D:BIGAP '),
        ]""")
        self.assertFalse(errors)
        self.assertEqualEntries(entries, self.exp_entries)

    def test_match_narration(self):
        entries, errors = self.fix("""[
          ('Big Apple Rental Co', 'D:Luxury Rental'),
        ]""")
        self.assertFalse(errors)
        self.assertEqualEntries(entries, self.exp_entries)

    def test_partial_not_matches(self):
        entries, errors = self.fix("""[
          ('Big Apple Rental Co', 'A:Expenses:Car-Rental' 'D:This Not Matches'),
        ]""")
        self.assertFalse(errors)
        self.assertEqualEntries(entries, self.in_entries)


if __name__ == '__main__':
    unittest.main()
