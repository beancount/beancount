import re

from beancount.core import compare
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.plugins import noduplicates


class TestValidateDuplicates(cmptest.TestCase):

    def checkDuplicates(self, entries, errors, options_map):
        self.assertEqual([], errors)
        _, valid_errors = noduplicates.validate_no_duplicates(entries, options_map)
        self.assertEqual([compare.CompareError], list(map(type, valid_errors)))
        self.assertTrue(re.search('Duplicate entry', valid_errors[0].message))

    @parser.parsedoc
    def test_validate_no_duplicates__open(self, entries, errors, options_map):
        """
        2000-01-01 open Assets:Checking
        2000-01-01 open Assets:Checking
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__close(self, entries, errors, options_map):
        """
        2000-01-01 close Assets:Checking
        2000-01-01 close Assets:Checking
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__pad(self, entries, errors, options_map):
        """
        2000-01-01 pad Assets:Checking Equity:Opening-Balances
        2000-01-01 pad Assets:Checking Equity:Opening-Balances
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__balance(self, entries, errors, options_map):
        """
        2000-01-01 balance Assets:Checking 201.00 USD
        2000-01-01 balance Assets:Checking 201.00 USD
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__balance(self, entries, errors, options_map):
        """
        2000-01-01 balance Assets:Checking 201.00 USD
        2000-01-01 balance Assets:Checking 201.00 USD
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__transaction(self, entries, errors, options_map):
        """
        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock   1 GOOG {500 USD}
          Assets:Investments:Cash

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock  1 GOOG {500 USD}
          Assets:Investments:Cash
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__note(self, entries, errors, options_map):
        """
        2000-01-01 note Assets:Checking "Something about something"
        2000-01-01 note Assets:Checking "Something about something"
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__event(self, entries, errors, options_map):
        """
        2000-01-01 event "location" "Somewhere, Earth"
        2000-01-01 event "location" "Somewhere, Earth"
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__document(self, entries, errors, options_map):
        """
        2000-01-01 document Assets:Checking "/path/to/nowhere.pdf"
        2000-01-01 document Assets:Checking "/path/to/nowhere.pdf"
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_no_duplicates__price(self, entries, errors, options_map):
        """
        2000-01-01 price GOOG 500 USD
        2000-01-01 price GOOG 500 USD
        """
        self.assertEqual([], errors)
        _, valid_errors = noduplicates.validate_no_duplicates(entries, options_map)
        # Note! This is different. We allow exact duplicates of price directive
        # on purpose. They may be common, and they won't hurt anything.
        self.assertEqual([], list(map(type, valid_errors)))
