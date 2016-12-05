__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core import compare
from beancount.parser import cmptest
from beancount.plugins import noduplicates
from beancount import loader


class TestValidateDuplicates(cmptest.TestCase):

    def checkDuplicates(self, entries, options_map):
        _, valid_errors = noduplicates.validate_no_duplicates(entries, options_map)
        self.assertEqual([compare.CompareError], list(map(type, valid_errors)))
        self.assertRegex(valid_errors[0].message, 'Duplicate entry')

    # Note: validation already checks for dups in open/close.
    @loader.load_doc(expect_errors=True)
    def test_validate_no_duplicates__open(self, entries, _, options_map):
        """
        2000-01-01 open Assets:Checking
        2000-01-01 open Assets:Checking
        """
        self.checkDuplicates(entries, options_map)

    # Note: validation already checks for dups in open/close.
    @loader.load_doc(expect_errors=True)
    def test_validate_no_duplicates__close(self, entries, _, options_map):
        """
        2000-01-01 close Assets:Checking
        2000-01-01 close Assets:Checking
        """
        self.checkDuplicates(entries, options_map)

    # Note: The regular padding code will trigger an unused Pad entry error.
    @loader.load_doc(expect_errors=True)
    def test_validate_no_duplicates__pad(self, entries, _, options_map):
        """
        2000-01-01 open Assets:Checking
        2000-01-01 open Equity:Opening-Balances
        2000-01-15 pad Assets:Checking Equity:Opening-Balances
        2000-01-15 pad Assets:Checking Equity:Opening-Balances
        2001-01-01 balance Assets:Checking 1000.00 USD
        """
        self.checkDuplicates(entries, options_map)

    @loader.load_doc()
    def test_validate_no_duplicates__balance(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Checking
        2014-01-01 open Equity:Opening-Balances

        2014-06-24 * "Go negative from zero"
          Assets:Checking  201.00 USD
          Equity:Opening-Balances

        2015-01-01 balance Assets:Checking 201.00 USD
        2015-01-01 balance Assets:Checking 201.00 USD
        """
        self.checkDuplicates(entries, options_map)

    @loader.load_doc()
    def test_validate_no_duplicates__transaction(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Investments:Stock
        2014-01-01 open Assets:Investments:Cash

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock    1 HOOL {500 USD}
          Assets:Investments:Cash  -500 USD

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock    1 HOOL {500 USD}
          Assets:Investments:Cash  -500 USD
        """
        self.checkDuplicates(entries, options_map)

    @loader.load_doc()
    def test_validate_no_duplicates__note(self, entries, _, options_map):
        """
        2000-01-01 open Assets:Checking
        2001-01-01 note Assets:Checking "Something about something"
        2001-01-01 note Assets:Checking "Something about something"
        """
        self.checkDuplicates(entries, options_map)

    @loader.load_doc()
    def test_validate_no_duplicates__event(self, entries, _, options_map):
        """
        2000-01-01 event "location" "Somewhere, Earth"
        2000-01-01 event "location" "Somewhere, Earth"
        """
        self.checkDuplicates(entries, options_map)

    @loader.load_doc(expect_errors=True)
    def test_validate_no_duplicates__document(self, entries, _, options_map):
        """
        2000-01-01 document Assets:Checking "/path/to/nowhere.pdf"
        2000-01-01 document Assets:Checking "/path/to/nowhere.pdf"
        """
        self.checkDuplicates(entries, options_map)

    @loader.load_doc()
    def test_validate_no_duplicates__price(self, entries, errors, options_map):
        """
        2000-01-01 price HOOL 500 USD
        2000-01-01 price HOOL 500 USD
        """
        self.assertEqual([], errors)
        _, valid_errors = noduplicates.validate_no_duplicates(entries, options_map)
        # Note! This is different. We allow exact duplicates of price directive
        # on purpose. They may be common, and they won't hurt anything.
        self.assertEqual([], list(map(type, valid_errors)))
