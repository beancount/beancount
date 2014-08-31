import datetime
import re

from beancount.core import data
from beancount.core import compare
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.parser import printer
from beancount.ops import validation
from beancount.ops import implicit_prices


class TestValidateInventoryBooking(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_inventory_booking(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Investments:Cash
        2014-01-01 open Assets:Investments:Stock

        2014-06-22 * "Add some positive units"
          Assets:Investments:Stock   1 GOOG {500 USD}
          Assets:Investments:Cash

        2014-06-23 * "Down to zero"
          Assets:Investments:Stock  -1 GOOG {500 USD}
          Assets:Investments:Cash

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock  -1 GOOG {500 USD}
          Assets:Investments:Cash

        2014-06-25 * "Go positive much"
          Assets:Investments:Stock  11 GOOG {500 USD}
          Assets:Investments:Cash

        2014-06-26 * "Cross to negative from above zero"
          Assets:Investments:Stock  -15 GOOG {500 USD}
          Assets:Investments:Cash

        """
        validation_errors = validation.validate_inventory_booking(entries, options_map)

        self.assertEqual([validation.ValidationError, validation.ValidationError],
                         list(map(type, validation_errors)))
        self.assertEqualEntries("""

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock  -1 GOOG {500 USD}
          Assets:Investments:Cash

        2014-06-26 * "Cross to negative from above zero"
          Assets:Investments:Stock  -15 GOOG {500 USD}
          Assets:Investments:Cash

        """, [e.entry for e in validation_errors])

    @parser.parsedoc
    def test_negative_lots(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                -1 GOOG {501 USD}
            Equity:Opening-Balances
        """
        validation_errors = validation.validate_inventory_booking(entries, options_map)
        self.assertEqual([validation.ValidationError], list(map(type, validation_errors)))


class TestValidateOpenClose(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_open_close__duplicate_open(self, entries, _, options_map):
        """
        ;; Regular, only appears once.
        2014-02-10 open  Assets:US:Bank:Checking1

        ;; Open twice on the same date.
        2014-02-11 open  Assets:US:Bank:Checking2
        2014-02-11 open  Assets:US:Bank:Checking2

        ;; Open twice on different dates.
        2014-02-20 open  Assets:US:Bank:Checking3
        2014-02-21 open  Assets:US:Bank:Checking3
        """
        errors = validation.validate_open_close(entries, options_map)
        self.assertEqual(['Assets:US:Bank:Checking2',
                          'Assets:US:Bank:Checking3'],
                         [error.entry.account for error in errors])

    @parser.parsedoc
    def test_validate_open_close__duplicate_close(self, entries, _, options_map):
        """
        2014-02-10 open  Assets:US:Bank:Checking1
        2014-02-10 open  Assets:US:Bank:Checking2
        2014-02-10 open  Assets:US:Bank:Checking3

        ;; Regular, only appears once.
        2014-03-01 close Assets:US:Bank:Checking1

        ;; Close twice on the same date.
        2014-03-11 close Assets:US:Bank:Checking2
        2014-03-11 close Assets:US:Bank:Checking2

        ;; Close twice on different dates.
        2014-03-21 close Assets:US:Bank:Checking3
        2014-03-22 close Assets:US:Bank:Checking3

        """
        errors = validation.validate_open_close(entries, options_map)
        self.assertEqual(['Assets:US:Bank:Checking2',
                          'Assets:US:Bank:Checking3'],
                         [error.entry.account for error in errors])

    @parser.parsedoc
    def test_validate_open_close__close_unopened(self, entries, _, options_map):
        """
        2014-03-01 close Assets:US:Bank:Checking1
        """
        errors = validation.validate_open_close(entries, options_map)
        self.assertEqual(['Assets:US:Bank:Checking1'],
                         [error.entry.account for error in errors])

    @parser.parsedoc
    def test_validate_open_close__ordering(self, entries, _, options_map):
        """
        2014-03-01 open  Assets:US:Bank:Checking1
        2014-02-01 close Assets:US:Bank:Checking1
        """
        errors = validation.validate_open_close(entries, options_map)
        self.assertEqual(['Assets:US:Bank:Checking1'],
                         [error.entry.account for error in errors])


class TestValidateDuplicateBalances(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_duplicate_balances(self, entries, _, options_map):
        """
        2014-01-01 balance Assets:US:Bank:Checking1
        2014-01-01 balance Assets:US:Bank:Checking2

        ;; Duplicates, with different amounts (error).
        2014-03-01 balance Assets:US:Bank:Checking1  100 USD
        2014-03-01 balance Assets:US:Bank:Checking1  101 USD

        ;; Duplicates, with same amount (okay).
        2014-03-02 balance Assets:US:Bank:Checking1  100 USD
        2014-03-02 balance Assets:US:Bank:Checking1  100 USD

        ;; Different dates (okay).
        2014-03-03 balance Assets:US:Bank:Checking1  100 USD
        2014-03-04 balance Assets:US:Bank:Checking1  101 USD

        ;; Different currencies (okay).
        2014-03-03 balance Assets:US:Bank:Checking1  100 USD
        2014-03-04 balance Assets:US:Bank:Checking1  100 CAD

        ;; Different accounts at same date (okay).
        2014-03-05 balance Assets:US:Bank:Checking1  100 USD
        2014-03-05 balance Assets:US:Bank:Checking2  100 USD
        """
        errors = validation.validate_duplicate_balances(entries, options_map)
        self.assertEqual([datetime.date(2014, 3, 1)],
                         [error.entry.date for error in errors])


class TestValidateActiveAccounts(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_active_accounts(self, entries, _, options_map):
        """
        2014-01-01 open  Equity:Opening-Balances

        2014-02-01 * "Invalid before"
          Assets:Temporary    1 USD
          Equity:Opening-Balances

        2014-02-02 note  Assets:Temporary "Invalid note entry"
        2014-02-03 pad   Assets:Temporary Equity:Opening-Balances

        2014-03-01 open  Assets:Temporary

        2014-04-01 * "Valid"
          Assets:Temporary    1 USD
          Equity:Opening-Balances

        2014-05-01 * "Unknown account"
          Assets:Temporary    1 USD
          Equity:ImUnknown

        2014-09-01 close Assets:Temporary

        2014-10-01 * "Invalid after"
          Assets:Temporary    1 USD
          Equity:Opening-Balances

        ;; These should be allowed after close.
        2014-10-02 note  Assets:Temporary "Invalid note entry again"
        2014-10-03 document  Assets:Temporary "/path/to/allowed-doc.pdf"

        """
        errors = validation.validate_active_accounts(entries, options_map)

        self.assertEqualEntries("""

        2014-02-01 * "Invalid before"
          Assets:Temporary    1 USD
          Equity:Opening-Balances

        2014-02-02 note  Assets:Temporary "Invalid note entry"
        2014-02-03 pad   Assets:Temporary Equity:Opening-Balances

        2014-05-01 * "Unknown account"
          Assets:Temporary    1 USD
          Equity:ImUnknown

        2014-10-01 * "Invalid after"
          Assets:Temporary    1 USD
          Equity:Opening-Balances

        """, [error.entry for error in errors])

        self.assertTrue(all(
            (re.search('inactive.*Assets:Temporary', error.message) or
             re.search('unknown.*Equity:ImUnknown', error.message))
            for error in errors))


class TestValidateUnusedAccounts(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_unused_accounts(self, entries, _, options_map):
        """
        2014-01-01 open  Assets:Account1 ; Used, kept open
        2014-01-01 open  Assets:Account2 ; Used and closed
        2014-01-01 open  Assets:Account3 ; Unused
        2014-01-01 open  Equity:Opening-Balances

        2014-02-01 *
          Assets:Account1            1 USD
          Assets:Account2            1 USD
          Equity:Opening-Balances

        2014-06-01 close Assets:Account2
        """
        errors = validation.validate_unused_accounts(entries, options_map)
        self.assertEqual(1, len(errors))
        self.assertEqual('Assets:Account3', errors[0].entry.account)


class TestValidateCurrencyConstraints(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_currency_constraints(self, entries, _, options_map):
        """
        2014-01-01 open  Assets:Account1    USD
        2014-01-01 open  Assets:Account2    GOOG
        2014-01-01 open  Assets:Account3    USD,GOOG

        2014-01-02 * "Entries without cost"
          Assets:Account1            1 USD
          Equity:Opening-Balances

        2014-01-03 * "Entries without cost" #expected
          Assets:Account1            1 CAD
          Equity:Opening-Balances

        2014-01-04 * "Entries with cost"
          Assets:Account2            1 GOOG {500 USD}
          Equity:Opening-Balances

        2014-01-05 * "Entries with cost" #expected
          Assets:Account2            1 AAPL {500 USD}
          Equity:Opening-Balances

        2014-01-02 * "Multiple currencies"
          Assets:Account3            1 USD
          Assets:Account3            1 GOOG {500 USD}
          Equity:Opening-Balances

        2014-01-05 * "Multiple currencies" #expected
          Assets:Account3            1 CAD
          Equity:Opening-Balances

        2014-01-05 * "Multiple currencies" #expected
          Assets:Account3            1 AAPL {500 USD}
          Equity:Opening-Balances

        """
        errors = validation.validate_currency_constraints(entries, options_map)

        self.assertEqualEntries([entry for entry in entries
                                 if (isinstance(entry, data.Transaction) and
                                     entry.tags and
                                     'expected' in entry.tags)],
                                [error.entry for error in errors])


class TestValidateDocumentPaths(cmptest.TestCase):

    def test_validate_documents_paths(self):
        date = datetime.date(2014, 3, 3)
        source = data.Source('<validation_test>', 0)
        entries = [data.Document(source, date, 'Assets:Account1',
                                 "/abs/path/to/something.pdf"),
                   data.Document(source, date, 'Assets:Account2',
                                 "relative/something.pdf"),
                   data.Document(source, date, 'Assets:Account2',
                                 "../something.pdf"),
                   data.Document(source, date, 'Assets:Account2',
                                 "")]
        errors = validation.validate_documents_paths(entries, {})
        self.assertEqual(3, len(errors))
        self.assertEqual({'Assets:Account2'}, set(error.entry.account for error in errors))


class TestValidateDataTypes(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_data_types(self, entries, errors, options_map):
        """
        2014-06-24 * "Narration"
          Assets:Investments:Stock  1 GOOG {500 USD}
          Assets:Investments:Cash
        """
        # Just a basic test that runs the sanitation code (that should already
        # be well tested by itself).
        entry = entries[0]
        new_entry = entry._replace(narration=None)
        valid_errors = validation.validate_data_types([new_entry], options_map)
        self.assertEqual([validation.ValidationError], list(map(type, valid_errors)))


class TestValidateCheckTransactionBalances(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_check_transaction_balances(self, entries, errors, options_map):
        """
        2014-06-24 * "Narration"
          Assets:Investments:Stock  1 USD
          Assets:Investments:Cash   1 USD
        """
        valid_errors = validation.validate_check_transaction_balances(entries, options_map)
        self.assertEqual([validation.ValidationError], list(map(type, valid_errors)))


class TestValidateDuplicates(cmptest.TestCase):

    def checkDuplicates(self, entries, errors, options_map):
        self.assertEqual([], errors)
        valid_errors = validation.validate_duplicates(entries, options_map)
        self.assertEqual([compare.CompareError], list(map(type, valid_errors)))
        self.assertTrue(re.search('Duplicate entry', valid_errors[0].message))

    @parser.parsedoc
    def test_validate_duplicates__open(self, entries, errors, options_map):
        """
        2000-01-01 open Assets:Checking
        2000-01-01 open Assets:Checking
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__close(self, entries, errors, options_map):
        """
        2000-01-01 close Assets:Checking
        2000-01-01 close Assets:Checking
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__pad(self, entries, errors, options_map):
        """
        2000-01-01 pad Assets:Checking Equity:Opening-Balances
        2000-01-01 pad Assets:Checking Equity:Opening-Balances
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__balance(self, entries, errors, options_map):
        """
        2000-01-01 balance Assets:Checking 201.00 USD
        2000-01-01 balance Assets:Checking 201.00 USD
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__balance(self, entries, errors, options_map):
        """
        2000-01-01 balance Assets:Checking 201.00 USD
        2000-01-01 balance Assets:Checking 201.00 USD
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__transaction(self, entries, errors, options_map):
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
    def test_validate_duplicates__note(self, entries, errors, options_map):
        """
        2000-01-01 note Assets:Checking "Something about something"
        2000-01-01 note Assets:Checking "Something about something"
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__event(self, entries, errors, options_map):
        """
        2000-01-01 event "location" "Somewhere, Earth"
        2000-01-01 event "location" "Somewhere, Earth"
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__document(self, entries, errors, options_map):
        """
        2000-01-01 document Assets:Checking "/path/to/nowhere.pdf"
        2000-01-01 document Assets:Checking "/path/to/nowhere.pdf"
        """
        self.checkDuplicates(entries, errors, options_map)

    @parser.parsedoc
    def test_validate_duplicates__price(self, entries, errors, options_map):
        """
        2000-01-01 price GOOG 500 USD
        2000-01-01 price GOOG 500 USD
        """
        self.assertEqual([], errors)
        valid_errors = validation.validate_duplicates(entries, options_map)
        # Note! This is different. We allow exact duplicates of price directive
        # on purpose. They may be common, and they won't hurt anything.
        self.assertEqual([], list(map(type, valid_errors)))


class TestValidate(cmptest.TestCase):

    @parser.parsedoc
    def test_validate(self, entries, _, options_map):
        """
        ;; Just trigger a few errors from here to ensure at least some of the plugins
        ;; tested above are run.

        2014-01-01 open Assets:Investments:Cash
        2014-01-01 open Assets:Investments:Stock   AAPL

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock  -1 GOOG {500 USD}
          Assets:Investments:Cash
        """
        errors = validation.validate(entries, options_map)
        self.assertEqual(2, len(errors))
        self.assertTrue(any(re.match('Position.*negative', error.message)
                            for error in errors))
        self.assertTrue(any(re.match('Invalid currency', error.message)
                            for error in errors))
