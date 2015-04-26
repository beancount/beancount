__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import re
import textwrap

from beancount.core import data
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import cmptest
from beancount.ops import validation


class TestValidateInventoryBooking(cmptest.TestCase):

    def setUp(self):
        self.input_str = textwrap.dedent("""

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

        """)

    def do_validate_inventory_booking(self, input_str):
        entries, errors, options_map = parser.parse_string(input_str)
        validation_errors = validation.validate_inventory_booking(entries, options_map)
        self.assertEqual([], list(map(type, validation_errors)))

    def test_validate_inventory_booking(self):
        self.do_validate_inventory_booking(self.input_str)

    def test_validate_inventory_booking__same_day(self):
        input_str = re.sub(r'\b2\d\b', '22', self.input_str)
        self.do_validate_inventory_booking(input_str)

    @parser.parsedoc
    def test_simple_negative_lots(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                -1 GOOG {501 USD}
            Equity:Opening-Balances
        """
        validation_errors = validation.validate_inventory_booking(entries, options_map)
        self.assertEqual([], list(map(type, validation_errors)))

    @parser.parsedoc
    def test_mixed_lots_in_single_transaction(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 GOOG {501 USD}
            Assets:Bank:Investing                -1 GOOG {502 USD}
            Equity:Opening-Balances
        """
        validation_errors = validation.validate_inventory_booking(entries, options_map)
        self.assertEqual([validation.ValidationError], list(map(type, validation_errors)))

    @parser.parsedoc
    def test_mixed_lots_in_multiple_transactions_augmenting(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 GOOG {501 USD}
            Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:Investing                -1 GOOG {502 USD}
            Equity:Opening-Balances
        """
        validation_errors = validation.validate_inventory_booking(entries, options_map)
        self.assertEqual([validation.ValidationError], list(map(type, validation_errors)))

    @parser.parsedoc
    def test_mixed_lots_in_multiple_transactions_reducing(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 GOOG {501 USD}
            Assets:Bank:Investing                 5 GOOG {502 USD}
            Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:Investing                -6 GOOG {502 USD}
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


class TestValidateDuplicateCommodities(cmptest.TestCase):

    @parser.parsedoc
    def test_validate_duplicate_commodities(self, entries, _, options_map):
        """
        2014-01-01 commodity USD
        2014-01-02 commodity CAD
        2014-01-03 commodity AAPL
        2014-01-04 commodity HOOL
        2014-01-05 commodity USD
        2014-01-06 commodity HOOL

        """
        errors = validation.validate_duplicate_commodities(entries, options_map)
        self.assertEqual([datetime.date(2014, 1, 5), datetime.date(2014, 1, 6)],
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

    @parser.parsedoc
    def test_validate_active_accounts__unopened(self, entries, _, options_map):
        """
        2014-02-01 *
          Assets:US:Bank:Checking     100 USD
          Assets:US:Bank:Savings     -100 USD
        """
        errors = validation.validate_active_accounts(entries, options_map)
        self.assertEqual(2, len(errors))
        self.assertEqual([validation.ValidationError, validation.ValidationError],
                         list(map(type, errors)))


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
        meta = data.new_metadata('<validation_test>', 0)
        entries = [data.Document(meta, date, 'Assets:Account1',
                                 "/abs/path/to/something.pdf"),
                   data.Document(meta, date, 'Assets:Account2',
                                 "relative/something.pdf"),
                   data.Document(meta, date, 'Assets:Account2',
                                 "../something.pdf"),
                   data.Document(meta, date, 'Assets:Account2',
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


class TestValidate(cmptest.TestCase):

    @parser.parsedoc
    def test_validate(self, entries, _, options_map):
        """
        ;; Just trigger a few errors from here to ensure at least some of the plugins
        ;; tested above are run.

        2014-01-01 open Assets:Investments:Cash
        2014-01-01 open Assets:Investments:Stock   AAPL

        2014-06-23 * "Go positive"
          Assets:Investments:Stock   1 AAPL {41 USD}
          Assets:Investments:Cash

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock  -1 AAPL {42 USD}
          Assets:Investments:Cash

        2014-06-23 * "Use invalid currency"
          Assets:Investments:Stock   1 HOOG {500 USD}
          Assets:Investments:Cash

        """
        errors = validation.validate(entries, options_map)
        self.assertEqual(2, len(errors))
        self.assertTrue(any(re.match('Reducing position results', error.message)
                            for error in errors))
        self.assertTrue(any(re.match('Invalid currency', error.message)
                            for error in errors))
