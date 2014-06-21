"""Validation tests.

These tests are intended to be run after all the plugins have transformed the
list of entries, just before serving them or generating reports from them. The
idea is to ensure a reasonable set of invariants and generate errors if those
invariants are violated.
"""
import datetime
import re

from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import cmptest
from beancount.ops import validation


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


class TestValidateBalances(cmptest.TestCase):

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
        2014-01-01 open  Equity:OpeningBalances

        2014-02-01 * "Invalid before"
          Assets:Temporary    1 USD
          Equity:OpeningBalances

        2014-02-02 note  Assets:Temporary "Invalid note entry"
        2014-02-03 pad   Assets:Temporary Equity:OpeningBalances

        2014-03-01 open  Assets:Temporary

        2014-04-01 * "Valid"
          Assets:Temporary    1 USD
          Equity:OpeningBalances

        2014-05-01 * "Unknown account"
          Assets:Temporary    1 USD
          Equity:ImUnknown

        2014-09-01 close Assets:Temporary

        2014-10-01 * "Invalid after"
          Assets:Temporary    1 USD
          Equity:OpeningBalances

        2014-10-02 note  Assets:Temporary "Invalid note entry again"

        """
        errors = validation.validate_active_accounts(entries, options_map)

        self.assertEqualEntries("""

        2014-02-01 * "Invalid before"
          Assets:Temporary    1 USD
          Equity:OpeningBalances

        2014-02-02 note  Assets:Temporary "Invalid note entry"
        2014-02-03 pad   Assets:Temporary Equity:OpeningBalances

        2014-05-01 * "Unknown account"
          Assets:Temporary    1 USD
          Equity:ImUnknown

        2014-10-01 * "Invalid after"
          Assets:Temporary    1 USD
          Equity:OpeningBalances

        2014-10-02 note  Assets:Temporary "Invalid note entry again"

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
        2014-01-01 open  Equity:OpeningBalances

        2014-02-01 *
          Assets:Account1            1 USD
          Assets:Account2            1 USD
          Equity:OpeningBalances

        2014-06-01 close Assets:Account2
        """
        errors = validation.validate_unused_accounts(entries, options_map)
        self.assertEqual(1, len(errors))
        self.assertEqual('Assets:Account3', errors[0].entry.account)








    # # @parser.parsedoc
    # def test_validate_currency_constraints(self):
    #     raise NotImplementedError

    # # @parser.parsedoc
    # def test_validate_documents_paths(self):
    #     raise NotImplementedError

    # # @parser.parsedoc
    # def test_validate(self):
    #     raise NotImplementedError

        #printer.print_errors(errors, prefix='\n\n\n')
