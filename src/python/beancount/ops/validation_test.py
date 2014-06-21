"""
FIXME TODO - Add validation tests.
"""
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
        validation_errors = validation.validate_inventory_booking(entries)

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
    def test_validate_open_close__duplicate_open(self, entries, _, __):
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
        errors = validation.validate_open_close(entries)
        self.assertEqual(['Assets:US:Bank:Checking2',
                          'Assets:US:Bank:Checking3'],
                         [error.entry.account for error in errors])

    @parser.parsedoc
    def test_validate_open_close__duplicate_close(self, entries, _, __):
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
        errors = validation.validate_open_close(entries)
        self.assertEqual(['Assets:US:Bank:Checking2',
                          'Assets:US:Bank:Checking3'],
                         [error.entry.account for error in errors])

    @parser.parsedoc
    def test_validate_open_close__close_unopened(self, entries, _, __):
        """
        2014-03-01 close Assets:US:Bank:Checking1
        """
        errors = validation.validate_open_close(entries)
        self.assertEqual(['Assets:US:Bank:Checking1'],
                         [error.entry.account for error in errors])

    @parser.parsedoc
    def test_validate_open_close__ordering(self, entries, _, __):
        """
        2014-03-01 open  Assets:US:Bank:Checking1
        2014-02-01 close Assets:US:Bank:Checking1
        """
        errors = validation.validate_open_close(entries)
        self.assertEqual(['Assets:US:Bank:Checking1'],
                         [error.entry.account for error in errors])




    # # @parser.parsedoc
    # def test_validate_unused_accounts(self):
    #     raise NotImplementedError

    # # @parser.parsedoc
    # def test_validate_currency_constraints(self):
    #     raise NotImplementedError

    # # @parser.parsedoc
    # def test_validate_documents_paths(self):
    #     raise NotImplementedError

    # # @parser.parsedoc
    # def test_validate(self):
    #     raise NotImplementedError

        # print()
        # printer.print_errors(validation_errors)
