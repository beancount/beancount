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

    # @parser.parsedoc
    def test_validate_open_close(self):
        raise NotImplementedError

    # @parser.parsedoc
    def test_validate_unused_accounts(self):
        raise NotImplementedError

    # @parser.parsedoc
    def test_validate_currency_constraints(self):
        raise NotImplementedError

    # @parser.parsedoc
    def test_validate_documents_paths(self):
        raise NotImplementedError

    # @parser.parsedoc
    def test_validate(self):
        raise NotImplementedError

        # print()
        # printer.print_errors(validation_errors)
