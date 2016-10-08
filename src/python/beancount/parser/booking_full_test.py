__author__ = "Martin Blais <blais@furius.ca>"

import collections
import datetime
import textwrap
import functools
import unittest
import re
import io
import itertools
import pprint
from unittest import mock

from beancount.core.number import D
from beancount.core.amount import A
from beancount.core.number import MISSING
from beancount.core.position import CostSpec
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.core.inventory import from_string as I
#from beancount.utils.misc_utils import dictmap
from beancount.utils import test_utils
from beancount.core.data import Booking
from beancount.core import inventory
from beancount.core import position
from beancount.core import amount
from beancount.core import data
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import booking_full as bf
from beancount.parser import booking_simple as bs
from beancount.parser import cmptest
from beancount import loader


I = inventory.from_string


def _gen_missing_combinations(template, args):
    """Generate all possible expansions of args in template.

    Args:
      template: A string, the template in new-sytle formatting.
      args: A list of strings to be included or excluded from the template.
    Yields:
      Strings of formatted template.
    """
    for mask in range(2 ** len(args)):
        actual_args = [arg if not (1<<i & mask) else ''
                       for i, arg in enumerate(args)]
        yield template.format(*actual_args)


class TestAllInterpolationCombinations(cmptest.TestCase):

    def test_all_currency_interpolations(self):
        template = textwrap.dedent("""
          2015-10-02 *
            Assets:Account  {}
            Assets:Other
        """)
        for pos_template, args in [
                ('100.00 {:3}',
                 ['USD']),
                ('100.00 {:3} @ 1.20 {:3}',
                 ['USD', 'CAD']),
                ('10 {:4} {{100.00 {:3}}}',
                 ['HOOL', 'USD']),
                ('10 {:4} {{100.00 {:3}}} @ 120.00 {:3}',
                 ['HOOL', 'USD', 'USD']),
        ]:
            for string in _gen_missing_combinations(template.format(pos_template), args):
                entries, errors, _ = parser.parse_string(string)
                self.assertFalse(errors)

    def test_all_interpolation_combinations(self):
        template = textwrap.dedent("""
          2015-10-02 *
            Assets:Account  {}
            Assets:Other
        """)
        for pos_template, args in [
                ('{:7} {:3}',
                 ['100.00', 'USD']),
                ('{:7} {:3} @ {:7} {:3}',
                 ['100.00', 'USD', '1.20', 'CAD']),
                ('{:2} {:4} {{{:7} {:3}}}',
                 ['10', 'HOOL', '100.00', 'USD']),
                ('{:2} {:4} {{{:7} # {:7} USD}}',
                 ['10', 'HOOL', '100.00', '9.95']),
                ('{:2} {:4} {{{:7} # {:7} USD}} @ {:7} {:3}',
                 ['10', 'HOOL', '100.00', '9.95', '120.00', 'USD']),
        ]:
            for string in _gen_missing_combinations(template.format(pos_template), args):
                entries, errors, _ = parser.parse_string(string)
                for error in errors:
                    oss = io.StringIO()
                    printer.print_error(error, oss)
                    oss.write("In transaction:\n")
                    oss.write(string)
                    self.fail(oss.getvalue())


def indexes(groups):
    """Return only the index sets from currency categorized groups."""
    return {currency: {refer[0] for refer in refers}
            for currency, refers in groups.items()}


class TestCategorizeCurrencyGroup(unittest.TestCase):
    "Tests of per-currency categorization of postings."

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units__unambiguous(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account  100.00 USD
          Assets:Other   -100.00 USD

        2015-10-02 *
          Assets:Account         USD
          Assets:Other   -100.00 USD
        """
        for entry in entries:
            groups, errors = bf.categorize_by_currency(entry, {})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0, 1}}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units__ambiguous(self, entries, _, options_map):
        """
        ;; Uses the other legs to disambiguate.
        2015-10-02 *
          Assets:Account  100.00
          Assets:Other   -100.00 USD

        ;; Uses the inventory contents to disambiguate.
        2015-10-02 *
          Assets:Account  100.00
          Assets:Other
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

        groups, errors = bf.categorize_by_currency(
            entries[1], {'Assets:Account': I('1.00 USD')})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))
        groups, errors = bf.categorize_by_currency(
            entries[1], {})
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Failed to categorize posting')
        self.assertEqual({}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units_price__unambiguous(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account  100.00 USD @ 1.20 CAD
          Assets:Other   -120.00 CAD

        2015-10-02 *
          Assets:Account  100.00     @ 1.20 CAD
          Assets:Other   -120.00 CAD
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0, 1}}, indexes(groups))

        groups, errors = bf.categorize_by_currency(
            entries[1], {'Assets:Account': I('1.00 USD')})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0, 1}}, indexes(groups))
        groups, errors = bf.categorize_by_currency(
            entries[1], {})
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'CAD': {0, 1}}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units_price__ambiguous(self, entries, _, options_map):
        """
        ;; Uses the other legs to disambiguate.
        2015-10-02 *
          Assets:Account  100.00 USD @ 1.20
          Assets:Other   -120.00 CAD

        2015-10-02 *
          Assets:Account  100.00     @ 1.20
          Assets:Other   -120.00 CAD

        ;; These cases fail, because using the inventory tells nothing which price to
        ;; convert from.
        2015-10-02 *
          Assets:Account  100.00 USD @ 1.20
          Assets:Other

        2015-10-02 *
          Assets:Account  100.00     @ 1.20
          Assets:Other
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0, 1}}, indexes(groups))

        groups, errors = bf.categorize_by_currency(
            entries[1], {'Assets:Account': I('1.00 USD')})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0, 1}}, indexes(groups))
        groups, errors = bf.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'CAD': {0, 1}}, indexes(groups))

        for i in 2, 3:
            groups, errors = bf.categorize_by_currency(entries[i], {})
            self.assertEqual(1, len(errors))
            self.assertRegex(errors[0].message, 'Failed to categorize posting')
            self.assertEqual({}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units_cost__unambiguous(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account    10 HOOL {100.00 USD}
          Assets:Other   -1000 USD

        2015-10-02 *
          Assets:Account    10      {100.00 USD}
          Assets:Other   -1000 USD
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

        groups, errors = bf.categorize_by_currency(
            entries[1], {'Assets:Account': I('1 HOOL {1.00 USD}')})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))
        groups, errors = bf.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units_cost__ambiguous(self, entries, _, options_map):
        """
        ;; Uses the other legs to disambiguate.
        2015-10-02 *
          Assets:Account    10 HOOL {100.00    }
          Assets:Other   -1000 USD

        2015-10-02 *
          Assets:Account    10      {100.00    }
          Assets:Other   -1000 USD

        ;; Disambiguate using the inventory cost, if some other lots exist in the
        ;; balance.
        2015-10-02 *
          Assets:Account    10 HOOL {100.00    }
          Assets:Other

        2015-10-02 *
          Assets:Account    10      {100.00    }
          Assets:Other
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

        groups, errors = bf.categorize_by_currency(
            entries[1], {'Assets:Account': I('1 HOOL {1.00 USD}')})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))
        groups, errors = bf.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

        for i in 2, 3:
            groups, errors = bf.categorize_by_currency(
                entries[i], {'Assets:Account': I('1 HOOL {1.00 USD}')})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0, 1}}, indexes(groups))
            groups, errors = bf.categorize_by_currency(
                entries[i], {})
            self.assertEqual(1, len(errors))
            self.assertRegex(errors[0].message, 'Failed to categorize posting')
            self.assertEqual({}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units_cost_price__unambiguous(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account  10 HOOL {100.00 USD} @ 120.00 USD
          Assets:Other

        2015-10-02 *
          Assets:Account  10      {100.00 USD} @ 120.00 USD
          Assets:Other

        2015-10-02 *
          Assets:Account  10 HOOL {100.00    } @ 120.00 USD
          Assets:Other

        2015-10-02 *
          Assets:Account  10      {100.00    } @ 120.00 USD
          Assets:Other

        2015-10-02 *
          Assets:Account  10 HOOL {100.00 USD} @ 120.00
          Assets:Other

        2015-10-02 *
          Assets:Account  10      {100.00 USD} @ 120.00
          Assets:Other
        """
        for i in 0, 2, 4:
            groups, errors = bf.categorize_by_currency(entries[i], {})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0, 1}}, indexes(groups))

        for i in 1, 3, 5:
            groups, errors = bf.categorize_by_currency(entries[i], {})
            self.assertEqual(1, len(errors))
            self.assertRegex(errors[0].message, 'Could not resolve units currency')
            self.assertEqual({'USD': {0, 1}}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__units_cost_price__ambiguous(self, entries, _, options_map):
        """
        ;; Uses the other legs to disambiguate.
        2015-10-02 *
          Assets:Account   10 HOOL {100.00    } @ 120.00
          Assets:Other  -1000 USD

        2015-10-02 *
          Assets:Account   10      {100.00    } @ 120.00
          Assets:Other  -1000 USD

        ;; Uses the cost to disambiguate.
        2015-10-02 *
          Assets:Account   10 HOOL {100.00    } @ 120.00
          Assets:Other

        2015-10-02 *
          Assets:Account   10      {100.00    } @ 120.00
          Assets:Other
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

        groups, errors = bf.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'USD': {0, 1}}, indexes(groups))

        for i in 2, 3:
            groups, errors = bf.categorize_by_currency(
                entries[i], {'Assets:Account': I('1 HOOL {1.00 USD}')})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0, 1}}, indexes(groups))
            groups, errors = bf.categorize_by_currency(
                entries[i], {})
            self.assertTrue(errors)
            self.assertRegex(errors[0].message, 'Failed to categorize posting')
            self.assertEqual({}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__multiple_auto_postings(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Account   100.00 CAD
          Assets:Other
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0, 2}, 'CAD': {1, 2}}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__redundant_auto_postings(self, entries, _, options_map):
        """
        ;; Uses the other legs to disambiguate.
        2015-10-02 *
          Assets:Account   10 HOOL {100.00    } @ 120.00
          Assets:Other
          Assets:Other
        """
        groups, errors = bf.categorize_by_currency(entries[0], {})
        self.assertTrue(errors)

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__two_unknown_postings(self, entries, _, options_map):
        """
        2016-05-02 *
          Assets:Account          -40 HOOL {}
          Assets:Account          -35 HOOL {}
        """
        balances = {'Assets:Account': I('50 HOOL {115.00 USD, 2016-01-15}, '
                                        '50 HOOL {116.00 USD, 2016-01-16}')}
        groups, errors = bf.categorize_by_currency(entries[0], balances)
        self.assertEqual(1, len(groups))
        self.assertEqual(2, len(groups['USD']))
        self.assertFalse(errors)

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__against_mixed(self, entries, _, options_map):
        """
        2016-05-02 *
          Assets:Account          -40 HOOL {}
        """
        balances = {'Assets:Account': I('50 HOOL {100.00 USD, 2016-01-15}, '
                                        '50 HOOL { 50.00 CAD, 2016-01-15}')}
        groups, errors = bf.categorize_by_currency(entries[0], balances)
        self.assertEqual(0, len(groups))
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, 'Failed to categorize posting')


class TestReplaceCurrenciesInGroup(unittest.TestCase):
    "Tests the replacement of currencies inferred in the categorization step."

    def check(self, expected, entry):
        groups, errors = bf.categorize_by_currency(entry, {})
        self.assertFalse(errors)
        posting_groups = bf.replace_currencies(entry.postings, groups)
        check_groups = {
            currency: [(posting.account,
                        posting.units.currency,
                        posting.cost.currency if posting.cost else None,
                        posting.price.currency if posting.price else None)
                       for posting in postings]
            for currency, postings in posting_groups.items()}
        self.assertEqual(expected, check_groups)

        # Check all the postings are unique instances.
        all_postings = [posting
                        for postings in posting_groups.values()
                        for posting in postings]
        self.assertEqual(len(set(map(id, all_postings))), len(all_postings))

    @parser.parse_doc(allow_incomplete=True)
    def test_auto_posting(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Other

        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Account   100.00 CAD
          Assets:Other

        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Account   100.00 CAD
          Assets:US:Other  USD
          Assets:CA:Other  CAD
        """
        self.check({'USD': [('Assets:Account', 'USD', None, None),
                            ('Assets:Other', 'USD', None, None)]}, entries[0])
        self.check({'CAD': [('Assets:Account', 'CAD', None, None),
                            ('Assets:Other', 'CAD', None, None)],
                    'USD': [('Assets:Account', 'USD', None, None),
                            ('Assets:Other', 'USD', None, None)]}, entries[1])
        self.check({'CAD': [('Assets:Account', 'CAD', None, None),
                            ('Assets:CA:Other', 'CAD', None, None)],
                    'USD': [('Assets:Account', 'USD', None, None),
                            ('Assets:US:Other', 'USD', None, None)]}, entries[2])

    @parser.parse_doc(allow_incomplete=True)
    def test_missing(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   100.00
          Assets:Another  -100.00 USD

        2015-10-02 *
          Assets:Account   100.00 USD @ 120.00
          Assets:Another  -120.00 CAD

        2015-10-02 *
          Assets:Account   10 HOOL {100.00}
          Assets:Another  -1000.00 USD

        2015-10-02 *
          Assets:Account   10 HOOL {100.00} @ 120.00 USD
          Assets:Another  -1000.00 USD
        2015-10-02 *
          Assets:Account   10 HOOL {100.00 USD} @ 120.00
          Assets:Another  -1000.00 USD
        """
        self.check({'USD': [('Assets:Account', 'USD', None, None),
                            ('Assets:Another', 'USD', None, None)]}, entries[0])

        self.check({'CAD': [('Assets:Account', 'USD', None, 'CAD'),
                            ('Assets:Another', 'CAD', None, None)]}, entries[1])

        self.check({'USD': [('Assets:Account', 'HOOL', 'USD', None),
                            ('Assets:Another', 'USD', None, None)]}, entries[2])

        self.check({'USD': [('Assets:Account', 'HOOL', 'USD', 'USD'),
                            ('Assets:Another', 'USD', None, None)]}, entries[3])
        self.check({'USD': [('Assets:Account', 'HOOL', 'USD', 'USD'),
                            ('Assets:Another', 'USD', None, None)]}, entries[4])


def normalize_postings(postings):
    """Normalize a list of postings ready for direct comparison, for testing.

    This sorts them by line order and removes metadata.

    Args:
      postings: A list of Posting instances.
    Returns:
      A new reordered and normalized Posting instances.
    """
    return [posting._replace(meta=None)
            for posting in sorted(postings,
                                  key=lambda posting: posting.meta['lineno'])]


class TestInterpolateCurrencyGroup(unittest.TestCase):
    "Tests the replacement of currencies inferred in the categorization step."

    maxDiff = 8192

    # 'expected' is a mapping of currency to tuples of
    #   interpolated: A boolean, asserting the return value of interpolate_group().
    #   string: A string, to be parsed to obtain the resulting Posting instances.
    #   errors: A list of error strings to check against the interpolation for that group.
    def check(self, entry, expected, balances=None, debug=False):
        if balances is None:
            balances = {}

        groups, errors = bf.categorize_by_currency(entry, balances)
        self.assertFalse(errors)
        posting_groups = bf.replace_currencies(entry.postings, groups)
        for currency, postings in posting_groups.items():
            try:
                exp_interpolated, exp_string, exp_errors = expected[currency]
            except KeyError:
                self.fail("Currency {} is unexpected".format(currency))

            # Run the interpolation for that group.
            new_postings, errors, interpolated = bf.interpolate_group(
                postings, balances, currency)

            # Print out infos for troubleshooting.
            if debug:
                print()
                for posting in new_postings:
                    print(posting)
                for error in errors:
                    print(error)
                print()

            # Check the expectation on interpolation.
            self.assertEqual(exp_interpolated, interpolated)

            # Check the expected number of errors.
            self.assertEqual(len(exp_errors) if exp_errors else 0, len(errors))
            if exp_errors:
                for exp_error in exp_errors:
                    self.assertTrue(any(re.match(exp_error, error.message)
                                        for error in errors))

            # Check the expected postings.
            if exp_string is not None:
                exp_entries, err1, _ = parser.parse_string(exp_string, dedent=True)
                exp_entries, err2 = bs.convert_lot_specs_to_lots(exp_entries)
                self.assertFalse(err1 or err2, "Internal error in test")
                self.assertEqual(1, len(exp_entries),
                                 "Internal error, expected one entry")
                exp_postings = normalize_postings(exp_entries[0].postings)
                self.assertEqual(exp_postings, normalize_postings(new_postings))

        return errors

    @parser.parse_doc(allow_incomplete=True)
    def test_complete(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Other    -100.00 USD
        """
        self.check(entries[0], {'USD': (False, None, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_impossible_twomiss_diff_units(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account          USD
          Assets:Other            USD
        """
        self.check(entries[0], {
            'USD': (False, None, ["Too many missing numbers for currency group"])})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_impossible_twomiss_diff_cost_and_units(self,
                                                               entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   2 HOOL {USD}
          Assets:Other       USD
        """
        self.check(entries[0], {
            'USD': (False, None, ["Too many missing numbers for currency group"])})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_impossible_miss_same_posting(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account            HOOL {USD}
          Assets:Other      -100.00 USD
        """
        self.check(entries[0], {
            'USD': (False, None, ["Too many missing numbers for currency group"])})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_units(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account          USD
          Assets:Other    -100.00 USD

        2015-10-02 *
          Assets:Account          HOOL {100.00 # 9.95 USD}
          Assets:Other   -1009.95 USD

        2015-10-02 *
          Assets:Account          HOOL {100.00 USD}
          Assets:Other   -1000.00 USD

        2015-10-02 *
          Assets:Account          HOOL {100.00 USD} @ 110.00 USD
          Assets:Other   -1000.00 USD

        2015-10-02 *
          Assets:Account          HOOL {0 # 1009.95 USD}
          Assets:Other   -1009.95 USD

        2015-10-02 *
          Assets:Account          CAD @ 1.25 USD
          Assets:Other    -100.00 USD
        """
        self.check(entries[0], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account   100.00 USD
                Assets:Other    -100.00 USD
            """, None)})

        self.check(entries[1], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 # 9.95 USD}
                Assets:Other   -1009.95 USD
            """, None)})

        self.check(entries[2], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 USD}
                Assets:Other   -1000.00 USD
            """, None)})

        self.check(entries[3], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 USD} @ 110.00 USD
                Assets:Other   -1000.00 USD
            """, None)})

        # Check impossible case.
        self.check(entries[4], {
            'USD': (True, None, ["Cannot infer per-unit cost only from total"])})

        self.check(entries[5], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account    80.00 CAD @ 1.25 USD
                Assets:Other    -100.00 USD
            """, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_cost_both(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account       10 HOOL {USD}
          Assets:Other   -1009.95 USD

        2015-10-02 *
          Assets:Account       10 HOOL {USD} @ 110.00 USD
          Assets:Other   -1009.95 USD

        2015-10-02 *
          Assets:Account       10 HOOL {USD, "blah"}
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.995 USD}
                Assets:Other   -1009.95 USD
            """, None)})
        self.check(entries[1], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.995 USD} @ 110.00 USD
                Assets:Other   -1009.95 USD
            """, None)})
        self.check(entries[2], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.995 USD, "blah"}
                Assets:Other   -1009.95 USD
            """, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_cost_per(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account       10 HOOL {# 9.95 USD}
          Assets:Other   -1009.95 USD

        2015-10-02 *
          Assets:Account       10 HOOL {# 9.95 USD} @ 110.00 USD
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 # 9.95 USD}
                Assets:Other   -1009.95 USD
            """, None)})
        self.check(entries[1], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 # 9.95 USD} @ 110.00 USD
                Assets:Other   -1009.95 USD
            """, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_cost_total(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account       10 HOOL {100.00 # USD}
          Assets:Other   -1009.95 USD

        2015-10-02 *
          Assets:Account       10 HOOL {100.00 # USD} @ 110.00 USD
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 # 9.95 USD}
                Assets:Other   -1009.95 USD
            """, None)})
        self.check(entries[1], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account       10 HOOL {100.00 # 9.95 USD} @ 110.00 USD
                Assets:Other   -1009.95 USD
            """, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_price(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account  125.00 CAD @ USD
          Assets:Other   -100.00 USD

        2015-10-02 *
          Assets:Account       10 HOOL {100.00 # 9.95 USD} @ USD
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], {
            'USD': (True, """
              2015-10-02 *
                Assets:Account  125.00 CAD @ 0.8 USD
                Assets:Other   -100.00 USD
            """, None)})
        self.check(entries[1], {
            'USD': (True, None,
                    ["Cannot infer price for postings with units held at cost"])})

    @parser.parse_doc(allow_incomplete=True)
    def test_multiple_groups(self, entries, _, options_map):
        """
          2010-05-28 *
            Assets:Account1     100.00 CAD
            Assets:Account2     -80.00 CAD
            Assets:Account3            CAD
            Assets:Account4     200.00 USD
            Assets:Account5            USD

          2010-05-28 *
            Assets:Account1     100.00 CAD
            Assets:Account2     -80.00 CAD
            Assets:Account3     -20.00 CAD
            Assets:Account4     200.00 USD
            Assets:Account5            USD
        """
        for entry in entries:
            self.check(entries[0], {
                'CAD': (True, """
                  2010-05-28 *
                    Assets:Account1     100.00 CAD
                    Assets:Account2     -80.00 CAD
                    Assets:Account3     -20.00 CAD
                """, None),
                'USD': (True, """
                  2010-05-28 *
                    Assets:Account4     200.00 USD
                    Assets:Account5    -200.00 USD
                """, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_underdefined(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account        -10 HOOL {USD} @ 120.00 USD
          Assets:Other      1000.00 USD
          Income:PnL
        """
        # Interpolation and booking both required... impossible.
        self.check(entries[0], {
            'USD': (False, None, ["Too many missing numbers for currency group"])
        })

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_underdefined2(self, entries, _, options_map):
        """
        1997-03-16 * "Transfer"
          Assets:CA:Life:RRSP:Cash          2000 CAD
          Assets:CA:Pop:Checking
          Assets:CA:CRA:PreTaxRSP:Allowed  -2000 RSPCAD
          Assets:CA:CRA:PreTaxRSP:Unused    2000 RSPCAD
        """
        # Interpolation and booking both required... impossible.
        self.check(entries[0], {
            'CAD': (True, """
              1997-03-16 *
                Assets:CA:Life:RRSP:Cash    2000 CAD
                Assets:CA:Pop:Checking     -2000 CAD
            """, None),
            'RSPCAD': (False, """
              1997-03-16 *
                Assets:CA:CRA:PreTaxRSP:Allowed  -2000 RSPCAD
                Assets:CA:CRA:PreTaxRSP:Unused    2000 RSPCAD
            """, None)})

    @parser.parse_doc(allow_incomplete=True)
    def test_auto_posting__superfluous_unused(self, entries, errors, _):
        """
          2000-01-01 open Assets:Account1
          2000-01-01 open Assets:Account2

          2016-04-23 * ""
            Assets:Account1     0.00 USD
            Assets:Account2
        """
        self.check(entries[-1], {
            'USD': (False, """
              2016-04-23 * ""
                Assets:Account1     0.00 USD
            """, None)})
        # FIXME: This ought to return a "Superfluous posting" error for Account2 only.

    @parser.parse_doc(allow_incomplete=True)
    def test_auto_posting__superfluous_unneeded(self, entries, errors, _):
        """
          2000-01-01 open Assets:Account1
          2000-01-01 open Assets:Account2
          2000-01-01 open Assets:Account3

          2016-04-23 * ""
            Assets:Account1   100.00 USD
            Assets:Account2  -100.00 USD
            Assets:Account3
        """
        self.check(entries[-1], {
            'USD': (False, """
              2016-04-23 * ""
                Assets:Account1   100.00 USD
                Assets:Account2  -100.00 USD
            """, None)})
        # FIXME: This ought to return a "Superfluous posting" error for Account3 only.

    @parser.parse_doc(allow_incomplete=True)
    def test_auto_posting__superfluous_needed_one_side(self, entries, errors, _):
        """
          2000-01-01 open Assets:Account1
          2000-01-01 open Assets:Account2
          2000-01-01 open Assets:Account3
          2000-01-01 open Assets:Account4
          2000-01-01 open Assets:Account5

          2016-04-23 * ""
            Assets:Account1   100.00 USD
            Assets:Account2  -100.00 USD
            Assets:Account3   100.00 CAD
            Assets:Account4   -99.00 CAD
            Assets:Account5
        """
        self.check(entries[-1], {
            'USD': (False, """
              2016-04-23 * ""
                Assets:Account1   100.00 USD
                Assets:Account2  -100.00 USD
            """, None),
            'CAD': (True, """
              2016-04-23 * ""
                Assets:Account3   100.00 CAD
                Assets:Account4   -99.00 CAD
                Assets:Account5    -1.00 CAD
            """, None)})


class TestComputeCostNumber(unittest.TestCase):

    date = datetime.date(2016, 1, 1)

    def test_missing_per(self):
        self.assertEqual(
            None,
            bf.compute_cost_number(
                position.CostSpec(MISSING, D('1'), 'USD', None, None, False),
                amount.from_string('12 HOOL')))

    def test_missing_total(self):
        self.assertEqual(
            None,
            bf.compute_cost_number(
                position.CostSpec(D('1'), MISSING, 'USD', None, None, False),
                amount.from_string('12 HOOL')))

    def test_both_none(self):
        self.assertEqual(
            None,
            bf.compute_cost_number(
                position.CostSpec(None, None, 'USD', None, None, False),
                amount.from_string('12 HOOL')))

    def test_total_only(self):
        self.assertEqual(
            D('4'),
            bf.compute_cost_number(
                position.CostSpec(None, D('48'), 'USD', None, None, False),
                amount.from_string('12 HOOL')))

    def test_per_only(self):
        self.assertEqual(
            D('4'),
            bf.compute_cost_number(
                position.CostSpec(D('4'), None, 'USD', None, None, False),
                amount.from_string('12 HOOL')))

    def test_both(self):
        self.assertEqual(
            D('3.5'),
            bf.compute_cost_number(
                position.CostSpec(D('3'), D('6'), 'USD', self.date, None, False),
                amount.from_string('12 HOOL')))

    def test_no_currency(self):
        self.assertEqual(
            D('3.5'),
            bf.compute_cost_number(
                position.CostSpec(D('3'), D('6'), None, self.date, None, False),
                amount.from_string('12 HOOL')))


class TestParseBookingOptions(cmptest.TestCase):

    @loader.load_doc()
    def test_booking_algorithm__simple(self, entries, _, options_map):
        """
          option "experiment_booking_algorithm" "SIMPLE"
        """
        self.assertEqual("SIMPLE", options_map["experiment_booking_algorithm"])

    @loader.load_doc()
    def test_booking_algorithm__full(self, entries, _, options_map):
        """
          option "experiment_booking_algorithm" "FULL"
        """
        self.assertEqual("FULL", options_map["experiment_booking_algorithm"])

    @loader.load_doc(expect_errors=True)
    def test_booking_algorithm__invalid(self, entries, errors, options_map):
        """
          option "experiment_booking_algorithm" "XXX"
        """
        self.assertEqual(1, len(errors))

    @loader.load_doc()
    def test_booking_method__strict(self, entries, _, options_map):
        """
          option "booking_method" "STRICT"
        """
        self.assertEqual(Booking.STRICT, options_map["booking_method"])

    @loader.load_doc()
    def test_booking_method__average(self, entries, _, options_map):
        """
          option "booking_method" "AVERAGE"
        """
        self.assertEqual(Booking.AVERAGE, options_map["booking_method"])

    @loader.load_doc(expect_errors=True)
    def test_booking_method__invalid(self, _, errors, options_map):
        """
          option "booking_method" "XXX"
        """
        self.assertEqual(1, len(errors))
        self.assertEqual(Booking.STRICT,
                         options_map["booking_method"])


# Base class and helpers for writing booking tests.
#
# Booking involves a large number of scenarios, and because of this we leverage
# Beancount's syntax to provide test input and expected values in the function's
# docstrings. See _book() for details of how this is interpreted. What
# follows is a helper base class for these tests.

_UNSET = object()

def find_first_with_tag(tag, entries, default=_UNSET):
    """Return the first entry matching the given tag."""
    found_entry = None
    for entry in entries:
        if tag in entry.tags:
            if found_entry is not None:
                raise KeyError("Multiple entries with tag #{}".format(tag))
            found_entry = entry
    if found_entry is not None:
        return found_entry
    if default is _UNSET:
        raise KeyError("Entry with tag #{} is missing".format(tag))
    else:
        return default


@test_utils.nottest
def book_test(booking_method):
    "A decorator factory for all booking tests. This calls _book() below."
    def decorator(func):
        @parser.parse_doc(allow_incomplete=True)
        @functools.wraps(func)
        def wrapper(self, entries, unused_errors, options_map):
            self._book(entries, options_map, booking_method)
            return func(self, entries, options_map)
        return wrapper
    return decorator


def _BM(booking_method):
    return collections.defaultdict(lambda: booking_method)


class _BookingTestBase(unittest.TestCase):
    """A base class for all booking scenario tsts.

    This reuses Beancount's input syntax to create a DSL for writing tests. The
    purpose is to easily write a single test per booking scenario for the
    various functions computing each part of the booking process.
    """

    maxDiff = 8192

    # A set of the valid tags on transactions.
    VALID_TAGS = {'ante',
                  'ex',
                  'apply',
                  'booked',
                  'ambi-matches',
                  'ambi-resolved',
                  'reduced',
                  'print'}

    def _book(self, entries, options_map, booking_method):
        """Test a call to book a particular scenario.

        This method will call 'book' with a subset of the entries provided to
        it. It interprets the list of entries as a simple DSL where some are fed
        to the routine and some are interpreted as assertions.

        Args:
          entries: A list of entries with particular tags:

            - An entry with #ante provides entries to be applied before the
              reduction. This is a mechanism to provide the ante-inventory of an
              account. This is optional, the test will be run on an empty
              inventory if there's no such entry.

            - An entry with #apply provides a single reducing posting for the
              call. This describes the type of reduction to be applied on the
              ante-inventory. This is the test action we care about.

              Note that there may be many such entries. If so, the test is run
              separately for each of those entries, everything else being kept
              the same.

            - An entry with #booked describes and asserts the list of postings
              which were resolved from the call to book().

            - An entry with #ex describes the expected contents of and asserts
              the ex-inventory that results from booking on the reduction
              account.

            Further, some internal calls are traced and can be asserted against:

            - An entry with #ambi-matches asserts the expected set of matching
              postings provided to handle_ambiguous_matches().

            - An entry with #ambi-resolved describes and asserts the list of
              postings which were resolved from the call fo
              handle_ambiguous_matches().

            - An entry with #reduced describes and asserts the list of postings
              which were resolved from the call to book_reductions().

            Finally, if you tag some entries with #print they will be printed by
            the test routine. This is a convenient debugging helper while
            crafting a new test.

          options_map: An options dict. The default booking method is consulted.
          booking_method: A data.Booking enum value.

        In order to assert errors, create an 'error' metadata field with a
        regular expression as value. If will be checked against the list of
        errors generated by the particular call. This applies to #booked,
        #reduced and #ambi-resolved transactions.

        If a posting has a 'S' flag, it is not converted to a Cost and we'll
        expect a CostSpec to be set on it.

        """
        # Make sure that all the tags provided in the test input are valid.
        for entry in entries:
            assert entry.tags
            for tag in entry.tags:
                assert tag in self.VALID_TAGS, tag
            remaining_meta = set(entry.meta.keys()) - set(['error', 'filename', 'lineno'])
            assert not remaining_meta, remaining_meta

        # Print all explicitly requested entries.
        for entry in entries:
            if 'print' in entry.tags:
                printer.print_entry(entry)

        # Find all entries with tags.
        entries_apply = [entry
                         for entry in entries
                         if 'apply' in entry.tags]
        assert entries_apply, "Internal error: No 'apply' entries found in test input"
        entries_other = [entry
                         for entry in entries
                         if 'apply' not in entry.tags]

        # Dispatch all the entries.
        for entry_apply in entries_apply:
            self._book_one(entry_apply, entries_other, options_map, booking_method)

    def _book_one(self, entry_apply, entries, options_map, booking_method):
        """See _book(). This is the same but with a single #apply entry."""

        all_entries = [entry_apply] + entries

        # Override the booking method.
        options_map = options_map.copy()
        options_map['booking_method'] = booking_method
        input_entries = []

        # Fetch the 'ante' entry.
        entry_ante = find_first_with_tag('ante', all_entries, None)
        if entry_ante:
            input_entries.append(entry_ante)

        # Check that the 'apply' entry has a single posting only.
        # assert len(entry_apply.postings) == 1, (
        #     "Internal error: 'apply' entry has more than one posting")
        assert len(set(posting.account for posting in entry_apply.postings)) == 1, (
            "Accounts don't match for 'apply' entry")
        account = entry_apply.postings[0].account
        input_entries.append(entry_apply)

        # Call the booking routine.
        methods = collections.defaultdict(lambda: booking_method)
        handle_patch = mock.patch.object(bf, 'handle_ambiguous_matches',
                                         test_utils.record(bf.handle_ambiguous_matches))
        reduce_patch = mock.patch.object(bf, 'book_reductions',
                                         test_utils.record(bf.book_reductions))
        with handle_patch as handle_mock, reduce_patch as reduce_mock:
            book_entries, book_errors, balances = bf._book(input_entries, options_map,
                                                           methods)

        ## FIXME: remove
        # print('-------------------------------- input_entries')
        # printer.print_entries(input_entries)
        # print('-------------------------------- book_entries')
        # printer.print_entries(book_entries)

        # If requested, check the result of booking.
        entry_booked = find_first_with_tag('booked', all_entries, None)
        if entry_booked:
            # Check the output postings and errors.
            self.assertErrors(entry_booked, book_errors)
            actual_postings = book_entries[-1].postings if book_entries else []
            if not book_errors and (entry_booked.postings or actual_postings):
                self.assertPostings(entry_booked.postings, actual_postings)

        # Check that the resulting inventory balance matches that of the 'ex'
        # entry, if present.
        entry_ex = find_first_with_tag('ex', all_entries, None)
        if entry_ex:
            inv_expected = inventory.Inventory()
            for posting in entry_ex.postings:
                inv_expected.add_amount(posting.units,
                                        bs.convert_spec_to_cost(posting.units,
                                                                posting.cost))
            self.assertEqual(inv_expected, balances[account])

        # If requested, check the output values to the last call to
        # book_reductions().
        entry_reduced = find_first_with_tag('reduced', all_entries, None)
        if entry_reduced:
            # Note: If there are multiple currencies in the input entries here
            # this equality may not match.
            self.assertEqual(len(input_entries), len(reduce_mock.calls))
            reduce_postings, reduce_errors = reduce_mock.calls[-1].return_value
            self.assertPostings(entry_reduced.postings, reduce_postings)
            self.assertErrors(entry_reduced, reduce_errors)

        # If requested, check the input and output values to the last call to
        # handle_ambiguous_matches().
        entry_matches = find_first_with_tag('ambi-matches', all_entries, None)
        entry_resolved = find_first_with_tag('ambi-resolved', all_entries, None)
        if entry_matches or entry_resolved:
            self.assertEqual(1, len(handle_mock.calls))
            call = handle_mock.calls[-1]

            # Convert the list of expected matching postings to positions and
            # compare those with the list of matches provided to
            # handle_ambiguous_matches().
            if entry_matches:
                actual_matches = call.args[2]
                expected_matches = [Position(posting.units,
                                             bs.convert_spec_to_cost(posting.units,
                                                                     posting.cost))
                                    for posting in entry_matches.postings]
                self.assertEqual(sorted(expected_matches), sorted(actual_matches))

            # Convert the list of expected resolved postings to those returned
            # by handle_ambiguous_matches().
            if entry_resolved:
                resolved_actual, resolved_errors = call.return_value
                self.assertPostings(entry_resolved.postings, resolved_actual)

    def assertErrors(self, entry, errors):
        """Check expected errors specified in the metadata of an entry against actual
        returned errors.

        Args:
          entry: A Transaction instance, with some metadata.
          errors: A list of actual errors generated.
        """
        error_regexp = entry.meta.get('error', None)
        if error_regexp:
            self.assertEqual(1, len(errors))
            self.assertRegex(errors[0].message, error_regexp)
        else:
            self.assertFalse(errors)

    def assertPostings(self, expected_postings, actual_postings):
        """Compare a list of expected postings against actual.

        This ignores metadata.

        Args:
          expected_postings: A list of Posting instances. Their CostSpec attribute
            is converted to a Cost attribute before comparing them, unless there is
            a 'S' flag on the posting.
          actual_postings: A list of actual Posting instances.
        """
        # Note: In this function we have to remove the flags to compare them
        # because the flag is already used by the testing method's DSL to
        # indicate to keep a CostSpec, so we cannot assert other flags like
        # this, like for example the 'M' for merged flags for average cost
        # booking.

        # Optionally convert CostSpec to Cost, removing the flag.
        expected_postings = [
            posting._replace(meta=None,
                             flag=None,
                             cost=(posting.cost
                                   if posting.flag == 'S' else
                                   bs.convert_spec_to_cost(posting.units,
                                                           posting.cost)))
            for posting in expected_postings]

        actual_postings = [
            posting._replace(meta=None,
                             flag=None)
            for posting in actual_postings]

        # Compare them while maintaining their order.
        self.assertEqual(len(expected_postings), len(actual_postings))
        for (posting_expected,
             actual_postings) in zip(expected_postings,
                                     actual_postings):
            self.assertEqual(posting_expected, actual_postings,
                             "Postings don't match:\n{} !=\n{}".format(posting_expected,
                                                                       actual_postings))


class TestBookAugmentations(_BookingTestBase):
    """
    Test that the augmentations are left alone by the book_reductions() function.
    """

    @book_test(Booking.STRICT)
    def test_augment__from_empty__no_cost__pos(self, _, __):
        """
        2015-10-01 * #apply
          Assets:Account           1 USD

        2015-10-01 * #ex #booked #reduced
          Assets:Account           1 USD
        """

    @book_test(Booking.STRICT)
    def test_augment__from_empty__no_cost__neg(self, _, __):
        """
        2015-10-01 * #apply
          Assets:Account          -1 USD

        2015-10-01 * #ex #booked #reduced
          Assets:Account           -1 USD
        """

    @book_test(Booking.STRICT)
    def test_augment__from_empty__at_cost__pos(self, _, __):
        """
        2015-10-01 * #apply
          Assets:Account          1 HOOL {100.00 USD}

        2015-10-01 * #ex #booked
          Assets:Account          1 HOOL {100.00 USD, 2015-10-01}

        2015-10-01 * #reduced
          S Assets:Account        1 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.STRICT)
    def test_augment__from_empty__at_cost__neg(self, _, __):
        """
        2015-10-01 * #apply
          Assets:Account          -1 HOOL {100.00 USD}

        2015-10-01 * #ex #booked
          Assets:Account          -1 HOOL {100.00 USD, 2015-10-01}

        2015-10-01 * #reduced
          S Assets:Account        -1 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.STRICT)
    def test_augment__from_empty__incomplete_cost__empty(self, entries, __):
        """
        2015-10-01 * #apply
          Assets:Account          1 HOOL {}

        2015-10-01 * #booked
          error: "Failed to categorize posting"
        """
        # Further test what would happen if book_reductions() would be called anyhow.
        entry = find_first_with_tag('apply', entries)
        postings, errors = bf.book_reductions(entry, entry.postings,
                                              {}, _BM(Booking.STRICT))
        self.assertFalse(errors)
        self.assertEqual(
            CostSpec(MISSING, None, MISSING, datetime.date(2015, 10, 1), None, False),
            postings[0].cost)

    @book_test(Booking.STRICT)
    def test_augment__from_empty__incomplete_cost__with_currency(self, entries, __):
        """
        2015-10-01 * #apply
          Assets:Account          1 HOOL {USD}

        2015-10-01 * #booked
          Assets:Account          1 HOOL {0 USD, 2015-10-01}

        2015-10-01 * #reduced
          S Assets:Account          1 HOOL {USD, 2015-10-01}
        """
        # Further test what would happen if book_reductions() would be called anyhow.
        entry = find_first_with_tag('apply', entries)
        postings, errors = bf.book_reductions(entry, entry.postings,
                                              {}, _BM(Booking.STRICT))
        self.assertFalse(errors)
        self.assertEqual(
            CostSpec(MISSING, None, 'USD', datetime.date(2015, 10, 1), None, False),
            postings[0].cost)


class TestBookReductions(_BookingTestBase):

    # Test that reductions with no cost basis are left alone.
    @book_test(Booking.STRICT)
    def test_reduce__no_cost(self, _, __):
        """
        2015-10-01 * #ante
          Assets:Account          10 USD

        2015-10-01 * #apply #booked #reduced
          Assets:Account          -5 USD

        2015-10-01 * #ex
          Assets:Account           5 USD
        """

    # Test reductions crossing the sign line.
    @book_test(Booking.STRICT)
    def test_reduce__sign_change_simple(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account         10 HOOL {33.33 USD, 2016-01-01}

        2016-05-08 * #apply
          Assets:Account        -13 HOOL {}

        2016-05-08 * #booked
          error: "Not enough lots to reduce"

        2016-01-01 * #ex
          Assets:Account         10 HOOL {33.33 USD, 2016-01-01}
        """
        # We should not allow this because the implied cost basis has nothing to
        # do with with the original cost basis. It does not make sense to carry
        # over the cost basis into negative units territory. For instance, the
        # negative 3 units that would result from this transaction have no
        # justification to be at 33.33 USD.
        #
        # FIXME: We may want to improve the error message here.

    # Test reductions which trigger matching.
    @book_test(Booking.STRICT)
    def test_reduce__no_match(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account          10 HOOL {123.45 USD, 2016-04-15}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {123.00 USD}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {123.45 CAD}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {123.45 USD, 2016-04-16}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {123.45 USD, "lot1"}

        2016-05-02 * #booked
          error: "No position matches"
        """

    # More reduction scenarios.
    @book_test(Booking.STRICT)
    def test_reduce__unambiguous(self, _, __):
        """
        2016-01-01 * #ante #ambi-matches
          Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {}

        2016-05-02 * #booked #ambi-resolved #reduced
          Assets:Account          -5 HOOL {115.00 USD, 2016-04-15, "lot1"}

        2016-01-01 * #ex
          Assets:Account           5 HOOL {115.00 USD, 2016-04-15, "lot1"}
        """

    @book_test(Booking.STRICT)
    def test_reduce__ambiguous__strict(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}
          Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot2"}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {115.00 USD}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {USD}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {2016-04-15}

        2016-05-02 * #booked
          error: "Ambiguous matches"

        2016-05-02 * #ex
          Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}
          Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot2"}
        """

    @book_test(Booking.NONE)
    def test_reduce__ambiguous__none(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account           1 HOOL {115.00 USD}
          Assets:Account           2 HOOL {116.00 USD}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {117.00 USD}

        2016-05-02 * #booked
          Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}

        2016-05-02 * #reduced
          S Assets:Account        -5 HOOL {117.00 USD, 2016-05-02}

        2016-01-01 * #ex
          Assets:Account           1 HOOL {115.00 USD, 2016-01-01}
          Assets:Account           2 HOOL {116.00 USD, 2016-01-01}
          Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}
        """

    @book_test(Booking.NONE)
    def test_reduce__ambiguous__none__from_mixed(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account           1 HOOL {115.00 USD}
          Assets:Account          -2 HOOL {116.00 USD}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {117.00 USD}

        2016-05-02 * #booked
          Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}

        2016-05-02 * #reduced
          S Assets:Account        -5 HOOL {117.00 USD, 2016-05-02}

        2016-01-01 * #ex
          Assets:Account           1 HOOL {115.00 USD, 2016-01-01}
          Assets:Account          -2 HOOL {116.00 USD, 2016-01-01}
          Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}
        """

    @book_test(Booking.STRICT)
    def test_reduce__other_currency(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account           8 AAPL {115.00 USD, 2016-01-11}
          Assets:Account           8 HOOL {115.00 USD, 2016-01-10}

        2016-01-01 * #ambi-matches
          Assets:Account           8 HOOL {115.00 USD, 2016-01-10}

        2016-01-01 * #ambi-resolved
          Assets:Account          -5 HOOL {115.00 USD, 2016-01-10}

        2016-05-02 * #apply
          Assets:Account          -5 HOOL {115.00 USD}

        2016-05-02 * #booked #reduced
          Assets:Account          -5 HOOL {115.00 USD, 2016-01-10}

        2016-01-01 * #ex
          Assets:Account           8 AAPL {115.00 USD, 2016-01-11}
          Assets:Account           3 HOOL {115.00 USD, 2016-01-10}
        """

    @book_test(Booking.FIFO)
    def test_reduce__multiple_reductions(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
          Assets:Account           50 HOOL {116.00 USD, 2016-01-16}

        2016-05-02 * #apply
          Assets:Account          -40 HOOL {}
          Assets:Account          -35 HOOL {}

        2016-05-02 * #booked
          Assets:Account          -40 HOOL {115.00 USD, 2016-01-15}
          Assets:Account          -10 HOOL {115.00 USD, 2016-01-15}
          Assets:Account          -25 HOOL {116.00 USD, 2016-01-16}

        2016-01-01 * #ex
          Assets:Account           25 HOOL {116.00 USD, 2016-01-16}
        """

    @book_test(Booking.FIFO)
    def test_reduce__multiple_reductions__with_error(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
          Assets:Account           50 HOOL {116.00 USD, 2016-01-16}

        2016-05-02 * #apply
          Assets:Account          -40 HOOL {}
          Assets:Account          -65 HOOL {}

        2016-05-02 * #booked
          error: "Not enough lots to reduce"
        """

    @book_test(Booking.FIFO)
    def test_reduce__reduction_with_same_currency_not_at_cost(self, _, __):
        """
        2016-01-01 * #ante
          Assets:Vanguard:Retire:AfterTax:HOOL   50 HOOL @ 14.33 USD

        2016-05-02 * #apply
          Assets:Vanguard:Retire:AfterTax:HOOL  -40 HOOL {14.33 USD} @ 14.33 USD

        2016-05-02 * #booked
          error: "No position matches"
        """


class TestBookAmbiguous(_BookingTestBase):

    @book_test(Booking.FIFO)
    def test_ambiguous__NONE__matching_existing1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__NONE__matching_existing2(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {101.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          3 HOOL {101.00 USD, 2015-10-01}
        """

    @book_test(Booking.NONE)
    def test_ambiguous__NONE__notmatching_nonmixed1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply #booked
          Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
          Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
        """

    @book_test(Booking.NONE)
    def test_ambiguous__NONE__notmatching_nonmixed1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply #booked
          Assets:Account          2 HOOL {102.00 USD, 2015-06-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
          Assets:Account          2 HOOL {102.00 USD, 2015-06-01}
        """

    @book_test(Booking.NONE)
    def test_ambiguous__NONE__notmatching_mixed1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply #booked
          Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
          Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
        """

    @book_test(Booking.NONE)
    def test_ambiguous__NONE__notmatching_mixed2(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply #booked
          Assets:Account          2 HOOL {102.00 USD, 2015-06-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
          Assets:Account          2 HOOL {102.00 USD, 2015-06-01}
        """

    @book_test(Booking.STRICT)
    def test_ambiguous__STRICT_1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {102.00 USD}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {2015-06-01}

        2015-06-01 * #booked
          error: "No position matches"

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
        """

    @book_test(Booking.STRICT)
    def test_ambiguous__STRICT_2(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -6 HOOL {100.00 USD, 2015-10-01}

        2015-06-01 * #booked
          error: "Not enough lots to reduce"

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
        """

    @book_test(Booking.STRICT)
    def test_ambiguous__STRICT__mixed(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {102.00 USD}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {2015-06-01}

        2015-06-01 * #booked
          error: "No position matches"

        2015-01-01 * #ex
          Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
        """


class TestBookAmbiguousFIFO(_BookingTestBase):

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__no_match_against_any_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account          0 HOOL {}

        2015-02-22 * #reduced
          S Assets:Account          0 HOOL {USD, 2015-02-22}

        2015-02-22 * #booked

        2015-01-01 * #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_match_against_partial_first_lot(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -2 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          2 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_match_against_complete_first_lot(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -4 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_partial_match_against_first_two_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -7 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -3 HOOL {111.11 USD, 2015-10-02}

        2015-01-01 * #ex
          Assets:Account          2 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_complete_match_against_first_two_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -9 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}

        2015-01-01 * #ex
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_partial_match_against_first_three_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -12 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account         -3 HOOL {122.22 USD, 2015-10-03}

        2015-01-01 * #ex
          Assets:Account          3 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_complete_match_against_first_three_lots (self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -15 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}

        2015-01-01 * #ex
        """

    @book_test(Booking.FIFO)
    def test_ambiguous__FIFO__test_matching_more_than_is_available(self, _, __):
        """
        2015-01-01 * #ante #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -16 HOOL {}

        2015-02-22 * #booked
          error: "Not enough lots to reduce"
        """


class TestBookAmbiguousLIFO(_BookingTestBase):

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__no_match_against_any_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account          0 HOOL {}

        2015-02-22 * #reduced
          S Assets:Account          0 HOOL {USD, 2015-02-22}

        2015-02-22 * #booked

        2015-01-01 * #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_match_against_partial_first_lot(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -2 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -2 HOOL {122.22 USD, 2015-10-03}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          4 HOOL {122.22 USD, 2015-10-03}
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_match_against_complete_first_lot(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -6 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}

        2015-01-01 * #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_partial_match_against_first_two_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account         -7 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
          Assets:Account         -1 HOOL {111.11 USD, 2015-10-02}

        2015-01-01 * #ex
          Assets:Account          4 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_complete_match_against_first_two_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -11 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
          Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}

        2015-01-01 * #ex
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_partial_match_against_first_three_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -12 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
          Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account         -1 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_complete_match_against_first_three_lots(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -15 HOOL {}

        2015-02-22 * #booked
          Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
          Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
        """

    @book_test(Booking.LIFO)
    def test_ambiguous__LIFO__test_matching_more_than_is_available(self, _, __):
        """
        2015-01-01 * #ante #ex
          Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
          Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

        2015-02-22 * #apply
          Assets:Account        -16 HOOL {}

        2015-02-22 * #booked
          error: "Not enough lots to reduce"
        """


class _TestBookAmbiguousAVERAGE(_BookingTestBase):

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__trivial1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account        100 HOOL {100.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -2 HOOL {}

        2015-06-01 * #booked
          Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         98 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__trivial2(self, _, __):
        """
        2015-01-01 * #ante #ex
          Assets:Account        100 HOOL {100.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account       -102 HOOL {}

        2015-06-01 * #booked
          error: "Not enough lots to reduce"
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__simple_merge2_match1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         50 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         50 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account        -49 HOOL {}

        2015-06-01 * #booked #reduced
          M Assets:Account      -50 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -50 HOOL {101.00 USD, 2015-10-01}
          M Assets:Account      100 HOOL {100.50 USD, 2015-10-01}
          Assets:Account        -49 HOOL {100.50 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         51 HOOL {100.50 USD, 2015-10-01}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__simple_merge2_match2(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         50 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         50 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account        -51 HOOL {}

        2015-06-01 * #booked #reduced
          M Assets:Account      -50 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -50 HOOL {101.00 USD, 2015-10-01}
          M Assets:Account      100 HOOL {100.50 USD, 2015-10-01}
          Assets:Account        -51 HOOL {100.50 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         49 HOOL {100.50 USD, 2015-10-01}
        """

    # Just another of the same.
    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__simple_merge2_match2_b(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 USD, 2015-10-02}

        2015-06-01 * #apply
          Assets:Account        -20 HOOL {}

        2015-06-01 * #booked
          M Assets:Account      -60 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -40 HOOL {110.00 USD, 2015-10-02}
          M Assets:Account      100 HOOL {104.00 USD, 2015-10-01}
          Assets:Account        -20 HOOL {104.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         80 HOOL {104.00 USD, 2015-10-01}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__simple_merge3_match1(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         50 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         50 HOOL {101.00 USD, 2015-10-01}
          Assets:Account         50 HOOL {102.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account        -49 HOOL {}

        2015-06-01 * #booked #reduced
          M Assets:Account      -50 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -50 HOOL {101.00 USD, 2015-10-01}
          M Assets:Account      -50 HOOL {102.00 USD, 2015-10-01}
          M Assets:Account      150 HOOL {101.00 USD, 2015-10-01}
          Assets:Account        -49 HOOL {101.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account        101 HOOL {101.00 USD, 2015-10-01}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__simple_merge2_insufficient(self, _, __):
        """
        2015-01-01 * #ante #ex
          Assets:Account         50 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         50 HOOL {101.00 USD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account       -101 HOOL {}

        2015-06-01 * #booked #reduced
          error: "Not enough lots to reduce"
        """

    # This is similar to the previous.
    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__simple_merge2_insufficient_b(self, _, __):
        """
        2015-01-01 * #ante #ex
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 USD, 2015-10-02}

        2015-06-01 * #apply
          Assets:Account       -120 HOOL {}

        2015-06-01 * #booked #reduced
          error: "Not enough lots to reduce"
        """



    # Tests with mixed currencies. These should fail if the match is at all
    # ambiguous.

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__ambi(self, _, __):
        """
        2015-01-01 * #ante #ex
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account        -20 HOOL {}

        2015-06-01 * #booked
          error: "Failed to categorize posting"
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__unambi_currency(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account        -30 HOOL {USD}

        2015-06-01 * #booked
          Assets:Account        -30 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         30 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__unambi_currency__merging(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         25 HOOL { 99.00 USD, 2015-10-01}
          Assets:Account         10 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         25 HOOL {101.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account        -30 HOOL {USD}

        2015-06-01 * #booked
          M Assets:Account      -25 HOOL { 99.00 USD, 2015-10-01}
          M Assets:Account      -10 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -25 HOOL {101.00 USD, 2015-10-01}
          M Assets:Account       60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account        -30 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         30 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}
        """

    @unittest.skip("FIXME enable this when supporting explicit cost reductions")
    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__unambi_cost_and_currency__merging(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         25 HOOL { 99.00 USD, 2015-10-01}
          Assets:Account         10 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         25 HOOL {101.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -5 HOOL {100.00 USD}

        2015-06-01 * #booked
          M Assets:Account      -25 HOOL { 99.00 USD, 2015-10-01}
          M Assets:Account      -10 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -25 HOOL {101.00 USD, 2015-10-01}
          M Assets:Account       60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account        -30 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         30 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}
        """

    @unittest.skip("FIXME enable this when supporting explicit cost reductions")
    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__unambi_cost__merging(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         25 HOOL { 99.00 USD, 2015-10-01}
          Assets:Account         10 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         25 HOOL {101.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}

        2015-06-01 * #apply
          Assets:Account         -5 HOOL {100.00}

        2015-06-01 * #booked
          M Assets:Account      -25 HOOL { 99.00 USD, 2015-10-01}
          M Assets:Account      -10 HOOL {100.00 USD, 2015-10-01}
          M Assets:Account      -25 HOOL {101.00 USD, 2015-10-01}
          M Assets:Account       60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account        -30 HOOL {100.00 USD, 2015-10-01}

        2015-01-01 * #ex
          Assets:Account         30 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 CAD, 2015-10-01}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__unambi_date(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         40 HOOL {110.00 USD, 2015-10-02}

        ;; Notice how this matches only a portion of the inventory, even if we're at
        ;; average cost. Handle this accordingly.
        2015-06-01 * #apply
          Assets:Account        -30 HOOL {2015-10-02}

        2015-06-01 * #booked #reduced
          Assets:Account        -30 HOOL {110.00 USD, 2015-10-02}

        ;; Note that here we leave the remaining lots merged.
        2015-01-01 * #ex
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         10 HOOL {110.00 USD, 2015-10-02}
        """

    @book_test(Booking.AVERAGE)
    def test_ambiguous__AVERAGE__mixed_currencies__unambi_with_merge(self, _, __):
        """
        2015-01-01 * #ante
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         10 HOOL {107.00 USD, 2015-10-02}
          Assets:Account         30 HOOL {111.00 USD, 2015-10-02}

        ;; This is like the previous example but which involves some merging.
        2015-06-01 * #apply
          Assets:Account        -30 HOOL {2015-10-02}

        2015-06-01 * #booked #reduced
          M Assets:Account      -10 HOOL {107.00 USD, 2015-10-02}
          M Assets:Account      -30 HOOL {111.00 USD, 2015-10-02}
          M Assets:Account       40 HOOL {110.00 USD, 2015-10-02}
          Assets:Account        -30 HOOL {110.00 USD, 2015-10-02}

        ;; Note that here we also leave the remaining lots merged.
        2015-01-01 * #ex
          Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
          Assets:Account         10 HOOL {110.00 USD, 2015-10-02}
        """













## CONTINUE



# @parser.parse_doc(allow_incomplete=True)
# def test_ambiguous__AVERAGE__merging_with_cost(self, entries, _, __):
#     """
#     2015-01-01 * "Single position"
#       Assets:Account         60 HOOL {100.00 USD, 2015-10-01}
#       Assets:Account         40 HOOL {110.00 USD, 2015-10-02}

#     2015-06-01 * "" #error
#       Assets:Account        -20 HOOL {140.00 USD}
#       M Assets:Account      -60 HOOL {100.00 USD, 2015-10-01}
#       M Assets:Account      -40 HOOL {110.00 USD, 2015-10-02}
#       M Assets:Account       80 HOOL { 95.00 USD, 2015-10-01}
#     """
#     exbal_list = self._reduce_first_expect_rest(entries[0], entries[1:],
#                                                 Booking.AVERAGE)
#     self.assertEqual(
#         {'Assets:Account': I('-20 HOOL {104.00 USD, 2015-10-01}')},
#         exbal_list[0])

# FIXME: You need to handle an explicit cost in an average reduction. This
# is required in order to handle pre-tax 401k accounts with fees that are
# calculated at market price.

# FIXME: You need to deal with the case of using the '*' syntax instead of
# having the matches come from the AVERAGE method. I'm not sure I need it
# anymore actually. If I do, then reductions need to be able to deal with
# mixed inventories.

# FIXME: If an account's method is AVERAGE we probably want to merge the
# augmentations together immediately after an augmentation. Do this now, it
# will result in nicer balances and a more predictable output, at the
# expense of some rounding error (probably negligible, given the precision
# we're supporting).

# FIXME: You need to test what happens when you use the NONE method and
# reduce without providing the cost information on the lot. Add a test for
# this case. This ought to result in an error.



# Note: these are redundant tests from the above; delete these or reuse the work.

# class TestBookingNEW(_BookingTestBase):

#     @book_test(Booking.NONE)
#     def test_ambiguous__NONE__matching_existing(self, _, __):
#         """
#         2015-01-01 * #ante
#           Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

#         2015-06-01 * #apply
#           Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

#         2015-06-01 * #booked
#           Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

#         2015-06-01 * #ex
#           Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

#         2015-06-01 * #reduced
#           S Assets:Account       -2 HOOL {100.00 USD, 2015-10-01}
#         """

#     @book_test(Booking.NONE)
#     def test_ambiguous__NONE__notmatching_mixed(self, _, __):
#         """
#         2015-01-01 * #ante
#           Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

#         2015-06-01 * #apply
#           Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

#         2015-01-01 * #ex
#           Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
#           Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
#         """

#     @book_test(Booking.STRICT)
#     def test_ambiguous__STRICT(self, _, __):
#         """
#         2015-01-01 * #ante
#           Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

#         2015-06-01 * #apply
#           Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

#         2015-06-01 * #booked
#           error: "No position matches"

#         2015-06-01 * #ex
#           Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
#         """

#     @book_test(Booking.FIFO)
#     def test_ambiguous__FIFO(self, _, __):
#         """
#         2015-01-01 * #ante #ambi-matches
#           Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
#           Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

#         2015-02-22 * #apply
#           Assets:Account         -7 HOOL {}

#         2015-02-22 * #booked #ambi-resolved
#           Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
#           Assets:Account         -3 HOOL {111.11 USD, 2015-10-02}

#         2015-01-01 * #ex
#           Assets:Account          2 HOOL {111.11 USD, 2015-10-02}
#           Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
#         """






class TestBasicBooking(_BookingTestBase):

    @book_test(Booking.STRICT)
    def test_augment__at_cost__same_date(self, _, __):
        """
        2015-10-01 * #ante
          Assets:Account          1 HOOL {100.00 USD}

        2015-10-01 * #apply
          Assets:Account          2 HOOL {100.00 USD}

        2015-10-02 * #apply
          Assets:Account          2 HOOL {100.00 USD, 2015-10-01}

        2015-11-01 * #ex
          Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
        """

    @book_test(Booking.STRICT)
    def test_augment__at_cost__different_date(self, _, __):
        """
        2015-10-01 * #ante
          Assets:Account          1 HOOL {100.00 USD}

        2015-10-02 * #apply
          Assets:Account          2 HOOL {100.00 USD}

        2015-10-01 * #apply
          Assets:Account          2 HOOL {100.00 USD, 2015-10-02}

        2015-11-01 * #ex
          Assets:Account          1 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          2 HOOL {100.00 USD, 2015-10-02}
        """

    @book_test(Booking.STRICT)
    def test_augment__at_cost__different_cost(self, _, __):
        """
        2015-10-01 * #ante
          Assets:Account          1 HOOL {100.00 USD}

        2015-10-01 * #apply
          Assets:Account          2 HOOL {101.00 USD}

        2015-10-01 * #booked
          Assets:Account          2 HOOL {101.00 USD, 2015-10-01}

        2015-11-01 * #ex
          Assets:Account          1 HOOL {100.00 USD, 2015-10-01}
          Assets:Account          2 HOOL {101.00 USD, 2015-10-01}
        """







# FIXME: Rewrite these tests.
class TestBook(unittest.TestCase):

    def book_reductions(self, entries, currency='USD'):
        balances = collections.defaultdict(inventory.Inventory)
        booking_methods = collections.defaultdict(lambda: Booking.STRICT)
        for entry in entries:
            (booked_postings,
             booked_errors) = bf.book_reductions(entry,
                                                 entry.postings,
                                                 balances,
                                                 booking_methods)
            (inter_postings,
             inter_errors,
             interpolated) = bf.interpolate_group(booked_postings,
                                                  balances,
                                                  currency)
            for posting in inter_postings:
                balances[posting.account].add_position(posting)

        return inter_postings, balances

    def assertPostingsEqual(self, postings1, postings2):
        postings1 = [posting._replace(meta=None) for posting in postings1]
        postings2 = [posting._replace(meta=None) for posting in postings2]
        self.assertEqual(postings1, postings2)

    @parser.parse_doc(allow_incomplete=True)
    def test_augment__at_cost__different_cost(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          1 HOOL {100.00 USD}
          Assets:Other          -100.00 USD

        2015-10-01 * "Held-at-cost, positive, differnt cost"
          Assets:Account1          2 HOOL {101.00 USD}
          Assets:Other          -204.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('1 HOOL {100.00 USD, 2015-10-01}, '
                           '2 HOOL {101.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('2 HOOL'),
                         Cost(D('101.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('-204.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_augment__at_cost__different_currency(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          1 HOOL {100.00 USD}
          Assets:Other          -100.00 USD

        2015-10-01 * "Held-at-cost, positive, same cost"
          Assets:Account1          2 HOOL {100.00 CAD}
          Assets:Other          -200.00 CAD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('1 HOOL {100.00 USD, 2015-10-01}, '
                           '2 HOOL {100.00 CAD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('2 HOOL'),
                         Cost(D('100.00'), 'CAD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('-200.00 CAD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_augment__at_cost__different_label(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          1 HOOL {100.00 USD}
          Assets:Other          -100.00 USD

        2015-10-01 * "Held-at-cost, positive, same cost"
          Assets:Account1          2 HOOL {100.00 USD, "lot1"}
          Assets:Other          -200.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('1 HOOL {100.00 USD, 2015-10-01}, '
                           '2 HOOL {100.00 USD, 2015-10-01, "lot1"}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('2 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), "lot1"),
                         None, None, None),
            data.Posting('Assets:Other', A('-200.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__no_cost(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          10 USD
          Assets:Other1           -10 USD

        2015-10-01 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 USD
          Assets:Other2            1 USD
        """
        _, balances = self.book_reductions(entries)
        self.assertEqual(I('9 USD'),
                         balances['Assets:Account1'])

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__same_cost(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 HOOL {100.00 USD}
          Assets:Other        100.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('2 HOOL {100.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('-1 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('100.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__any_spec(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 HOOL {}
          Assets:Other        100.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('2 HOOL {100.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('-1 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('100.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__same_cost__per(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 HOOL {100.00}
          Assets:Other        100.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('2 HOOL {100.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('-1 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('100.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__same_cost__total(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -2 HOOL {# 100.00 USD}
          Assets:Other        200.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('1 HOOL {100.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('-2 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('200.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__same_currency(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 HOOL {USD}
          Assets:Other        100.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('2 HOOL {100.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('-1 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('100.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__same_date(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 HOOL {2015-10-01}
          Assets:Other        100.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(I('2 HOOL {100.00 USD, 2015-10-01}'),
                         balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting('Assets:Account1', A('-1 HOOL'),
                         Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), None),
                         None, None, None),
            data.Posting('Assets:Other', A('100.00 USD'), None, None, None, None),
            ], postings)

    @parser.parse_doc(allow_incomplete=True)
    def test_reduce__same_label(self, entries, _, __):
        """
        2015-10-01 * "Held-at-cost, positive"
          Assets:Account1          3 HOOL {100.00 USD, "6e425dd7b820"}
          Assets:Other       -300.00 USD

        2015-10-02 * "Held-at-cost, positive, same cost"
          Assets:Account1         -1 HOOL {"6e425dd7b820"}
          Assets:Other        100.00 USD
        """
        postings, balances = self.book_reductions(entries)
        self.assertEqual(
            I('2 HOOL {100.00 USD, 2015-10-01, "6e425dd7b820"}'),
            balances['Assets:Account1'])
        self.assertPostingsEqual([
            data.Posting(
                'Assets:Account1', A('-1 HOOL'),
                Cost(D('100.00'), 'USD', datetime.date(2015, 10, 1), "6e425dd7b820"),
                None, None, None),
            data.Posting('Assets:Other', A('100.00 USD'), None, None, None, None),
            ], postings)


class TestBooking(unittest.TestCase):
    "Tests the booking & interpolation process."

    maxDiff = 8192

    # def book(self, entry, balances=None, exp_costs=None, debug=False):
    #     if balances is None:
    #         balances = {}
    #     groups, errors = bf.categorize_by_currency(entry, balances)
    #     self.assertFalse(errors)
    #     posting_groups = bf.replace_currencies(entry.postings, groups)
    #     for currency, postings in posting_groups.items():
    #         new_postings, new_balances = bf.book_reductions(postings, balances)
    #         if debug:
    #             for posting in new_postings:
    #                 print(posting)
    #             print(new_balances)

    #         # Check the expected costs.
    #         if exp_costs is not None:
    #             for posting, exp_cost in zip(new_postings, exp_costs):
    #                 self.assertEqual(posting.cost, exp_cost)

    # for balances in {}, {'Assets:Account': I('10 HOOL {99.00 USD}')}:
    #     self.book(entries[0], balances, [
    #         position.CostSpec(D('100.00'), None, 'USD', None, None, False),
    #         None])
    #     self.book(entries[1], balances, [
    #         position.CostSpec(MISSING, None, 'USD', None, None, False),
    #         None])



    # @parser.parse_doc(allow_incomplete=True)
    # def test_augmentation_noop(self, entries, _, options_map):
    #     """
    #     2015-10-01 *
    #       Assets:Account          2 HOOL {100.00 USD}
    #       Assets:Other     -1000.00 USD

    #     2015-10-02 *
    #       Assets:Account          2 HOOL {USD}
    #       Assets:Other     -1000.00 USD
    #     """
    #     # Check that these augmenting legs aren't being touched.
    #     for balances in {}, {'Assets:Account': I('10 HOOL {99.00 USD}')}:
    #         self.book(entries[0], balances, [
    #             position.CostSpec(D('100.00'), None, 'USD', None, None, False),
    #             None])
    #         self.book(entries[1], balances, [
    #             position.CostSpec(MISSING, None, 'USD', None, None, False),
    #             None])

    # @parser.parse_doc(allow_incomplete=True)
    # def test_reduction(self, entries, _, options_map):
    #     """
    #     2015-10-01 *
    #       Assets:Account         -2 HOOL {100.00 USD}
    #       Assets:Other      1000.00 USD
    #     """
    #     balances = {'Assets:Account':
    #                 I('5 HOOL {100.00 USD, 2015-01-01}')}
    #     # FIXME: Bring this back in.
    #     # self.book(entries[0], balances, [
    #     #     position.Cost(D('100.00'), 'USD', datetime.date(2015, 1, 1), None),
    #     #     None], debug=1)


# FIXME: Continue here.
__incomplete__ = True


# class TestFullBooking1(cmptest.TestCase):
#
#     @parser.parse_doc()
#     def __test_categorize_by_currency__ambiguous_cost_no_choice(self, ientries, _, __):
#         """
#         ;; Pick the USD lot, because that's all there is in the inventory
#         2015-01-01 *
#           Assets:Bank:Investing          -1 HOOL {}
#           Equity:Opening-Balances       101 USD
#         """
#         groups, free = bf.categorize_by_currency_by_currency(
#             ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})
#         self.assertEqual({'USD': 2}, dictmap(groups, valfun=len))
#         self.assertFalse(free)
#
#     @parser.parse_doc()
#     def __test_categorize_by_currency__ambiguous_cost_choose_lot(self, ientries, _, __):
#         """
#         ;; This should know to pick the USD leg because that's the only currency
#         2015-01-01 *
#           Assets:Bank:Investing          -1 HOOL {}
#           Equity:Opening-Balances       101 USD
#         """
#         groups, free = bf.categorize_by_currency_by_currency(
#             ientries[0].postings, {'USD': I('1 HOOL {100 USD}, '
#                                             '1 HOOL {100 CAD}')})
#
#     @parser.parse_doc()
#     def __test_categorize_by_currency__ambiguous_cost_choose_ccy(self, ientries, _, __):
#         """
#         ;; Pick the USD lot, because that's all there is in the inventory
#         2015-01-01 *
#           Assets:Bank:Investing          -1 HOOL {}
#           Equity:Opening-Balances       101 USD
#           Equity:Opening-Balances       102 CAD
#         """
#         groups, free = bf.categorize_by_currency_by_currency(
#             ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})
#
#     @parser.parse_doc()
#     def __test_categorize_by_currency__ambiguous_cost_no_choice(self, ientries, _, __):
#         """
#         ;; Pick the USD lot, because that's all there is in the inventory
#         2015-01-01 *
#           Assets:Bank:Investing          -1 HOOL {}
#           Equity:Opening-Balances       100 USD
#         """
#         groups, free = bf.categorize_by_currency_by_currency(
#             ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})
#
#     @parser.parse_doc()
#     def __test_categorize_by_currency__ambiguous_cost_with_bal(self, ientries, _, __):
#         """
#         ;; This should know to pick the USD leg because that's the only that doesn't
#         ;; already balance from the other postings.
#         2015-01-01 *
#           Assets:Bank:Investing          -1 HOOL {}
#           Equity:Opening-Balances       101 USD
#           Equity:Opening-Balances      -102 CAD
#           Assets:Cash                   102 CAD
#         """
#         groups, free = bf.categorize_by_currency_by_currency(
#             ientries[0].postings, {'USD': I('1 HOOL {100 USD}, '
#                                                '1 HOOL {100 CAD}')})
#
#
# class TestFullBooking2(cmptest.TestCase):
#
#     @loader.load_doc()
#     def __test_full_booking(self, entries, _, __):
#         """
#           option "booking_method" "FULL"
#           2013-05-01 open Assets:Bank:Investing
#           2013-05-01 open Equity:Opening-Balances
#
#           2013-05-02 *
#             Assets:Bank:Investing           5 HOOL {501 USD}
#             Equity:Opening-Balances     -2505 USD
#         """
#         self.assertEqual(D('-2505'), entries[-1].postings[-1].units.number)
