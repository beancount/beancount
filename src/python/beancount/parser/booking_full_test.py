__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import unittest
import pprint

from beancount.core.number import D
from beancount.core.inventory import from_string as I
from beancount.utils.misc_utils import dictmap
from beancount.core import inventory
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import booking_full
from beancount.parser import cmptest
from beancount import loader


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
                printer.print_errors(errors)

    # def test_all_interpolation_combinations(self):
    #     template = textwrap.dedent("""
    #       2015-10-02 *
    #         Assets:Account  {}
    #         Assets:Other
    #     """)
    #     for pos_template, args in [
    #             ('{:7} {:3}',
    #              ['100.00', 'USD']),
    #             ('{:7} {:3} @ {:7} {:3}',
    #              ['100.00', 'USD', '1.20', 'CAD']),
    #             ('{:2} {:4} {{{:7} {:3}}}',
    #              ['10', 'HOOL', '100.00', 'USD']),
    #             ('{:2} {:4} {{{:7} # {:7} {:3}}}',
    #              ['10', 'HOOL', '100.00', '9.95', 'USD']),
    #             ('{:2} {:4} {{{:7} # {:7} {:3}}} @ {:7} {:3}',
    #              ['10', 'HOOL', '100.00', '9.95', 'USD', '120.00', 'USD']),
    #     ]:
    #         for string in _gen_missing_combinations(template.format(pos_template), args):
    #             print(string)
    #             entries, errors, _ = parser.parse_string(string)
    #             print(len(entries))
    #             printer.print_errors(errors)
    #             print()
    #             print()
    #             print()


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
            groups, errors = booking_full.categorize_by_currency(entry, {})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0,1}}, indexes(groups))

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
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))

        groups, errors = booking_full.categorize_by_currency(
            entries[1], {'Assets:Account': I('1.00 USD')})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))
        groups, errors = booking_full.categorize_by_currency(
            entries[1], {})
        self.assertTrue(errors)
        self.assertRegexpMatches(errors[0].message, 'Failed to categorize posting')
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
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0,1}}, indexes(groups))

        groups, errors = booking_full.categorize_by_currency(
            entries[1], {'Assets:Account': I('1.00 USD')})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0,1}}, indexes(groups))
        groups, errors = booking_full.categorize_by_currency(
            entries[1], {})
        self.assertTrue(errors)
        self.assertRegexpMatches(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'CAD': {0,1}}, indexes(groups))

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
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0,1}}, indexes(groups))

        groups, errors = booking_full.categorize_by_currency(
            entries[1], {'Assets:Account': I('1.00 USD')})
        self.assertFalse(errors)
        self.assertEqual({'CAD': {0,1}}, indexes(groups))
        groups, errors = booking_full.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegexpMatches(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'CAD': {0,1}}, indexes(groups))

        for i in 2, 3:
            groups, errors = booking_full.categorize_by_currency(entries[i], {})
            self.assertEqual(1, len(errors))
            self.assertRegexpMatches(errors[0].message, 'Failed to categorize posting')
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
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))

        groups, errors = booking_full.categorize_by_currency(
            entries[1], {'Assets:Account': I('1 HOOL {1.00 USD}')})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))
        groups, errors = booking_full.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegexpMatches(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'USD': {0,1}}, indexes(groups))

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
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))

        groups, errors = booking_full.categorize_by_currency(
            entries[1], {'Assets:Account': I('1 HOOL {1.00 USD}')})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))
        groups, errors = booking_full.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegexpMatches(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'USD': {0,1}}, indexes(groups))

        for i in 2, 3:
            groups, errors = booking_full.categorize_by_currency(
                entries[i], {'Assets:Account': I('1 HOOL {1.00 USD}')})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0,1}}, indexes(groups))
            groups, errors = booking_full.categorize_by_currency(
                entries[i], {})
            self.assertEqual(1, len(errors))
            self.assertRegexpMatches(errors[0].message, 'Failed to categorize posting')
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
            groups, errors = booking_full.categorize_by_currency(entries[i], {})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0,1}}, indexes(groups))

        for i in 1, 3, 5:
            groups, errors = booking_full.categorize_by_currency(entries[i], {})
            self.assertEqual(1, len(errors))
            self.assertRegexpMatches(errors[0].message, 'Could not resolve units currency')
            self.assertEqual({'USD': {0,1}}, indexes(groups))

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
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,1}}, indexes(groups))

        groups, errors = booking_full.categorize_by_currency(entries[1], {})
        self.assertTrue(errors)
        self.assertRegexpMatches(errors[0].message, 'Could not resolve units currency')
        self.assertEqual({'USD': {0,1}}, indexes(groups))

        for i in 2, 3:
            groups, errors = booking_full.categorize_by_currency(
                entries[i], {'Assets:Account': I('1 HOOL {1.00 USD}')})
            self.assertFalse(errors)
            self.assertEqual({'USD': {0,1}}, indexes(groups))
            groups, errors = booking_full.categorize_by_currency(
                entries[i], {})
            self.assertTrue(errors)
            self.assertRegexpMatches(errors[0].message, 'Failed to categorize posting')
            self.assertEqual({}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__multiple_auto_postings(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Account   100.00 CAD
          Assets:Other
        """
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertFalse(errors)
        self.assertEqual({'USD': {0,2}, 'CAD': {1,2}}, indexes(groups))

    @parser.parse_doc(allow_incomplete=True)
    def test_categorize__redundant_auto_postings(self, entries, _, options_map):
        """
        ;; Uses the other legs to disambiguate.
        2015-10-02 *
          Assets:Account   10 HOOL {100.00    } @ 120.00
          Assets:Other
          Assets:Other
        """
        groups, errors = booking_full.categorize_by_currency(entries[0], {})
        self.assertTrue(errors)


class TestReplaceCurrenciesInGroup(unittest.TestCase):
    "Tests the replacement of currencies inferred in the categorization step."

    def check(self, expected, entry):
        groups, errors = booking_full.categorize_by_currency(entry, {})
        self.assertFalse(errors)
        posting_groups = booking_full.replace_currencies(entry.postings, groups)
        check_groups = {
            currency: [(posting.position.units.currency,
                        posting.position.cost.currency if posting.position.cost else None,
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
        """
        self.check({'USD': [('USD', None, None),
                            ('USD', None, None)]}, entries[0])
        self.check({'CAD': [('CAD', None, None),
                            ('CAD', None, None)],
                    'USD': [('USD', None, None),
                            ('USD', None, None)]}, entries[1])

    @parser.parse_doc(allow_incomplete=True)
    def test_missing(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   100.00
          Assets:Other    -100.00 USD

        2015-10-02 *
          Assets:Account   100.00 USD @ 120.00
          Assets:Other    -120.00 CAD

        2015-10-02 *
          Assets:Account   10 HOOL {100.00}
          Assets:Other    -1000.00 USD

        2015-10-02 *
          Assets:Account   10 HOOL {100.00} @ 120.00 USD
          Assets:Other    -1000.00 USD
        2015-10-02 *
          Assets:Account   10 HOOL {100.00 USD} @ 120.00
          Assets:Other    -1000.00 USD
        """
        self.check({'USD': [('USD', None, None),
                            ('USD', None, None)]}, entries[0])

        self.check({'CAD': [('USD', None, 'CAD'),
                            ('CAD', None, None)]}, entries[1])

        self.check({'USD': [('HOOL', 'USD', None),
                            ('USD', None, None)]}, entries[2])

        self.check({'USD': [('HOOL', 'USD', 'USD'),
                            ('USD', None, None)]}, entries[3])
        self.check({'USD': [('HOOL', 'USD', 'USD'),
                            ('USD', None, None)]}, entries[4])


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

    def check(self, entry, interpolated, expected=None, num_errors=0, balances=None):
        groups, errors = booking_full.categorize_by_currency(entry, {})
        self.assertFalse(errors)
        posting_groups = booking_full.replace_currencies(entry.postings, groups)
        if balances is None:
            balances = inventory.Inventory()
        for currency, postings in posting_groups.items():
            new_postings, errors, interpolated = booking_full.interpolate_group(postings,
                                                                                balances,
                                                                                currency)

            # Check the expected number of errors.
            self.assertEqual(num_errors, len(errors))

            if 0:
                print()
                for p in new_postings:
                    print(p)
                for e in errors:
                    print(e)
                print()

            # Check the expected postings.
            if expected:
                for currency, string in expected.items():
                    exp_entries, exp_errors, _ = parser.parse_string(string, dedent=True)
                    self.assertFalse(exp_errors, "Internal error in test")
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
        self.check(entries[0], False)

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_impossible_diffmiss_units(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account          USD
          Assets:Other            USD
        """
        self.check(entries[0], False, num_errors=1)

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_impossible_diffmiss_cost_and_units(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   2 GOOG {USD}
          Assets:Other            USD
        """
        self.check(entries[0], False, num_errors=1)

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_impossible_samemiss_posting(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account   GOOG {USD}
          Assets:Other      -100.00 USD
        """
        self.check(entries[0], False, num_errors=1)

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_units(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account          USD
          Assets:Other    -100.00 USD
        """
        self.check(entries[0], False,
                   expected={'USD': """
        2015-10-02 *
          Assets:Account   100.00 USD
          Assets:Other    -100.00 USD
                   """})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_cost_both(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account       10 GOOG {USD}
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], False,
                   expected={'USD': """
        2015-10-02 *
          Assets:Account       10 GOOG {100.995 USD}
          Assets:Other   -1009.95 USD
                   """})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_cost_per(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account       10 GOOG {# 9.95 USD}
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], False,
                   expected={'USD': """
        2015-10-02 *
          Assets:Account       10 GOOG {100.00 # 9.95 USD}
          Assets:Other   -1009.95 USD
                   """})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_cost_total(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account       10 GOOG {100.00 # USD}
          Assets:Other   -1009.95 USD
        """
        self.check(entries[0], False,
                   expected={'USD': """
        2015-10-02 *
          Assets:Account       10 GOOG {100.00 # 9.95 USD}
          Assets:Other   -1009.95 USD
                   """})

    @parser.parse_doc(allow_incomplete=True)
    def test_incomplete_price(self, entries, _, options_map):
        """
        2015-10-02 *
          Assets:Account  120.00 CAD @ USD
          Assets:Other   -100.00 USD
        """
        self.check(entries[0], False,
                   expected={'USD': """
        2015-10-02 *
          Assets:Account  120.00 CAD @ 1.2 USD
          Assets:Other   -100.00 USD
                   """})



# You need to test the missing value for each variant of possible posting.
# You need to test with mulitple groups as well.


# FIXME: When the other amounts balance, this should be doable.
"""
  2010-05-28 *
    Assets:Account1     100.00 CAD
    Assets:Account2     -80.00 CAD
    Assets:Account3     -20.00 CAD
    Assets:Account4     200.00 USD
    Assets:Account5
"""

# FIXME: When the other amounts balance, this should be doable.
# In this example, the first three postings in CAD balance each other.
# the 4th posting is USD and not completely balanced, and the last is
# unknown. We should look at the two groups of CAD and USD postings,
# realize that the first group is already balanced and that the second
# isn't, and automatically select the USD group.
"""
  2010-05-28 *
    Assets:Account1     100.00 CAD
    Assets:Account2     -80.00 CAD
    Assets:Account3     -20.00 CAD
    Assets:Account4      20.00 USD
    Assets:Account4    -100.00 CAD @
"""


# FIXME: If there is only a single other group, we should be able infer the
# price is for CAD here. Currently this raises an error.
"""
  2010-05-28 *
    Assets:Account1     100.00 USD @ 1.2
    Assets:Account2     120.00 CAD
"""


class Whatever:

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_no_choice(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})
        self.assertEqual({'USD': 2}, dictmap(groups, valfun=len))
        self.assertFalse(free)

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_choose_lot(self, ientries, _, options_map):
        """
        ;; This should know to pick the USD leg because that's the only currency
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}, '
                                            '1 HOOL {100 CAD}')})

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_choose_ccy(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
          Equity:Opening-Balances       102 CAD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_no_choice(self, ientries, _, options_map):
        """
        ;; Pick the USD lot, because that's all there is in the inventory
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       100 USD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}')})

    @parser.parse_doc()
    def __test_categorize_by_currency__ambiguous_cost_with_bal(self, ientries, _, options_map):
        """
        ;; This should know to pick the USD leg because that's the only that doesn't already
        ;; balance from the other postings.
        2015-01-01 *
          Assets:Bank:Investing          -1 HOOL {}
          Equity:Opening-Balances       101 USD
          Equity:Opening-Balances      -102 CAD
          Assets:Cash                   102 CAD
        """
        groups, free = booking_full.categorize_by_currency_by_currency(
            ientries[0].postings, {'USD': I('1 HOOL {100 USD}, '
                                               '1 HOOL {100 CAD}')})


    # FIXME: Come up with cases where we're able to infer an AUGMENTING leg

    # FIXME: Come up wiht a case that would be ambiguous if not for the fact
    # that one of the currencies already balances.




class TestFullBooking(cmptest.TestCase):

    @loader.load_doc()
    def __test_full_booking(self, entries, _, options_map):
        """
          option "booking_method" "FULL"
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing           5 HOOL {501 USD}
            Equity:Opening-Balances     -2505 USD
        """
        self.assertEqual(D('-2505'), entries[-1].postings[-1].position.units.number)
