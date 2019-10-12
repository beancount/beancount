"""This module contains some old unit tests created for the FULL booking algorithm.
In particular, it should contain some tests relevant for the AVERAGE booking method.

Just saved here until I have time to integrate them into the booking_full_test.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"


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


if __name__ == '__main__':
    main()
