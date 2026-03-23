package parser

import (
	"testing"
	"github.com/beancount/beancount/v3/core"
)


func TestTestBookAugmentations_test_augment__from_empty__no_cost__pos(t *testing.T) {
	input := `
2015-10-01 * #apply
  Assets:Account           1 USD

2015-10-01 * #ex #booked #reduced
  Assets:Account           1 USD
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAugmentations_test_augment__from_empty__no_cost__neg(t *testing.T) {
	input := `
2015-10-01 * #apply
  Assets:Account          -1 USD

2015-10-01 * #ex #booked #reduced
  Assets:Account           -1 USD
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAugmentations_test_augment__from_empty__at_cost__pos(t *testing.T) {
	input := `
2015-10-01 * #apply
  Assets:Account          1 HOOL {100.00 USD}

2015-10-01 * #ex #booked
  Assets:Account          1 HOOL {100.00 USD, 2015-10-01}

2015-10-01 * #reduced
  S Assets:Account        1 HOOL {100.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAugmentations_test_augment__from_empty__at_cost__neg(t *testing.T) {
	input := `
2015-10-01 * #apply
  Assets:Account          -1 HOOL {100.00 USD}

2015-10-01 * #ex #booked
  Assets:Account          -1 HOOL {100.00 USD, 2015-10-01}

2015-10-01 * #reduced
  S Assets:Account        -1 HOOL {100.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAugmentations_test_augment__from_empty__incomplete_cost__empty(t *testing.T) {
	input := `
2015-10-01 * #apply
  Assets:Account          1 HOOL {}

2015-10-01 * #booked
  error: "Failed to categorize posting"
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAugmentations_test_augment__from_empty__incomplete_cost__with_currency(t *testing.T) {
	input := `
2015-10-01 * #apply
  Assets:Account          1 HOOL {USD}

2015-10-01 * #booked
  Assets:Account          1 HOOL {0 USD, 2015-10-01}

2015-10-01 * #reduced
  S Assets:Account          1 HOOL {USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__no_cost(t *testing.T) {
	input := `
2015-10-01 * #ante
  Assets:Account          10 USD

2015-10-01 * #apply #booked #reduced
  Assets:Account          -5 USD

2015-10-01 * #ex
  Assets:Account           5 USD
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__sign_change_simple(t *testing.T) {
	input := `
2016-01-01 * #ante
  Assets:Account         10 HOOL {33.33 USD, 2016-01-01}

2016-05-08 * #apply
  Assets:Account        -13 HOOL {}

2016-05-08 * #booked
  error: "Not enough lots to reduce"

2016-01-01 * #ex
  Assets:Account         10 HOOL {33.33 USD, 2016-01-01}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__no_match(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__unambiguous(t *testing.T) {
	input := `
2016-01-01 * #ante #ambi-matches
  Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {}

2016-05-02 * #booked #ambi-resolved #reduced
  Assets:Account          -5 HOOL {115.00 USD, 2016-04-15, "lot1"}

2016-01-01 * #ex
  Assets:Account           5 HOOL {115.00 USD, 2016-04-15, "lot1"}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__ambiguous__strict(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__ambiguous__none(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingNone)
}


func TestTestBookReductions_test_reduce__ambiguous__none__from_mixed(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingNone)
}


func TestTestBookReductions_test_reduce__other_currency(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__multiple_reductions(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookReductions_test_reduce__multiple_reductions_hifo(t *testing.T) {
	input := `
2016-01-01 * #ante
  Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           50 HOOL {116.00 USD, 2016-01-16}
  Assets:Account           50 HOOL {114.00 USD, 2016-01-17}

2016-05-02 * #apply
  Assets:Account          -40 HOOL {}
  Assets:Account          -35 HOOL {}
  Assets:Account          -30 HOOL {}

2016-05-02 * #booked
  Assets:Account          -40 HOOL {116.00 USD, 2016-01-16}
  Assets:Account          -10 HOOL {116.00 USD, 2016-01-16}
  Assets:Account          -25 HOOL {115.00 USD, 2016-01-15}
  Assets:Account          -25 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           -5 HOOL {114.00 USD, 2016-01-17}

2016-01-01 * #ex
  Assets:Account           45 HOOL {114.00 USD, 2016-01-17}
`
	runBookingTest(t, input, core.BookingHIFO)
}


func TestTestBookReductions_test_reduce__multiple_reductions__competing__with_error(t *testing.T) {
	input := `
2016-01-01 * #ante
  Assets:Account            5 HOOL {115.00 USD, 2016-01-15}

2016-05-02 * #apply
  Assets:Account           -4 HOOL {115.00 USD}
  Assets:Account           -4 HOOL {2016-01-15}

2016-05-02 * #booked
  error: "Not enough lots to reduce"
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__multiple_reductions__overflowing__with_error(t *testing.T) {
	input := `
2016-01-01 * #ante
  Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           50 HOOL {116.00 USD, 2016-01-16}

2016-05-02 * #apply
  Assets:Account          -40 HOOL {}
  Assets:Account          -65 HOOL {}

2016-05-02 * #booked
  error: "Not enough lots to reduce"
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookReductions_test_reduce__multiple_reductions__no_error_because_total(t *testing.T) {
	input := `
2016-01-01 * #ante
  Assets:Account            7 HOOL {115.00 USD, 2016-01-15}
  Assets:Account            4 HOOL {115.00 USD, 2016-01-16}
  Assets:Account            3 HOOL {117.00 USD, 2016-01-15}

2016-05-02 * #apply
  Assets:Account          -11 HOOL {115.00 USD}

2016-01-01 * #ambi-matches
  Assets:Account            7 HOOL {115.00 USD, 2016-01-15}
  Assets:Account            4 HOOL {115.00 USD, 2016-01-16}

2016-01-01 * #ambi-resolved #booked
  Assets:Account           -7 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           -4 HOOL {115.00 USD, 2016-01-16}

`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductions_test_reduce__reduction_with_same_currency_not_at_cost(t *testing.T) {
	input := `
2016-01-01 * #ante
  Assets:Account   50 HOOL @ 14.33 USD

2016-05-02 * #apply
  Assets:Account  -40 HOOL {14.33 USD} @ 14.33 USD

2016-05-02 * #booked
  error: "No position matches"
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookReductions_test_reduce__missing_units_number(t *testing.T) {
	input := `
2016-01-01 * #ante

2016-05-02 * #apply
  Assets:Account              HOOL {115.00 USD}

2016-01-01 * #booked
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductionsSelf_test_reduce__augment_and_reduce_with_empty_balance(t *testing.T) {
	input := `
2016-01-01 * #ante

2016-05-02 * #apply
  Assets:Account            2 HOOL {115.00 USD}
  Assets:Account           -2 HOOL {116.00 USD}

2016-05-02 * #booked
  Assets:Account            2 HOOL {115.00 USD, 2016-05-02}
  Assets:Account           -2 HOOL {116.00 USD, 2016-05-02}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductionsSelf_test_reduce__augment_and_reduce_with_empty_balance__matching_pos(t *testing.T) {
	t.Skip("Python test is empty")
	input := `
2016-01-01 * #ante

2016-05-02 * #apply
  Assets:Account            2 HOOL {115.00 USD}
  Assets:Account           -2 HOOL {}

2016-01-01 * #ambi-matches
  Assets:Account            2 HOOL {115.00 USD}

2016-01-01 * #ambi-resolved #booked
  Assets:Account            2 HOOL {115.00 USD}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductionsSelf_test_reduce__augment_and_reduce_with_empty_balance__matching_neg(t *testing.T) {
	t.Skip("Python test is skipped")
	input := `
2016-01-01 * #ante

2016-05-02 * #apply
  Assets:Account           -2 HOOL {115.00 USD}
  Assets:Account            2 HOOL {}

2016-01-01 * #ambi-matches
  Assets:Account           -2 HOOL {115.00 USD}

2016-01-01 * #ambi-resolved #booked
  Assets:Account           -2 HOOL {115.00 USD}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookReductionsSelf_test_reduce__augment_and_reduce_with_non_empty_balance(t *testing.T) {
	t.Skip("Python test is empty")
	input := `
2016-02-01 * #ante
  Assets:Account            1 HOOL {5 USD}

;; Acquiring an asset and selling it in the same transaction.
2016-03-01 * #apply
  Assets:Account            2 HOOL {6 USD}
  Assets:Account           -2 HOOL {6 USD} @ 7 USD

2016-05-02 * #ambi-matches
  Assets:Account            2 HOOL {6 USD}

2016-05-02 * #ambi-resolved #booked
  Assets:Account            2 HOOL {6 USD}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAmbiguous_test_ambiguous__NONE__matching_existing1(t *testing.T) {
	input := `
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguous_test_ambiguous__NONE__matching_existing2(t *testing.T) {
	input := `
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {101.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          3 HOOL {101.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguous_test_ambiguous__NONE__notmatching_nonmixed1(t *testing.T) {
	input := `
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
`
	runBookingTest(t, input, core.BookingNone)
}


func TestTestBookAmbiguous_test_ambiguous__NONE__notmatching_nonmixed2(t *testing.T) {
	input := `
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account          2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {102.00 USD, 2015-06-01}
`
	runBookingTest(t, input, core.BookingNone)
}


func TestTestBookAmbiguous_test_ambiguous__NONE__notmatching_mixed1(t *testing.T) {
	input := `
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
`
	runBookingTest(t, input, core.BookingNone)
}


func TestTestBookAmbiguous_test_ambiguous__NONE__notmatching_mixed2(t *testing.T) {
	input := `
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account          2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {102.00 USD, 2015-06-01}
`
	runBookingTest(t, input, core.BookingNone)
}


func TestTestBookAmbiguous_test_ambiguous__STRICT_1(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAmbiguous_test_ambiguous__STRICT_2(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAmbiguous_test_ambiguous__STRICT__mixed(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__no_match_against_any_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_match_against_partial_first_lot(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_match_against_complete_first_lot(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_partial_match_against_first_two_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_complete_match_against_first_two_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_partial_match_against_first_three_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_complete_match_against_first_three_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousFIFO_test_ambiguous__FIFO__test_matching_more_than_is_available(t *testing.T) {
	input := `
2015-01-01 * #ante #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -16 HOOL {}

2015-02-22 * #booked
  error: "Not enough lots to reduce"
`
	runBookingTest(t, input, core.BookingFIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__no_match_against_any_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_match_against_partial_first_lot(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_match_against_complete_first_lot(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_partial_match_against_first_two_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_complete_match_against_first_two_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_partial_match_against_first_three_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_complete_match_against_first_three_lots(t *testing.T) {
	input := `
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
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBookAmbiguousLIFO_test_ambiguous__LIFO__test_matching_more_than_is_available(t *testing.T) {
	input := `
2015-01-01 * #ante #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -16 HOOL {}

2015-02-22 * #booked
  error: "Not enough lots to reduce"
`
	runBookingTest(t, input, core.BookingLIFO)
}


func TestTestBasicBooking_test_augment__at_cost__same_date(t *testing.T) {
	input := `
2015-10-01 * #ante
  Assets:Account          1 HOOL {100.00 USD}

2015-10-01 * #apply
  Assets:Account          2 HOOL {100.00 USD}

2015-10-02 * #apply
  Assets:Account          2 HOOL {100.00 USD, 2015-10-01}

2015-11-01 * #ex
  Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBasicBooking_test_augment__at_cost__different_date(t *testing.T) {
	input := `
2015-10-01 * #ante
  Assets:Account          1 HOOL {100.00 USD}

2015-10-02 * #apply
  Assets:Account          2 HOOL {100.00 USD}

2015-10-01 * #apply
  Assets:Account          2 HOOL {100.00 USD, 2015-10-02}

2015-11-01 * #ex
  Assets:Account          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {100.00 USD, 2015-10-02}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestBasicBooking_test_augment__at_cost__different_cost(t *testing.T) {
	input := `
2015-10-01 * #ante
  Assets:Account          1 HOOL {100.00 USD}

2015-10-01 * #apply
  Assets:Account          2 HOOL {101.00 USD}

2015-10-01 * #booked
  Assets:Account          2 HOOL {101.00 USD, 2015-10-01}

2015-11-01 * #ex
  Assets:Account          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {101.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingStrict)
}


func TestTestStrictWithSize_test_strict_with_size_single(t *testing.T) {
	input := `
2015-10-01 * #ante
  Assets:Account          1 HOOL {101.00 USD}
  Assets:Account          2 HOOL {102.00 USD}

2015-10-02 * #apply
  Assets:Account         -1 HOOL {}

2015-10-02 * #booked
  Assets:Account         -1 HOOL {101.00 USD, 2015-10-01}

2015-11-04 * #ex
  Assets:Account          2 HOOL {102.00 USD, 2015-10-01}
`
	runBookingTest(t, input, core.BookingStrictWithSize)
}


func TestTestStrictWithSize_test_strict_with_size_multiple(t *testing.T) {
	input := `
2015-10-01 * #ante
  Assets:Account          2 HOOL {101.00 USD, 2014-06-02}
  Assets:Account          2 HOOL {102.00 USD, 2014-06-01}

2015-10-02 * #apply
  Assets:Account         -2 HOOL {}

2015-10-02 * #booked
  Assets:Account         -2 HOOL {102.00 USD, 2014-06-01}

2015-11-04 * #ex
  Assets:Account          2 HOOL {101.00 USD, 2014-06-02}
`
	runBookingTest(t, input, core.BookingStrictWithSize)
}
