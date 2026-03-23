package parser

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
)

func TestInterpolateBasic(t *testing.T) {
	input := `
2020-01-01 * "Test"
  Expenses:Food  100.00 USD
  Assets:Bank
`
	directives, errs, rawOptions := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}

	options, _ := ProcessOptions(rawOptions)
	booked, errs := Book(directives, options)
	if len(errs) > 0 {
		t.Fatalf("Booking failed: %v", errs)
	}

	txn := booked[0].(*core.Transaction)
	p1 := txn.Postings[1]
	if p1.Units.Number.Cmp(core.D("-100.00")) != 0 {
		t.Errorf("Interpolation failed: got %v, expected -100.00 USD", p1.Units)
	}
}

func TestInterpolateCost(t *testing.T) {
	input := `
2020-01-01 * "Buy"
  Assets:Investments  10 AAPL {150.00 USD}
  Assets:Bank
`
	directives, errs, rawOptions := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}

	options, _ := ProcessOptions(rawOptions)
	booked, errs := Book(directives, options)
	if len(errs) > 0 {
		t.Fatalf("Booking failed: %v", errs)
	}

	txn := booked[0].(*core.Transaction)
	p1 := txn.Postings[1]
	if p1.Units.Number.Cmp(core.D("-1500.00")) != 0 {
		t.Errorf("Interpolation at cost failed: got %v, expected -1500.00 USD", p1.Units)
	}
}
