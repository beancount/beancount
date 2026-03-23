package ops

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

func TestValidate(t *testing.T) {
	// 1. Balanced transaction should have no errors
	inputOk := `
2020-01-01 open Assets:Checking
2020-01-01 open Expenses:Food

2020-01-02 * "Balanced"
  Assets:Checking  -100 USD
  Expenses:Food     100 USD
`
	directives, errs, rawOptions := parser.ParseString(inputOk)
	if len(errs) > 0 {
		t.Fatalf("Parse errors: %v", errs)
	}
	options, _ := parser.ProcessOptions(rawOptions)
	booked, errs := parser.Book(directives, options)
	if len(errs) > 0 {
		t.Fatalf("Booking errors: %v", errs)
	}

	valErrors := Validate(booked, options)
	if len(valErrors) > 0 {
		t.Errorf("Unexpected validation errors: %v", valErrors)
	}

	// 2. Transaction with non-open account
	inputNoOpen := `
2020-01-01 * "No Open"
  Assets:Checking  -100 USD
  Expenses:Food     100 USD
`
	directives, _, rawOptions = parser.ParseString(inputNoOpen)
	options, _ = parser.ProcessOptions(rawOptions)
	booked, _ = parser.Book(directives, options)
	valErrors = Validate(booked, options)
	found := false
	for _, e := range valErrors {
		if ve, ok := e.(ValidationError); ok && ve.Message == "Account Assets:Checking is not open" {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("Expected 'not open' validation error")
	}
}

func TestPadAndBalance(t *testing.T) {
	input := `
2020-01-01 open Assets:Checking
2020-01-01 open Equity:OpeningBalances

2020-01-02 pad Assets:Checking Equity:OpeningBalances
2020-01-05 balance Assets:Checking 1000 USD
`
	directives, errs, rawOptions := parser.ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse errors: %v", errs)
	}
	options, _ := parser.ProcessOptions(rawOptions)
	booked, _ := parser.Book(directives, options)

	// Before padding, balance check should fail
	balErrors := CheckBalances(booked, options)
	if len(balErrors) == 0 {
		t.Errorf("Expected balance failure before padding")
	}

	// Apply padding
	padded, _ := Pad(booked, options)
	
	// After padding, balance check should pass
	balErrors = CheckBalances(padded, options)
	if len(balErrors) > 0 {
		t.Errorf("Unexpected balance failure after padding: %v", balErrors)
	}

	// Verify a transaction was inserted
	found := false
	for _, d := range padded {
		if txn, ok := d.(*core.Transaction); ok && txn.Flag == "P" {
			found = true
			if txn.Postings[0].Units.Number.Cmp(core.D("1000")) != 0 {
				t.Errorf("Wrong padding amount: %v", txn.Postings[0].Units)
			}
		}
	}
	if !found {
		t.Errorf("Padding transaction not found")
	}
}

func TestLifetimes(t *testing.T) {
	input := `
2020-01-01 open Assets:Checking
2020-01-10 * "Spend"
  Assets:Checking  -10 USD
  Expenses:Food     10 USD
`
	directives, _, _ := parser.ParseString(input)
	lifetimes := GetAccountLifetimes(directives)

	lt, ok := lifetimes["Assets:Checking"]
	if !ok {
		t.Fatalf("Account not found in lifetimes")
	}
	if lt.First.Format("2006-01-02") != "2020-01-01" || lt.Last.Format("2006-01-02") != "2020-01-10" {
		t.Errorf("Wrong lifetime for Assets:Checking: %v to %v", lt.First, lt.Last)
	}
}
