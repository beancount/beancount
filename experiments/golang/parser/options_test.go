package parser

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
)

func TestProcessOptions(t *testing.T) {
	rawOptions := map[string][]string{
		"title":              {"My Ledger"},
		"operating_currency": {"USD", "CAD"},
		"name_assets":        {"Finances:Assets"},
		"booking_method":     {"FIFO"},
		"render_commas":      {"TRUE"},
		"display_precision":  {"USD:0.01", "CAD:0.005"},
	}

	opts, errs := ProcessOptions(rawOptions)
	if len(errs) > 0 {
		t.Fatalf("ProcessOptions failed with errors: %v", errs)
	}

	if opts.Title != "My Ledger" {
		t.Errorf("Expected Title='My Ledger', got %v", opts.Title)
	}

	if len(opts.OperatingCurrency) != 2 || opts.OperatingCurrency[0] != "USD" || opts.OperatingCurrency[1] != "CAD" {
		t.Errorf("Expected OperatingCurrency=[USD, CAD], got %v", opts.OperatingCurrency)
	}

	if opts.AccountTypes.Assets != "Finances:Assets" {
		t.Errorf("Expected AccountTypes.Assets='Finances:Assets', got %v", opts.AccountTypes.Assets)
	}

	if opts.BookingMethod != core.BookingFIFO {
		t.Errorf("Expected BookingMethod=FIFO, got %v", opts.BookingMethod)
	}

	if !opts.RenderCommas {
		t.Errorf("Expected RenderCommas=true, got false")
	}

	if opts.DisplayPrecision["USD"] != "0.01" || opts.DisplayPrecision["CAD"] != "0.005" {
		t.Errorf("Expected DisplayPrecision values, got %v", opts.DisplayPrecision)
	}
}

func TestProcessOptionsDefaults(t *testing.T) {
	opts, errs := ProcessOptions(make(map[string][]string))
	if len(errs) > 0 {
		t.Fatalf("ProcessOptions failed: %v", errs)
	}

	if opts.Title != "Beancount" {
		t.Errorf("Expected default Title='Beancount', got %v", opts.Title)
	}

	if opts.AccountTypes.Assets != "Assets" {
		t.Errorf("Expected default Assets='Assets', got %v", opts.AccountTypes.Assets)
	}

	if opts.BookingMethod != core.BookingStrict {
		t.Errorf("Expected default BookingMethod=STRICT, got %v", opts.BookingMethod)
	}
}
