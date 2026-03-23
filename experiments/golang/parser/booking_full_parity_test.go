package parser

import (
	"fmt"
	"strings"
	"testing"

	"github.com/beancount/beancount/v3/core"
)

func getIndexes(groups []GroupEntry) map[string]map[int]bool {
	res := make(map[string]map[int]bool)
	for _, g := range groups {
		indices := make(map[int]bool)
		for _, r := range g.Refers {
			indices[r.Index] = true
		}
		res[g.Currency] = indices
	}
	return res
}

func assertEqualIndexes(t *testing.T, expected map[string][]int, actual map[string]map[int]bool) {
	t.Helper()
	if len(expected) != len(actual) {
		t.Errorf("Expected %d groups, got %d", len(expected), len(actual))
	}
	for c, expIndices := range expected {
		actIndices, ok := actual[c]
		if !ok {
			t.Errorf("Expected currency %s not found in actual", c)
			continue
		}
		if len(expIndices) != len(actIndices) {
			t.Errorf("Currency %s: expected %d indices, got %d", c, len(expIndices), len(actIndices))
		}
		for _, idx := range expIndices {
			if !actIndices[idx] {
				t.Errorf("Currency %s: expected index %d not found", c, idx)
			}
		}
	}
}

func parseInventory(s string) *core.Inventory {
	inv, _ := core.InventoryFromString(s)
	return inv
}

func runBookingTest(t *testing.T, input string, method core.Booking) {
	t.Helper()
	directives, errs, rawOptions := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}

	opts, _ := ProcessOptions(rawOptions)
	opts.BookingMethod = method

	var anteEntries []core.Directive
	var applyEntries []*core.Transaction
	var expectedBooked []*core.Transaction
	var expectedEx []*core.Transaction

	for _, d := range directives {
		if txn, ok := d.(*core.Transaction); ok {
			if _, ok := txn.Tags["ante"]; ok {
				anteEntries = append(anteEntries, d)
			} else if _, ok := txn.Tags["apply"]; ok {
				applyEntries = append(applyEntries, txn)
			} else if _, ok := txn.Tags["booked"]; ok {
				expectedBooked = append(expectedBooked, txn)
			} else if _, ok := txn.Tags["ex"]; ok {
				expectedEx = append(expectedEx, txn)
			}
		} else {
			anteEntries = append(anteEntries, d)
		}
	}

	for i, applyTxn := range applyEntries {
		inputEntries := append([]core.Directive(nil), anteEntries...)
		inputEntries = append(inputEntries, applyTxn)

		booked, errs := Book(inputEntries, opts)
		
		expectedError := ""
		if err, ok := applyTxn.Meta["error"].(string); ok {
			expectedError = err
		} else if len(expectedBooked) > 0 {
			// In many tests, there is only one #booked block even if multiple #apply
			// The #booked block applies to ALL #apply independence.
			idx := i
			if idx >= len(expectedBooked) {
				idx = 0
			}
			if err, ok := expectedBooked[idx].Meta["error"].(string); ok {
				expectedError = err
			}
		}

		if expectedError != "" {
			found := false
			for _, e := range errs {
				if strings.Contains(strings.ToLower(e.Error()), strings.ToLower(expectedError)) {
					found = true
					break
				}
			}
			if !found {
				t.Errorf("ApplyTxn %d: Expected error matching %q, got: %v", i, expectedError, errs)
			}
			continue
		}

		if len(errs) > 0 {
			t.Errorf("ApplyTxn %d: Booking failed unexpectedly: %v", i, errs)
			continue
		}

		// Verify booked postings
		if len(expectedBooked) > 0 {
			idx := i
			if idx >= len(expectedBooked) {
				idx = 0
			}
			expTxn := expectedBooked[idx]
			
			var actualTxn *core.Transaction
			applyLine := applyTxn.Meta["lineno"]
			for _, b := range booked {
				if bt, ok := b.(*core.Transaction); ok {
					if bt.Meta["lineno"] == applyLine {
						actualTxn = bt
						break
					}
				}
			}

			if actualTxn == nil {
				t.Errorf("ApplyTxn %d: Could not find booked transaction", i)
				continue
			}

			fmt.Printf("DEBUG: ApplyTxn %d actualTxn postings: %v\n", i, actualTxn.Postings)
			if len(actualTxn.Postings) != len(expTxn.Postings) {
				t.Errorf("ApplyTxn %d: Wrong number of booked postings: expected %d, got %d", i, len(expTxn.Postings), len(actualTxn.Postings))
				continue
			}

			for j, p := range actualTxn.Postings {
				expP := ConvertCostSpecToCost(expTxn.Postings[j])
				if p.Account != expP.Account {
					t.Errorf("ApplyTxn %d, Posting %d account mismatch: expected %q, got %q", i, j, expP.Account, p.Account)
				}
				if !p.Units.Equal(expP.Units) {
					t.Errorf("ApplyTxn %d, Posting %d units mismatch: expected %v, got %v", i, j, expP.Units, p.Units)
				}
				
				// Handle 'S' flag: expected CostSpec instead of Cost
				if expP.Flag == "S" {
					if p.CostSpec == nil {
						t.Errorf("ApplyTxn %d, Posting %d: expected CostSpec (S flag), got nil or Cost", i, j)
					}
				} else {
					if (p.Cost == nil) != (expP.Cost == nil) {
						t.Errorf("ApplyTxn %d, Posting %d cost presence mismatch: actual.Cost=%v, exp.Cost=%v", i, j, p.Cost, expP.Cost)
					} else if p.Cost != nil {
						if p.Cost.Number.Cmp(expP.Cost.Number) != 0 || p.Cost.Currency != expP.Cost.Currency {
							t.Errorf("ApplyTxn %d, Posting %d cost mismatch: expected %v, got %v", i, j, expP.Cost, p.Cost)
						}
					}
				}
			}
		}
	}
}

func TestCategorize__units__unambiguous(t *testing.T) {
	input := `
2015-10-02 *
  Assets:Account  100.00 USD
  Assets:Other   -100.00 USD

2015-10-02 *
  Assets:Account         USD
  Assets:Other   -100.00 USD
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	
	for _, d := range directives {
		txn := d.(*core.Transaction)
		groups, errs := CategorizeByCurrency(txn, nil)
		if len(errs) > 0 {
			t.Errorf("Categorize failed: %v", errs)
		}
		assertEqualIndexes(t, map[string][]int{"USD": {0, 1}}, getIndexes(groups))
	}
}

func TestCategorize__units__ambiguous(t *testing.T) {
	input := `
;; Uses the other legs to disambiguate.
2015-10-02 *
  Assets:Account  100.00
  Assets:Other   -100.00 USD

;; Uses the inventory contents to disambiguate.
2015-10-02 *
  Assets:Account  100.00
  Assets:Other
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	
	// Case 1
	txn0 := directives[0].(*core.Transaction)
	groups, errs := CategorizeByCurrency(txn0, nil)
	if len(errs) > 0 {
		t.Errorf("Case 1: Categorize failed: %v", errs)
	}
	assertEqualIndexes(t, map[string][]int{"USD": {0, 1}}, getIndexes(groups))
	
	// Case 2 with inventory
	txn1 := directives[1].(*core.Transaction)
	balances := map[string]*core.Inventory{
		"Assets:Account": parseInventory("1.00 USD"),
	}
	groups, errs = CategorizeByCurrency(txn1, balances)
	if len(errs) > 0 {
		t.Errorf("Case 2 with inv: Categorize failed: %v", errs)
	}
	assertEqualIndexes(t, map[string][]int{"USD": {0, 1}}, getIndexes(groups))
}

func TestCategorize__units_price__unambiguous(t *testing.T) {
	input := `
2015-10-02 *
  Assets:Account  100.00 USD @ 1.20 CAD
  Assets:Other   -120.00 CAD

2015-10-02 *
  Assets:Account  100.00     @ 1.20 CAD
  Assets:Other   -120.00 CAD
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	
	// Case 1
	txn0 := directives[0].(*core.Transaction)
	groups, errs := CategorizeByCurrency(txn0, nil)
	if len(errs) > 0 {
		t.Errorf("Case 1: Categorize failed: %v", errs)
	}
	assertEqualIndexes(t, map[string][]int{"CAD": {0, 1}}, getIndexes(groups))
	
	// Case 2 with inventory
	txn1 := directives[1].(*core.Transaction)
	balances := map[string]*core.Inventory{
		"Assets:Account": parseInventory("1.00 USD"),
	}
	groups, errs = CategorizeByCurrency(txn1, balances)
	if len(errs) > 0 {
		t.Errorf("Case 2 with inv: Categorize failed: %v", errs)
	}
	assertEqualIndexes(t, map[string][]int{"CAD": {0, 1}}, getIndexes(groups))
}

func TestCategorize__multiple_auto_postings(t *testing.T) {
	input := `
2015-10-02 *
  Assets:Account   100.00 USD
  Assets:Account   100.00 CAD
  Assets:Other
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	
	txn := directives[0].(*core.Transaction)
	groups, errs := CategorizeByCurrency(txn, nil)
	if len(errs) > 0 {
		t.Errorf("Categorize failed: %v", errs)
	}
	assertEqualIndexes(t, map[string][]int{"USD": {0, 2}, "CAD": {1, 2}}, getIndexes(groups))
}
