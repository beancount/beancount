package core_test

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

const TEST_INPUT = `
2012-02-01 open Assets:US:Cash
2012-02-01 open Assets:US:Credit-Card
2012-02-01 open Expenses:Grocery
2012-02-01 open Expenses:Coffee
2012-02-01 open Expenses:Restaurant

2012-05-18 * "Buying food" #dinner
  Expenses:Restaurant         100 USD
  Expenses:Grocery            200 USD
  Assets:US:Cash

2013-06-20 * "Whole Foods Market" "Buying books" #books #dinner ^ee89ada94a39
  Expenses:Restaurant         150 USD
  Assets:US:Credit-Card

2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash

2014-02-01 close Assets:US:Cash
2014-02-01 close Assets:US:Credit-Card
`

func TestHashEntries(t *testing.T) {
	previousHashes := make(map[string]core.Directive)
	for i := 0; i < 64; i++ {
		entries, errs, _ := parser.ParseString(TEST_INPUT)
		if len(errs) > 0 {
			t.Fatalf("Parse errors: %v", errs)
		}
		hashes, errs := core.HashEntries(entries, true)
		if len(errs) > 0 {
			t.Fatalf("Hash errors: %v", errs)
		}
		if i == 0 {
			previousHashes = hashes
		} else {
			if len(previousHashes) != len(hashes) {
				t.Fatalf("Hash counts differ: %d != %d", len(previousHashes), len(hashes))
			}
			for k := range hashes {
				if _, ok := previousHashes[k]; !ok {
					t.Fatalf("Missing hash key: %s", k)
				}
			}
		}
	}
}

func TestHashEntriesWithDuplicates(t *testing.T) {
	input1 := `
          2014-08-01 price HOOL  603.10 USD
        `
	entries1, _, _ := parser.ParseString(input1)
	hashes1, _ := core.HashEntries(entries1, true)
	if len(hashes1) != 1 {
		t.Errorf("Expected 1 hash, got %d", len(hashes1))
	}

	input2 := `
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
          2014-08-01 price HOOL  603.10 USD
        `
	entries2, _, _ := parser.ParseString(input2)
	hashes2, _ := core.HashEntries(entries2, true)
	if len(hashes2) != 1 {
		t.Errorf("Expected 1 hash for duplicates, got %d", len(hashes2))
	}
}

func TestHashEntriesSamePostings(t *testing.T) {
	input1 := `
          2020-01-01 * "BarAlice" "Beer with my guy friends ASDF"
            Assets:Cash           -10.00 USD
            Expenses:Food:Drinks    2.00 USD
              shared: "Assets:Debtors:Bob 4.00 USD"
              shared901: "Assets:Debtors:Bob 4.00 USD"
            Assets:Debtors:Bob      4.00 USD
              shared: "Expenses:Food:Drinks 4.00 USD"
            Assets:Debtors:Bob      4.00 USD
              shared: "Expenses:Food:Drinks 4.00 USD"
        `
	input2 := `
          2020-01-01 * "BarAlice" "Beer with my guy friends ASDF"
            Assets:Cash           -10.00 USD
            Expenses:Food:Drinks    2.00 USD
              shared: "Assets:Debtors:Bob 4.00 USD"
              shared901: "Assets:Debtors:Bob 4.00 USD"
            Assets:Debtors:Bob      4.00 USD
              shared: "Expenses:Food:Drinks 4.00 USD"
        `
	entries1, _, _ := parser.ParseString(input1)
	entries2, _, _ := parser.ParseString(input2)
	hashes1, _ := core.HashEntries(entries1, true)
	hashes2, _ := core.HashEntries(entries2, true)

	if len(hashes1) != 1 || len(hashes2) != 1 {
		t.Fatalf("Expected 1 hash for each, got %d and %d", len(hashes1), len(hashes2))
	}

	var h1, h2 string
	for k := range hashes1 {
		h1 = k
	}
	for k := range hashes2 {
		h2 = k
	}

	if h1 == h2 {
		t.Errorf("Hashes should be different for different number of postings")
	}
}

func TestCompareEntries(t *testing.T) {
	entries1, _, _ := parser.ParseString(TEST_INPUT)
	entries2, _, _ := parser.ParseString(TEST_INPUT)

	// Check two equal sets.
	same, missing1, missing2, err := core.CompareEntries(entries1, entries2)
	if err != nil {
		t.Fatalf("CompareEntries failed: %v", err)
	}
	if !same || len(missing1) != 0 || len(missing2) != 0 {
		t.Errorf("Entries should be same")
	}

	// First > Second.
	same, missing1, missing2, _ = core.CompareEntries(entries1, entries2[:len(entries2)-1])
	if same || len(missing1) != 1 || len(missing2) != 0 {
		t.Errorf("First > Second should fail same")
	}
	if _, ok := missing1[0].(*core.Close); !ok {
		t.Errorf("Missing entry should be Close")
	}

	// First < Second.
	same, missing1, missing2, _ = core.CompareEntries(entries1[:len(entries1)-1], entries2)
	if same || len(missing1) != 0 || len(missing2) != 1 {
		t.Errorf("First < Second should fail same")
	}
	if _, ok := missing2[0].(*core.Close); !ok {
		t.Errorf("Missing entry should be Close")
	}

	// Both have missing.
	same, missing1, missing2, _ = core.CompareEntries(entries1[1:], entries2[:len(entries2)-1])
	if same || len(missing1) != 1 || len(missing2) != 1 {
		t.Errorf("Both have missing should fail same")
	}
	if _, ok := missing1[0].(*core.Close); !ok {
		t.Errorf("Missing1 should be Close")
	}
	if _, ok := missing2[0].(*core.Open); !ok {
		t.Errorf("Missing2 should be Open")
	}
}

func TestIncludesEntriesParity(t *testing.T) {
	entries1, _, _ := parser.ParseString(TEST_INPUT)
	entries2, _, _ := parser.ParseString(TEST_INPUT)

	includes, missing, err := core.IncludesEntries(entries1[:len(entries1)-3], entries2)
	if err != nil {
		t.Fatalf("IncludesEntries failed: %v", err)
	}
	if !includes || len(missing) != 0 {
		t.Errorf("Should include subset")
	}

	includes, missing, _ = core.IncludesEntries(entries1, entries2[:len(entries2)-3])
	if includes || len(missing) != 3 {
		t.Errorf("Should not include superset, missing %d", len(missing))
	}
}

func TestExcludesEntriesParity(t *testing.T) {
	entries1, _, _ := parser.ParseString(TEST_INPUT)
	entries2, _, _ := parser.ParseString(TEST_INPUT)

	excludes, extra, err := core.ExcludesEntries(entries1[:4], entries2)
	if err != nil {
		t.Fatalf("ExcludesEntries failed: %v", err)
	}
	if excludes || len(extra) != 4 {
		t.Errorf("Should not exclude overlapping set")
	}

	excludes, extra, _ = core.ExcludesEntries(entries1[:4], entries2[4:])
	if !excludes || len(extra) != 0 {
		t.Errorf("Should exclude disjoint set")
	}
}

func TestHashWithExcludeMetaParity(t *testing.T) {
	input := `
2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash

2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash
`
	entries, _, _ := parser.ParseString(input)
	if len(entries) != 2 {
		t.Fatalf("Expected 2 entries, got %d", len(entries))
	}

	h1 := core.HashEntry(entries[0], false)
	h2 := core.HashEntry(entries[1], false)
	if h1 == h2 {
		t.Errorf("Hashes should be different with meta (due to line numbers)")
	}

	h1_no_meta := core.HashEntry(entries[0], true)
	h2_no_meta := core.HashEntry(entries[1], true)
	if h1_no_meta != h2_no_meta {
		t.Errorf("Hashes should be same without meta")
	}
}
