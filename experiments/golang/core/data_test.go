package core

import (
	"reflect"
	"testing"
	"time"
)

func TestSortDirectives(t *testing.T) {
	date1 := time.Date(2014, 1, 15, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2014, 1, 18, 0, 0, 0, 0, time.UTC)
	date3 := time.Date(2014, 1, 20, 0, 0, 0, 0, time.UTC)

	account := "Assets:Bank:Checking"

	d1 := &Transaction{Meta: NewMetadata(".", 1100), Date: date3, Narration: "Next day"}
	d2 := &Close{Meta: NewMetadata(".", 1000), Date: date2, Account: account}
	d3 := &Balance{Meta: NewMetadata(".", 1001), Date: date2, Account: account, Amount: NewAmount(D("200"), "USD")}
	d4 := &Open{Meta: NewMetadata(".", 1002), Date: date2, Account: account}
	d5 := &Transaction{Meta: NewMetadata(".", 1009), Date: date2, Narration: "Transaction 2"}
	d6 := &Transaction{Meta: NewMetadata(".", 1008), Date: date2, Narration: "Transaction 1"}
	d7 := &Transaction{Meta: NewMetadata(".", 900), Date: date1, Narration: "Previous day"}

	directives := []Directive{d1, d2, d3, d4, d5, d6, d7}
	SortDirectives(directives)

	expected := []Directive{d7, d4, d3, d6, d5, d2, d1}

	for i := range directives {
		if directives[i] != expected[i] {
			t.Errorf("At index %d: got %v (line %v), expected %v (line %v)",
				i, directives[i].Type(), directives[i].GetMeta()["lineno"],
				expected[i].Type(), expected[i].GetMeta()["lineno"])
		}
	}
}

func TestFilterTransactions(t *testing.T) {
	d1 := &Transaction{Meta: NewMetadata(".", 1), Narration: "T1"}
	d2 := &Open{Meta: NewMetadata(".", 2), Account: "A1"}
	d3 := &Transaction{Meta: NewMetadata(".", 3), Narration: "T2"}

	directives := []Directive{d1, d2, d3}
	txns := FilterTransactions(directives)

	if len(txns) != 2 {
		t.Errorf("Expected 2 transactions, got %d", len(txns))
	}
	if txns[0] != d1 || txns[1] != d3 {
		t.Errorf("Wrong transactions returned")
	}
}

func TestHasConversion(t *testing.T) {
	p1 := Posting{
		Account: "A1",
		Units:   NewAmount(D("100"), "MSFT"),
		Price:   NewAmount(D("150"), "USD"),
	}
	if !PostingHasConversion(p1) {
		t.Errorf("p1 should have conversion")
	}

	p2 := Posting{
		Account: "A1",
		Units:   NewAmount(D("100"), "MSFT"),
		Cost:    &Cost{Number: D("120"), Currency: "USD"},
		Price:   NewAmount(D("150"), "USD"),
	}
	if PostingHasConversion(p2) {
		t.Errorf("p2 should NOT have conversion (has cost)")
	}

	txn := &Transaction{
		Postings: []Posting{p1, p2},
	}
	if !TransactionHasConversion(txn) {
		t.Errorf("txn should have conversion")
	}
}

func TestMetadata(t *testing.T) {
	meta := NewMetadata("file.beancount", 10)
	if meta["filename"] != "file.beancount" {
		t.Errorf("Wrong filename in meta")
	}
	if meta["lineno"] != 10 {
		t.Errorf("Wrong lineno in meta")
	}
}

func TestDeepEqualIssue(t *testing.T) {
	// Pointers to different big.Int with same value might fail reflect.DeepEqual
	// but our implementation uses Equal() for amounts/positions when needed.
	// This is just a reminder for future complex tests.
	d1 := D("100")
	d2 := D("100")
	if d1 == d2 {
		t.Errorf("Should be different pointers")
	}
	if d1.Cmp(d2) != 0 {
		t.Errorf("Should represent same value")
	}
	if reflect.DeepEqual(d1, d2) {
		// This might actually pass depending on decimal library structure,
		// but typically pointer equality fails reflect.DeepEqual if it compares pointers.
	}
}
