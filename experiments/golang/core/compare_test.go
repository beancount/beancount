package core

import (
	"testing"
	"time"
)

func TestHashEntries(t *testing.T) {
	date := time.Date(2012, 2, 1, 0, 0, 0, 0, time.UTC)
	d1 := &Open{Meta: NewMetadata("f", 1), Date: date, Account: "Assets:Cash"}
	d2 := &Open{Meta: NewMetadata("f", 2), Date: date, Account: "Assets:Cash"}

	h1 := HashEntry(d1, false)
	h2 := HashEntry(d2, false)
	if h1 == h2 {
		t.Errorf("Hashes should be different with meta")
	}

	h1_no_meta := HashEntry(d1, true)
	h2_no_meta := HashEntry(d2, true)
	if h1_no_meta != h2_no_meta {
		t.Errorf("Hashes should be same without meta")
	}
}

func TestHashEntriesWithDuplicates(t *testing.T) {
	date := time.Date(2014, 8, 1, 0, 0, 0, 0, time.UTC)
	p1 := &Price{Meta: NewMetadata("f", 1), Date: date, Currency: "HOOL", Amount: NewAmount(D("603.10"), "USD")}
	p2 := &Price{Meta: NewMetadata("f", 2), Date: date, Currency: "HOOL", Amount: NewAmount(D("603.10"), "USD")}

	entries := []Directive{p1, p2}
	hashes, errs := HashEntries(entries, true)
	if len(errs) > 0 {
		t.Errorf("Unexpected errors: %v", errs)
	}
	if len(hashes) != 1 {
		t.Errorf("Expected 1 hash for duplicate prices, got %d", len(hashes))
	}
}

func TestCompareEntries(t *testing.T) {
	date := time.Date(2012, 2, 1, 0, 0, 0, 0, time.UTC)
	d1 := &Open{Meta: NewMetadata("f1", 1), Date: date, Account: "Assets:Cash"}
	d2 := &Open{Meta: NewMetadata("f2", 2), Date: date, Account: "Assets:Cash"}

	same, missing1, missing2, err := CompareEntries([]Directive{d1}, []Directive{d2})
	if err != nil {
		t.Fatalf("CompareEntries failed: %v", err)
	}
	if !same || len(missing1) != 0 || len(missing2) != 0 {
		t.Errorf("Entries should be same")
	}

	d3 := &Open{Meta: NewMetadata("f3", 3), Date: date, Account: "Assets:Bank"}
	same, missing1, missing2, err = CompareEntries([]Directive{d1, d3}, []Directive{d2})
	if err != nil {
		t.Fatalf("CompareEntries failed: %v", err)
	}
	if same || len(missing1) != 1 || len(missing2) != 0 {
		t.Errorf("Entries should be different")
	}
}

func TestIncludesEntries(t *testing.T) {
	date := time.Date(2012, 2, 1, 0, 0, 0, 0, time.UTC)
	d1 := &Open{Meta: NewMetadata("f1", 1), Date: date, Account: "Assets:Cash"}
	d2 := &Open{Meta: NewMetadata("f2", 2), Date: date, Account: "Assets:Bank"}

	includes, missing, err := IncludesEntries([]Directive{d1}, []Directive{d1, d2})
	if err != nil {
		t.Fatalf("IncludesEntries failed: %v", err)
	}
	if !includes || len(missing) != 0 {
		t.Errorf("Should include d1")
	}

	includes, missing, err = IncludesEntries([]Directive{d1, d2}, []Directive{d1})
	if err != nil {
		t.Fatalf("IncludesEntries failed: %v", err)
	}
	if includes || len(missing) != 1 {
		t.Errorf("Should not include d2")
	}
}

func TestExcludesEntries(t *testing.T) {
	date := time.Date(2012, 2, 1, 0, 0, 0, 0, time.UTC)
	d1 := &Open{Meta: NewMetadata("f1", 1), Date: date, Account: "Assets:Cash"}
	d2 := &Open{Meta: NewMetadata("f2", 2), Date: date, Account: "Assets:Bank"}
	d3 := &Open{Meta: NewMetadata("f3", 3), Date: date, Account: "Assets:Other"}

	excludes, extra, err := ExcludesEntries([]Directive{d1}, []Directive{d2, d3})
	if err != nil {
		t.Fatalf("ExcludesEntries failed: %v", err)
	}
	if !excludes || len(extra) != 0 {
		t.Errorf("Should exclude d1")
	}

	excludes, extra, err = ExcludesEntries([]Directive{d1}, []Directive{d1, d2})
	if err != nil {
		t.Fatalf("ExcludesEntries failed: %v", err)
	}
	if excludes || len(extra) != 1 {
		t.Errorf("Should not exclude d1")
	}
}
