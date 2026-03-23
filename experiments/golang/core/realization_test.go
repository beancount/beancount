package core

import (
	"testing"
	"time"
)

func TestRealAccountGetOrCreate(t *testing.T) {
	root := NewRealAccount("")
	checking := root.GetOrCreate("Assets:US:Bank:Checking")
	
	if checking.AccountName != "Assets:US:Bank:Checking" {
		t.Errorf("Wrong account name: %q", checking.AccountName)
	}
	
	if _, ok := root.Children["Assets"]; !ok {
		t.Errorf("Assets not created under root")
	}
	
	assets := root.Children["Assets"]
	if _, ok := assets.Children["US"]; !ok {
		t.Errorf("US not created under Assets")
	}
}

func TestRealize(t *testing.T) {
	directives := []Directive{
		&Open{Account: "Assets:Checking", Date: D1},
		&Transaction{
			Narration: "T1",
			Postings: []Posting{
				{Account: "Assets:Checking", Units: NewAmount(D("100"), "USD")},
				{Account: "Income:Salary", Units: NewAmount(D("-100"), "USD")},
			},
		},
	}
	
	root := Realize(directives, nil)
	
	checking := root.Get("Assets:Checking")
	if checking == nil {
		t.Fatal("Assets:Checking not found")
	}
	
	if len(checking.Postings) != 2 { // Open + TxnPosting
		t.Errorf("Expected 2 postings in checking, got %d", len(checking.Postings))
	}
	
	if checking.Balance.GetCurrencyUnits("USD").Number.Cmp(D("100")) != 0 {
		t.Errorf("Wrong balance in checking: %v", checking.Balance)
	}
}

func TestComputeBalanceRecursive(t *testing.T) {
	root := NewRealAccount("")
	root.GetOrCreate("Assets:Bank:Checking").Balance.AddAmount(NewAmount(D("100"), "USD"), nil)
	root.GetOrCreate("Assets:Bank:Savings").Balance.AddAmount(NewAmount(D("200"), "USD"), nil)
	root.GetOrCreate("Assets:Cash").Balance.AddAmount(NewAmount(D("50"), "USD"), nil)
	
	assets := root.Get("Assets")
	total := assets.ComputeBalance()
	
	if total.GetCurrencyUnits("USD").Number.Cmp(D("350")) != 0 {
		t.Errorf("Wrong total balance for Assets: %v", total)
	}
}

var D1 = timeMustParse("2023-01-01")

func timeMustParse(s string) time.Time {
	t, err := time.Parse("2006-01-02", s)
	if err != nil {
		panic(err)
	}
	return t
}
