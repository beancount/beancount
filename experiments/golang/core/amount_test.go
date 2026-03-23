package core

import (
	"sort"
	"testing"
)

func TestAmountConstructor(t *testing.T) {
	a := NewAmount(D("100.00"), "USD")
	if a.Number.Cmp(D("100")) != 0 || a.Currency != "USD" {
		t.Errorf("Wrong amount: %v", a)
	}
}

func TestAmountFromString(t *testing.T) {
	a, _ := AmountFromString("100.00 USD")
	if a.Number.Cmp(D("100")) != 0 || a.Currency != "USD" {
		t.Errorf("Wrong amount: %v", a)
	}

	a2, _ := AmountFromString("-50.5 CAD")
	if a2.Number.Cmp(D("-50.5")) != 0 || a2.Currency != "CAD" {
		t.Errorf("Wrong amount: %v", a2)
	}
}

func TestAmountToString(t *testing.T) {
	a := NewAmount(D("100"), "USD")
	if a.String() != "100 USD" {
		t.Errorf("Wrong string: %s", a.String())
	}
}

func TestAmountComparisons(t *testing.T) {
	a1 := NewAmount(D("100"), "USD")
	a2 := NewAmount(D("100"), "USD")
	a3 := NewAmount(D("101"), "USD")
	a4 := NewAmount(D("100"), "CAD")

	if !a1.Equal(a2) {
		t.Errorf("a1 should equal a2")
	}
	if a1.Equal(a3) {
		t.Errorf("a1 should NOT equal a3")
	}
	if a1.Equal(a4) {
		t.Errorf("a1 should NOT equal a4")
	}
}

func TestAmountSort(t *testing.T) {
	a1 := NewAmount(D("100"), "USD")
	a2 := NewAmount(D("101"), "USD")
	a3 := NewAmount(D("100"), "CAD")
	a4 := NewAmount(D("50"), "ZZZ")

	amounts := []Amount{a4, a3, a2, a1}
	sort.Slice(amounts, func(i, j int) bool {
		if amounts[i].Currency != amounts[j].Currency {
			return amounts[i].Currency < amounts[j].Currency
		}
		return amounts[i].Number.Cmp(amounts[j].Number) < 0
	})

	if amounts[0].Currency != "CAD" {
		t.Errorf("Expected CAD first alphabetically, got %s", amounts[0].Currency)
	}
}

func TestAmountArithmetic(t *testing.T) {
	a1 := NewAmount(D("100"), "USD")
	a2 := NewAmount(D("50"), "USD")

	// Add
	res, _ := a1.Add(a2)
	if res.Number.Cmp(D("150")) != 0 {
		t.Errorf("Add failed: %v", res)
	}

	// Sub
	res, _ = a1.Sub(a2)
	if res.Number.Cmp(D("50")) != 0 {
		t.Errorf("Sub failed: %v", res)
	}

	// Mul
	res = a1.Mul(D("3"))
	if res.Number.Cmp(D("300")) != 0 {
		t.Errorf("Mul failed: %v", res)
	}

	// Neg
	res = a1.Neg()
	if res.Number.Cmp(D("-100")) != 0 {
		t.Errorf("Neg failed: %v", res)
	}

	// Abs
	res = res.Abs()
	if res.Number.Cmp(D("100")) != 0 {
		t.Errorf("Abs failed: %v", res)
	}
}
