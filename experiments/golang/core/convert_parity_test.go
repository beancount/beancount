package core

import (
	"testing"
	"time"
)

func TestConvertPositionParity(t *testing.T) {
	date := time.Date(2014, 1, 1, 0, 0, 0, 0, time.UTC)
	priceMap := make(PriceMap)
	priceMap.Add("HOOL", date, NewAmount(D("500"), "USD"))

	t.Run("basic", func(t *testing.T) {
		pos := Position{Units: NewAmount(D("10"), "HOOL")}
		amt, err := ConvertPosition(pos, "USD", priceMap, date)
		if err != nil {
			t.Errorf("ConvertPosition failed: %v", err)
		}
		if amt.Currency != "USD" || amt.Number.Cmp(D("5000")) != 0 {
			t.Errorf("Wrong converted amount: %v", amt)
		}
	})

	t.Run("same_currency", func(t *testing.T) {
		pos := Position{Units: NewAmount(D("10"), "USD")}
		amt, err := ConvertPosition(pos, "USD", priceMap, date)
		if err != nil {
			t.Errorf("ConvertPosition failed: %v", err)
		}
		if amt.Currency != "USD" || amt.Number.Cmp(D("10")) != 0 {
			t.Errorf("Wrong converted amount: %v", amt)
		}
	})

	t.Run("with_cost", func(t *testing.T) {
		pos := Position{
			Units: NewAmount(D("10"), "HOOL"),
			Cost:  &Cost{Number: D("450"), Currency: "USD"},
		}
		// Should use cost currency if matching.
		amt, err := ConvertPosition(pos, "USD", priceMap, date)
		if err != nil {
			t.Errorf("ConvertPosition failed: %v", err)
		}
		if amt.Currency != "USD" || amt.Number.Cmp(D("4500")) != 0 {
			t.Errorf("Wrong converted amount: %v", amt)
		}
	})
}

func TestConvertAmountParity(t *testing.T) {
	date := time.Date(2014, 1, 1, 0, 0, 0, 0, time.UTC)
	priceMap := make(PriceMap)
	priceMap.Add("HOOL", date, NewAmount(D("500"), "USD"))
	priceMap.Add("USD", date, NewAmount(D("1.1"), "CAD"))

	t.Run("basic", func(t *testing.T) {
		amount := NewAmount(D("10"), "HOOL")
		amt, err := ConvertAmount(amount, "USD", priceMap, date)
		if err != nil {
			t.Errorf("ConvertAmount failed: %v", err)
		}
		if amt.Currency != "USD" || amt.Number.Cmp(D("5000")) != 0 {
			t.Errorf("Wrong converted amount: %v", amt)
		}
	})

	t.Run("multi_hop", func(t *testing.T) {
		amount := NewAmount(D("10"), "HOOL")
		amt, err := ConvertAmount(amount, "CAD", priceMap, date, "USD")
		if err != nil {
			t.Errorf("ConvertAmount failed: %v", err)
		}
		if amt.Currency != "CAD" || amt.Number.Cmp(D("5500")) != 0 {
			t.Errorf("Wrong converted amount: %v", amt)
		}
	})
}

func TestValueParity(t *testing.T) {
	date := time.Date(2014, 1, 1, 0, 0, 0, 0, time.UTC)
	priceMap := make(PriceMap)
	priceMap.Add("HOOL", date, NewAmount(D("500"), "USD"))

	pos := Position{Units: NewAmount(D("10"), "HOOL")}
	amt, err := ConvertPosition(pos, "USD", priceMap, date)
	if err != nil {
		t.Errorf("ConvertPosition failed: %v", err)
	}
	_ = amt
}
