package core

import (
	"testing"
	"time"
)

func TestConvertAmount(t *testing.T) {
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
