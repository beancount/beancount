package core

import (
	"testing"
	"time"
)

func TestBuildPriceMap(t *testing.T) {
	d1 := time.Date(2013, 6, 1, 0, 0, 0, 0, time.UTC)
	d2 := time.Date(2013, 6, 2, 0, 0, 0, 0, time.UTC)

	p1 := &Price{Date: d1, Currency: "USD", Amount: NewAmount(D("1.10"), "CAD")}
	p2 := &Price{Date: d2, Currency: "USD", Amount: NewAmount(D("1.11"), "CAD")}
	p3 := &Price{Date: d2, Currency: "USD", Amount: NewAmount(D("1.12"), "CAD")}

	priceMap := BuildPriceMap([]Directive{p1, p2, p3})

	points := priceMap[[2]string{"USD", "CAD"}]
	if len(points) != 2 {
		t.Fatalf("Expected 2 price points, got %d", len(points))
	}
	if points[0].Number.Cmp(D("1.10")) != 0 {
		t.Errorf("Expected 1.10, got %v", points[0].Number)
	}
	if points[1].Number.Cmp(D("1.12")) != 0 {
		t.Errorf("Expected 1.12 (last one for date), got %v", points[1].Number)
	}

	// Check inverse
	invPoints := priceMap[[2]string{"CAD", "USD"}]
	if len(invPoints) != 2 {
		t.Fatalf("Expected 2 inverse price points, got %d", len(invPoints))
	}
}

func TestGetPrice(t *testing.T) {
	d1 := time.Date(2013, 6, 1, 0, 0, 0, 0, time.UTC)
	d2 := time.Date(2013, 6, 10, 0, 0, 0, 0, time.UTC)

	p1 := &Price{Date: d1, Currency: "USD", Amount: NewAmount(D("1.00"), "CAD")}
	p2 := &Price{Date: d2, Currency: "USD", Amount: NewAmount(D("1.50"), "CAD")}

	priceMap := BuildPriceMap([]Directive{p1, p2})

	// Before first price
	amt, err := priceMap.GetPrice("USD", "CAD", time.Date(2013, 5, 1, 0, 0, 0, 0, time.UTC))
	if err == nil {
		t.Errorf("Expected error before first price, got %v", amt)
	}

	// At first price
	amt, err = priceMap.GetPrice("USD", "CAD", d1)
	if err != nil || amt.Number.Cmp(D("1.00")) != 0 {
		t.Errorf("Expected 1.00 at d1, got %v (err: %v)", amt, err)
	}

	// Between prices
	amt, err = priceMap.GetPrice("USD", "CAD", time.Date(2013, 6, 5, 0, 0, 0, 0, time.UTC))
	if err != nil || amt.Number.Cmp(D("1.00")) != 0 {
		t.Errorf("Expected 1.00 between prices, got %v (err: %v)", amt, err)
	}

	// Latest price
	amt, err = priceMap.GetPrice("USD", "CAD", time.Time{})
	if err != nil || amt.Number.Cmp(D("1.50")) != 0 {
		t.Errorf("Expected 1.50 for latest price, got %v (err: %v)", amt, err)
	}
}

func TestProject(t *testing.T) {
	d1 := time.Date(2013, 6, 1, 0, 0, 0, 0, time.UTC)
	d2 := time.Date(2013, 6, 15, 0, 0, 0, 0, time.UTC)

	p1 := &Price{Date: d1, Currency: "USD", Amount: NewAmount(D("1.12"), "CAD")}
	p2 := &Price{Date: d2, Currency: "HOOL", Amount: NewAmount(D("1000.00"), "USD")}

	priceMap := BuildPriceMap([]Directive{p1, p2})
	newPriceMap := Project(priceMap, "USD", "CAD", nil)

	amt, err := newPriceMap.GetPrice("HOOL", "CAD", d2)
	if err != nil || amt.Number.Cmp(D("1120.00")) != 0 {
		t.Errorf("Expected 1120.00 for HOOL in CAD, got %v (err: %v)", amt, err)
	}
}
