package core

import (
	"testing"
)

func TestInventoryFromString(t *testing.T) {
	inv, _ := InventoryFromString("10 USD, 2 CAD")
	if len(inv.Positions) != 2 {
		t.Errorf("Expected 2 positions, got %d", len(inv.Positions))
	}

	inv2, _ := InventoryFromString("2.2 HOOL {532.43 USD}, 3.413 EUR")
	if len(inv2.Positions) != 2 {
		t.Errorf("Expected 2 positions, got %d", len(inv2.Positions))
	}
}

func TestInventoryAddAmount(t *testing.T) {
	inv := NewInventory()
	inv.AddAmount(NewAmount(D("100"), "USD"), nil)
	inv.AddAmount(NewAmount(D("25.01"), "USD"), nil)
	
	units := inv.GetCurrencyUnits("USD")
	if units.Number.Cmp(D("125.01")) != 0 {
		t.Errorf("Expected 125.01 USD, got %v", units)
	}

	inv.AddAmount(NewAmount(D("-125.01"), "USD"), nil)
	if !inv.IsEmpty() {
		t.Errorf("Inventory should be empty after removing all units")
	}
}

func TestInventoryArithmetic(t *testing.T) {
	inv, _ := InventoryFromString("100 USD, -50 CAD")
	
	// Neg
	negInv := inv.Neg()
	expectedNeg, _ := InventoryFromString("-100 USD, 50 CAD")
	if !inventoriesEqual(negInv, expectedNeg) {
		t.Errorf("Neg failed: got %v, expected %v", negInv, expectedNeg)
	}

	// Abs
	absInv := inv.Abs()
	expectedAbs, _ := InventoryFromString("100 USD, 50 CAD")
	if !inventoriesEqual(absInv, expectedAbs) {
		t.Errorf("Abs failed: got %v, expected %v", absInv, expectedAbs)
	}
}

func TestInventoryIsSmall(t *testing.T) {
	inv, _ := InventoryFromString("0.03 JPY, 0.003 USD")
	
	if !inv.IsSmallValue(D("0.05")) {
		t.Errorf("Should be small under 0.05")
	}
	if inv.IsSmallValue(D("0.01")) {
		t.Errorf("Should NOT be small under 0.01")
	}

	tols := map[string]Decimal{
		"JPY": D("0.05"),
		"USD": D("0.005"),
	}
	if !inv.IsSmall(tols) {
		t.Errorf("Should be small under per-currency tolerances")
	}
}

func TestInventoryIsMixed(t *testing.T) {
	inv, _ := InventoryFromString("100 HOOL {250 USD}, 101 HOOL {251 USD}")
	if inv.IsMixed() {
		t.Errorf("Should NOT be mixed")
	}

	inv2, _ := InventoryFromString("100 HOOL {250 USD}, -1 HOOL {251 USD}")
	if !inv2.IsMixed() {
		t.Errorf("Should be mixed")
	}
}

func TestInventoryIsReducedBy(t *testing.T) {
	inv, _ := InventoryFromString("100 HOOL {250 USD}, 101 HOOL {251 USD}")
	if inv.IsReducedBy(NewAmount(D("2"), "HOOL")) {
		t.Errorf("Should NOT be reduced by positive amount")
	}
	if !inv.IsReducedBy(NewAmount(D("-2"), "HOOL")) {
		t.Errorf("Should be reduced by negative amount")
	}
}

func TestInventoryAverage(t *testing.T) {
	inv, _ := InventoryFromString("40 USD {1.01 CAD}, 40 USD {1.02 CAD}")
	avg := inv.Average()
	expected, _ := InventoryFromString("80.00 USD {1.015 CAD}")
	if !inventoriesEqual(avg, expected) {
		t.Errorf("Average failed: got %v, expected %v", avg, expected)
	}
}

func TestInventoryReduce(t *testing.T) {
	inv, _ := InventoryFromString("100.00 USD, 101.00 CAD, 100 HOOL {300.00 USD}")
	unitsInv := inv.Reduce(func(p Position) Amount {
		return p.Units
	})
	expected, _ := InventoryFromString("100.00 USD, 101.00 CAD, 100 HOOL")
	if !inventoriesEqual(unitsInv, expected) {
		t.Errorf("Reduce failed: got %v, expected %v", unitsInv, expected)
	}
}
