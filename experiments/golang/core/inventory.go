package core

import (
	"strings"

	"github.com/ericlagergren/decimal"
)

// MatchResult represents the result of an inventory matching operation.
type MatchResult int

const (
	MatchIgnored MatchResult = iota
	MatchCreated
	MatchReduced
	MatchAugmented
)

// InventoryKey is a composite key for positions in an inventory.
type InventoryKey struct {
	Currency     string
	CostNumber   string
	CostCurrency string
	CostDate     string
	CostLabel    string
}

// GetInventoryKey returns an InventoryKey for a currency and cost.
func GetInventoryKey(currency string, cost *Cost) InventoryKey {
	key := InventoryKey{Currency: currency}
	if cost != nil {
		key.CostNumber = Format(cost.Number)
		key.CostCurrency = cost.Currency
		if !cost.Date.IsZero() {
			key.CostDate = cost.Date.Format("2006-01-02")
		}
		key.CostLabel = cost.Label
	}
	return key
}

// Inventory represents a collection of positions.
type Inventory struct {
	Positions map[InventoryKey]Position
}

// NewInventory creates a new empty inventory.
func NewInventory() *Inventory {
	return &Inventory{
		Positions: make(map[InventoryKey]Position),
	}
}

// Clone returns a deep copy of the inventory.
func (inv *Inventory) Clone() *Inventory {
	newInv := NewInventory()
	for k, v := range inv.Positions {
		newInv.Positions[k] = v
	}
	return newInv
}

// Equals returns true if two inventories are equal.
func (inv *Inventory) Equals(other *Inventory) bool {
	if len(inv.Positions) != len(other.Positions) {
		return false
	}
	for k, v := range inv.Positions {
		ov, ok := other.Positions[k]
		if !ok || !v.Units.Equal(ov.Units) {
			return false
		}
	}
	return true
}

// LessThan returns true if this inventory is "less than" another (for sorting).
func (inv *Inventory) LessThan(other *Inventory) bool {
	ps1 := inv.GetPositions()
	ps2 := other.GetPositions()
	SortPositions(ps1)
	SortPositions(ps2)

	for i := 0; i < len(ps1) && i < len(ps2); i++ {
		if !ps1[i].Units.Equal(ps2[i].Units) {
			return ps1[i].SortKey().LessThan(ps2[i].SortKey())
		}
	}
	return len(ps1) < len(ps2)
}

// AddAmount adds an amount and optional cost to the inventory.
func (inv *Inventory) AddAmount(units Amount, cost *Cost) (Position, MatchResult) {
	if units.Number.Sign() == 0 {
		return Position{}, MatchIgnored
	}

	key := GetInventoryKey(units.Currency, cost)
	pos, ok := inv.Positions[key]

	if !ok {
		newPos := Position{Units: units, Cost: cost}
		inv.Positions[key] = newPos
		return Position{}, MatchCreated
	}

	resUnits, _ := pos.Units.Add(units)
	pos.Units = resUnits
	if pos.Units.Number.Sign() == 0 {
		delete(inv.Positions, key)
		return Position{}, MatchReduced
	}
	inv.Positions[key] = pos
	if SameSign(pos.Units.Number, units.Number) {
		return pos, MatchAugmented
	}
	return pos, MatchReduced
}

// AddPosition adds a position to the inventory.
func (inv *Inventory) AddPosition(pos Position) (Position, MatchResult) {
	return inv.AddAmount(pos.Units, pos.Cost)
}

// AddInventory adds another inventory to this one.
func (inv *Inventory) AddInventory(other *Inventory) {
	for _, p := range other.Positions {
		inv.AddPosition(p)
	}
}

// SubInventory subtracts another inventory from this one.
func (inv *Inventory) SubInventory(other *Inventory) {
	for _, p := range other.Positions {
		inv.AddAmount(p.Units.Neg(), p.Cost)
	}
}

// Neg returns a new inventory with all positions negated.
func (inv *Inventory) Neg() *Inventory {
	newInv := NewInventory()
	for _, p := range inv.Positions {
		newInv.AddAmount(p.Units.Neg(), p.Cost)
	}
	return newInv
}

// Abs returns a new inventory with all positions having absolute units.
func (inv *Inventory) Abs() *Inventory {
	newInv := NewInventory()
	for _, p := range inv.Positions {
		absUnits := p.Units
		absUnits.Number = new(decimal.Big).Abs(p.Units.Number)
		newInv.AddAmount(absUnits, p.Cost)
	}
	return newInv
}

// IsEmpty returns true if the inventory has no positions.
func (inv *Inventory) IsEmpty() bool {
	return len(inv.Positions) == 0
}

// Currencies returns the set of unit currencies held.
func (inv *Inventory) Currencies() map[string]struct{} {
	ccys := make(map[string]struct{})
	for k := range inv.Positions {
		ccys[k.Currency] = struct{}{}
	}
	return ccys
}

// CostCurrencies returns the set of cost currencies held.
func (inv *Inventory) CostCurrencies() map[string]struct{} {
	ccys := make(map[string]struct{})
	for _, p := range inv.Positions {
		if p.Cost != nil {
			ccys[p.Cost.Currency] = struct{}{}
		}
	}
	return ccys
}

// Reduce reduces the inventory to a single currency balance using a reducer function.
func (inv *Inventory) Reduce(reducer func(Position) Amount) *Inventory {
	newInv := NewInventory()
	for _, p := range inv.Positions {
		newInv.AddAmount(reducer(p), nil)
	}
	return newInv
}

// GetCurrencyUnits returns the total units for a given currency.
func (inv *Inventory) GetCurrencyUnits(currency string) Amount {
	total := NewAmount(ZERO, currency)
	for _, p := range inv.Positions {
		if p.Units.Currency == currency && p.Cost == nil {
			total, _ = total.Add(p.Units)
		}
	}
	return total
}

// Average reduces the inventory by averaging positions with the same units currency.
func (inv *Inventory) Average() *Inventory {
	newInv := NewInventory()
	type entry struct {
		units Amount
		cost  Amount
	}
	totals := make(map[string]entry)
	for _, p := range inv.Positions {
		e := totals[p.Units.Currency]
		if e.units.Number == nil {
			e.units = NewAmount(ZERO, p.Units.Currency)
		}
		e.units, _ = e.units.Add(p.Units)
		
		// Use total cost (weight)
		weight := GetWeight(p)
		if e.cost.Number == nil {
			e.cost = NewAmount(ZERO, weight.Currency)
		}
		e.cost, _ = e.cost.Add(weight)
		totals[p.Units.Currency] = e
	}
	for _, e := range totals {
		if e.units.Number.Sign() != 0 {
			costNum := new(decimal.Big)
			DefaultContext.Quo(costNum, e.cost.Number, e.units.Number)
			newInv.AddAmount(e.units, &Cost{Number: costNum, Currency: e.cost.Currency})
		}
	}
	return newInv
}

// String returns a human-readable string representation of the inventory.
func (inv *Inventory) String() string {
	var parts []string
	positions := inv.GetPositions()
	SortPositions(positions)
	for _, p := range positions {
		parts = append(parts, p.String())
	}
	if len(parts) == 0 {
		return ""
	}
	return "(" + strings.Join(parts, ", ") + ")"
}

// GetPositions returns all positions in the inventory as a slice.
func (inv *Inventory) GetPositions() []Position {
	ps := make([]Position, 0, len(inv.Positions))
	for _, p := range inv.Positions {
		ps = append(ps, p)
	}
	return ps
}

// IsSmall returns true if all positions are below a tolerance.
func (inv *Inventory) IsSmall(tolerance map[string]Decimal) bool {
	for _, p := range inv.Positions {
		tol, ok := tolerance[p.Units.Currency]
		if !ok {
			tol = ZERO
		}
		if new(decimal.Big).Abs(p.Units.Number).Cmp(tol) > 0 {
			return false
		}
	}
	return true
}

// IsSmallValue returns true if all positions are below a single tolerance.
func (inv *Inventory) IsSmallValue(tolerance Decimal) bool {
	for _, p := range inv.Positions {
		if new(decimal.Big).Abs(p.Units.Number).Cmp(tolerance) > 0 {
			return false
		}
	}
	return true
}

// IsMixed returns true if the inventory has both positive and negative positions.
func (inv *Inventory) IsMixed() bool {
	pos := false
	neg := false
	for _, p := range inv.Positions {
		if p.Units.Number.Sign() > 0 {
			pos = true
		} else if p.Units.Number.Sign() < 0 {
			neg = true
		}
	}
	return pos && neg
}

// IsReducedBy returns true if the inventory has any position that would be reduced by the given units.
func (inv *Inventory) IsReducedBy(units Amount) bool {
	for _, p := range inv.Positions {
		if p.Units.Currency == units.Currency && !SameSign(p.Units.Number, units.Number) {
			return true
		}
	}
	return false
}

// InventoryFromString parses a string into an Inventory.
func InventoryFromString(s string) (*Inventory, error) {
	inv := NewInventory()
	s = strings.Trim(s, "()")
	if s == "" {
		return inv, nil
	}
	parts := strings.Split(s, ",")
	for _, part := range parts {
		part = strings.TrimSpace(part)
		pos, err := PositionFromString(part)
		if err != nil {
			return nil, err
		}
		inv.AddPosition(pos)
	}
	return inv, nil
}
