package core

import (
	"fmt"
	"regexp"
	"sort"
	"strings"
	"time"

	"github.com/ericlagergren/decimal"
)

var dateRegex = regexp.MustCompile(`^[0-9]{4}-[0-9]{2}-[0-9]{2}$`)

// Position represents an amount at a particular cost.
type Position struct {
	Units Amount
	Cost  *Cost
}

// NewPosition creates a new Position.
func NewPosition(units Amount, cost *Cost) Position {
	return Position{Units: units, Cost: cost}
}

// Equals returns true if two positions are equal.
func (p Position) Equals(other Position) bool {
	if !p.Units.Equal(other.Units) {
		return false
	}
	if (p.Cost == nil) != (other.Cost == nil) {
		return false
	}
	if p.Cost != nil {
		if (p.Cost.Number == nil) != (other.Cost.Number == nil) {
			return false
		}
		if p.Cost.Number != nil && p.Cost.Number.Cmp(other.Cost.Number) != 0 {
			return false
		}
		if p.Cost.Currency != other.Cost.Currency ||
			!p.Cost.Date.Equal(other.Cost.Date) ||
			p.Cost.Label != other.Cost.Label {
			return false
		}
	}
	return true
}

// Equal is a helper for tests that takes a pointer and handles nil.
func (p Position) Equal(other *Position) bool {
	if other == nil {
		return p.Units.Number != nil && p.Units.Number.Sign() == 0
	}
	return p.Equals(*other)
}

// String returns a human-readable string representation of the position.
func (p Position) String() string {
	if p.Cost == nil {
		return p.Units.String()
	}
	return fmt.Sprintf("%s %s", p.Units.String(), p.Cost.String())
}

// Neg returns a new position with the units negated.
func (p Position) Neg() Position {
	return Position{Units: p.Units.Neg(), Cost: p.Cost}
}

// Abs returns a new position with absolute units.
func (p Position) Abs() Position {
	absUnits := p.Units
	absUnits.Number = new(decimal.Big).Abs(p.Units.Number)
	return Position{Units: absUnits, Cost: p.Cost}
}

// Mul returns a new position with units scaled.
func (p Position) Mul(factor Decimal) Position {
	mulUnits := p.Units
	res := new(decimal.Big)
	DefaultContext.Mul(res, p.Units.Number, factor)
	mulUnits.Number = res
	return Position{Units: mulUnits, Cost: p.Cost}
}

// GetUnits returns the units of the position.
func GetUnits(p Position) Amount {
	return p.Units
}

// GetCost returns the cost of the position.
func GetCost(p Position) Amount {
	if p.Cost != nil {
		return NewAmount(p.Cost.Number, p.Cost.Currency)
	}
	return p.Units
}

// GetWeight returns the weight of the position.
func GetWeight(p Position) Amount {
	if p.Cost != nil {
		res := new(decimal.Big)
		DefaultContext.Mul(res, p.Units.Number, p.Cost.Number)
		return NewAmount(res, p.Cost.Currency)
	}
	return p.Units
}

// CURRENCY_ORDER maps common currencies to their sort priority.
var CURRENCY_ORDER = map[string]int{
	"USD": 0,
	"EUR": 1,
	"JPY": 2,
	"CAD": 3,
	"GBP": 4,
	"AUD": 5,
	"NZD": 6,
	"CHF": 7,
}

// PositionSortKey returns a sort key for a position.
type PositionSortKey struct {
	OrderUnits   int
	CostNumber   string
	CostCurrency string
	UnitsNumber  string
}

func (p Position) SortKey() PositionSortKey {
	currency := p.Units.Currency
	order, ok := CURRENCY_ORDER[currency]
	if !ok {
		order = len(CURRENCY_ORDER) + len(currency)
	}

	costNumber := "0"
	costCurrency := ""
	if p.Cost != nil {
		costNumber = Format(p.Cost.Number)
		costCurrency = p.Cost.Currency
	}

	return PositionSortKey{
		OrderUnits:   order,
		CostNumber:   costNumber,
		CostCurrency: costCurrency,
		UnitsNumber:  Format(p.Units.Number),
	}
}

// LessThan returns true if this sort key is less than another.
func (k PositionSortKey) LessThan(other PositionSortKey) bool {
	if k.OrderUnits != other.OrderUnits {
		return k.OrderUnits < other.OrderUnits
	}
	if k.CostNumber != other.CostNumber {
		return k.CostNumber < other.CostNumber
	}
	if k.CostCurrency != other.CostCurrency {
		return k.CostCurrency < other.CostCurrency
	}
	return k.UnitsNumber < other.UnitsNumber
}

// SortPositions sorts a slice of positions deterministically.
func SortPositions(ps []Position) {
	sort.Slice(ps, func(i, j int) bool {
		return ps[i].SortKey().LessThan(ps[j].SortKey())
	})
}

// PositionFromString parses a string into a Position.
func PositionFromString(s string) (Position, error) {
	var costStr string
	var total bool
	if strings.Contains(s, "{{") {
		parts := strings.Split(s, "{{")
		s = strings.TrimSpace(parts[0])
		costStr = "{" + parts[1] // Use single brace for parsing internal part
		total = true
	} else if strings.Contains(s, "{") {
		parts := strings.Split(s, "{")
		s = strings.TrimSpace(parts[0])
		costStr = "{" + parts[1]
		total = false
	}

	units, err := AmountFromString(s)
	if err != nil {
		if strings.HasPrefix(strings.TrimSpace(s), "{") {
			costStr = s
			units = Amount{}
		} else {
			return Position{}, err
		}
	}

	var cost *Cost
	if costStr != "" {
		c, err := CostFromStringWithUnits(costStr, units, total)
		if err != nil {
			return Position{}, err
		}
		cost = &c
	}
	return Position{Units: units, Cost: cost}, nil
}

// CostFromString parses a string into a Cost.
func CostFromString(s string) (Cost, error) {
	total := strings.Contains(s, "{{")
	return CostFromStringWithUnits(s, Amount{}, total)
}

// CostFromStringWithUnits parses a string into a Cost, using units for commission calculation if needed.
func CostFromStringWithUnits(s string, units Amount, total bool) (Cost, error) {
	s = strings.Trim(s, "{}")
	parts := strings.Split(s, ",")
	var c Cost
	for i, part := range parts {
		part = strings.TrimSpace(part)
		if i == 0 {
			if strings.Contains(part, "#") {
				p2 := strings.Split(part, "#")
				totalAmt := D(strings.TrimSpace(p2[0]))
				if len(p2) > 1 {
					commPart := strings.TrimSpace(p2[1])
					commAmt, _ := AmountFromString(commPart)
					if units.Number != nil && units.Number.Sign() != 0 {
						num := new(decimal.Big).Set(totalAmt)
						DefaultContext.Add(num, num, commAmt.Number)
						DefaultContext.Quo(num, num, new(decimal.Big).Abs(units.Number))
						c.Number = num
						c.Currency = commAmt.Currency
					}
				} else {
					if units.Number != nil && units.Number.Sign() != 0 {
						num := new(decimal.Big)
						DefaultContext.Quo(num, totalAmt, new(decimal.Big).Abs(units.Number))
						c.Number = num
					}
				}
			} else {
				p2 := strings.Fields(part)
				if len(p2) > 0 {
					if firstChar := p2[0][0]; (firstChar >= '0' && firstChar <= '9') || firstChar == '-' || firstChar == '+' {
						num := D(p2[0])
						if total && units.Number != nil && units.Number.Sign() != 0 {
							num2 := new(decimal.Big)
							DefaultContext.Quo(num2, num, new(decimal.Big).Abs(units.Number))
							num = num2
						}
						c.Number = num
						if len(p2) > 1 {
							c.Currency = p2[1]
						}
					} else {
						if strings.HasPrefix(p2[0], "\"") {
							c.Label = strings.Trim(p2[0], "\"")
						}
					}
				}
			}
		} else if dateRegex.MatchString(part) {
			t, _ := time.Parse("2006-01-02", part)
			c.Date = t
		} else if strings.HasPrefix(part, "\"") {
			c.Label = strings.Trim(part, "\"")
		} else {
			if c.Currency == "" {
				c.Currency = part
			}
		}
	}
	return c, nil
}
