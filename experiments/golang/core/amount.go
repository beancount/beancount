package core

import (
	"fmt"
	"strings"

	"github.com/ericlagergren/decimal"
)

// Amount represents a quantity of a particular currency.
type Amount struct {
	Number   Decimal
	Currency string
}

// NewAmount creates a new Amount.
func NewAmount(number Decimal, currency string) Amount {
	return Amount{Number: number, Currency: currency}
}

// AmountFromString parses a string into an Amount.
func AmountFromString(s string) (Amount, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return Amount{}, nil
	}
	
	// Handle cases like "- 111.2934"
	if strings.HasPrefix(s, "- ") {
		s = "-" + strings.TrimSpace(s[1:])
	} else if strings.HasPrefix(s, "+ ") {
		s = strings.TrimSpace(s[1:])
	}

	parts := strings.Fields(s)
	if len(parts) < 2 {
		return Amount{}, fmt.Errorf("invalid string for amount: %s", s)
	}
	num := D(parts[0])
	currency := parts[1]
	return Amount{Number: num, Currency: currency}, nil
}

// String returns a human-readable string representation of the amount.
func (a Amount) String() string {
	if a.Number == nil {
		return a.Currency
	}
	return Format(a.Number) + " " + a.Currency
}

// Add adds two amounts of the same currency.
func (a Amount) Add(other Amount) (Amount, error) {
	if a.Currency != other.Currency {
		return Amount{}, fmt.Errorf("unmatching currencies: %s and %s", a.Currency, other.Currency)
	}
	res := new(decimal.Big)
	DefaultContext.Add(res, a.Number, other.Number)
	return Amount{Number: res, Currency: a.Currency}, nil
}

// Sub subtracts another amount from this one.
func (a Amount) Sub(other Amount) (Amount, error) {
	if a.Currency != other.Currency {
		return Amount{}, fmt.Errorf("unmatching currencies: %s and %s", a.Currency, other.Currency)
	}
	res := new(decimal.Big)
	DefaultContext.Sub(res, a.Number, other.Number)
	return Amount{Number: res, Currency: a.Currency}, nil
}

// Mul scales an amount by a factor.
func (a Amount) Mul(factor Decimal) Amount {
	res := new(decimal.Big)
	DefaultContext.Mul(res, a.Number, factor)
	return Amount{Number: res, Currency: a.Currency}
}

// Neg returns the negated amount.
func (a Amount) Neg() Amount {
	res := new(decimal.Big).Neg(a.Number)
	return Amount{Number: res, Currency: a.Currency}
}

// Abs returns the absolute value of the amount.
func (a Amount) Abs() Amount {
	res := new(decimal.Big).Abs(a.Number)
	return Amount{Number: res, Currency: a.Currency}
}

// Equal returns true if two amounts are equal.
func (a Amount) Equal(other Amount) bool {
	return a.Currency == other.Currency && a.Number.Cmp(other.Number) == 0
}
