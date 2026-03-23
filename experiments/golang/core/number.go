package core

import (
	"fmt"
	"math/big"
	"strings"

	"github.com/ericlagergren/decimal"
)

// Decimal is a type alias for *decimal.Big to maintain consistency with the
// Python implementation's Decimal type.
type Decimal = *decimal.Big

var (
	// ZERO is a constant for the decimal value 0.
	ZERO = NewDecimal(0)
	// ONE is a constant for the decimal value 1.
	ONE = NewDecimal(1)
	// TEN is a constant for the decimal value 10.
	TEN = NewDecimal(10)

	// DefaultContext is used for decimal operations.
	DefaultContext = decimal.ContextUnlimited

	// MISSING is a sentinel value for missing numbers.
	MISSING = new(decimal.Big)
)

// NewDecimal creates a new Decimal from an int64.
func NewDecimal(i int64) Decimal {
	d := decimal.New(i, 0)
	d.Context = DefaultContext
	return d
}

// D converts a string into a Decimal object, handling commas and spaces.
func D(s string) Decimal {
	s = strings.TrimSpace(s)
	// Handle cases like "- 122.34"
	if strings.HasPrefix(s, "- ") {
		s = "-" + strings.TrimSpace(s[1:])
	} else if strings.HasPrefix(s, "+ ") {
		s = strings.TrimSpace(s[1:])
	}

	s = strings.ReplaceAll(s, ",", "")
	// Handle cases where some other garbage might be at the end, like "#"
	if idx := strings.IndexAny(s, " #"); idx != -1 {
		s = s[:idx]
	}

	if s == "" {
		d := new(decimal.Big)
		d.Context = DefaultContext
		return d
	}
	d := new(decimal.Big)
	d.Context = DefaultContext
	d, ok := d.SetString(s)
	if !ok {
		// Try one more thing: if it's just a sign
		if s == "-" || s == "+" {
			return NewDecimal(0)
		}
		panic("Impossible to create Decimal instance from " + s)
	}
	return d
}

// Format returns a string representation of the decimal.
func Format(d Decimal) string {
	if d == nil {
		return ""
	}
	// Normalize the decimal by removing non-significant trailing zeros.
	dNormalized := new(decimal.Big).Set(d)
	dNormalized.Reduce()
	
	// %f format in ericlagergren/decimal respects the scale and avoids scientific notation.
	return fmt.Sprintf("%f", dNormalized)
}

// RoundTo rounds a number down to a particular increment.
func RoundTo(number Decimal, increment Decimal) Decimal {
	// return int(number / increment) * increment
	res := new(decimal.Big)
	res.Context = DefaultContext
	DefaultContext.QuoInt(res, number, increment)
	DefaultContext.Mul(res, res, increment)
	return res
}

// SameSign returns true if both numbers have the same sign.
func SameSign(d1, d2 Decimal) bool {
	return (d1.Sign() >= 0) == (d2.Sign() >= 0)
}

// NumFractionalDigits returns the number of fractional digits.
func NumFractionalDigits(d Decimal) int {
	return d.Scale()
}

// AutoQuantizedExponent automatically infers the exponent that would be used below a given threshold.
func AutoQuantizedExponent(d Decimal, threshold float64) int {
	dNormalized := new(decimal.Big)
	DefaultContext.Set(dNormalized, d)
	DefaultContext.Reduce(dNormalized)
	scale := dNormalized.Scale()

	// Get unscaled value (coefficient)
	var coeff big.Int
	if m, ok := dNormalized.Mantissa(); ok {
		coeff.SetUint64(m)
	} else {
		// Too large for uint64, use Raw
		_, unscaled := decimal.Raw(dNormalized)
		if unscaled != nil {
			coeff.Set(unscaled)
		}
	}

	s := coeff.String()
	if coeff.Sign() == 0 {
		return 0
	}
	// norm = dNormalized shifted so that it's in [0, 1)
	norm := new(decimal.Big).SetBigMantScale(&coeff, len(s))
	norm.Context = DefaultContext

	low := new(decimal.Big)
	low.Context = DefaultContext
	low.SetFloat64(threshold)

	high := new(decimal.Big)
	high.Context = DefaultContext
	high.SetFloat64(1.0)
	DefaultContext.Sub(high, high, low)

	iterations := 0
	for norm.Sign() != 0 {
		if norm.Cmp(low) < 0 || norm.Cmp(high) > 0 {
			break
		}
		// norm = (norm * 10) % 1
		DefaultContext.Mul(norm, norm, TEN)
		intPart := norm.Int(nil)
		intPartDec := new(decimal.Big).SetBigMantScale(intPart, 0)
		intPartDec.Context = DefaultContext
		DefaultContext.Sub(norm, norm, intPartDec)
		iterations++
	}
	res := -scale + len(s) - iterations
	return res
}

// AutoQuantize automatically quantize the number at a given threshold.
func AutoQuantize(d Decimal, threshold float64) Decimal {
	exponent := AutoQuantizedExponent(d, threshold)
	if exponent != -d.Scale() {
		qnumber := new(decimal.Big)
		DefaultContext.Set(qnumber, d)
		DefaultContext.Quantize(qnumber, -exponent)
		DefaultContext.Reduce(qnumber)
		return qnumber
	}
	return d
}

// InferQuantumFromList given a list of numbers, infer the common quantization.
func InferQuantumFromList(numbers []Decimal, threshold float64) int {
	if len(numbers) == 0 {
		return 0
	}
	minExp := 1000 // Start with a large value
	for _, n := range numbers {
		q := AutoQuantize(n, threshold)
		exp := -q.Scale()
		if exp < minExp {
			minExp = exp
		}
	}
	return minExp
}
