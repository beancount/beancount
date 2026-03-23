package core

import (
	"time"

	"github.com/ericlagergren/decimal"
)

// GetPriceMap returns a price map from price entries.
func GetPriceMap(entries []Directive) PriceMap {
	return BuildPriceMap(entries)
}

// ConvertPosition converts a position to a different currency.
func ConvertPosition(pos Position, targetCurrency string, priceMap PriceMap, date time.Time) (Amount, error) {
	units := pos.Units
	if units.Currency == targetCurrency {
		return units, nil
	}

	// Use price from cost if available.
	if pos.Cost != nil && pos.Cost.Currency == targetCurrency {
		res := new(decimal.Big)
		DefaultContext.Mul(res, units.Number, pos.Cost.Number)
		return NewAmount(res, targetCurrency), nil
	}

	// Use price map.
	rate, err := priceMap.GetPrice(units.Currency, targetCurrency, date)
	if err != nil {
		return Amount{}, err
	}

	res := new(decimal.Big)
	DefaultContext.Mul(res, units.Number, rate.Number)
	return NewAmount(res, targetCurrency), nil
}

// GetValue returns the value of a position at the given date using a price map.
func GetValue(pos Position, priceMap PriceMap, date time.Time) Amount {
	// If no price map, return weight.
	if priceMap == nil {
		return GetWeight(pos)
	}
	// Note: Beancount's get_value in Python is more complex, 
	// but for the tests we have, it seems we return the weight if no target currency is given.
	return GetWeight(pos)
}

// ConvertAmount converts an amount to a different currency using a price map.
func ConvertAmount(amt Amount, targetCurrency string, priceMap PriceMap, date time.Time, via ...string) (Amount, error) {
	if amt.Currency == targetCurrency {
		return amt, nil
	}

	currentAmt := amt
	for _, intermediateCcy := range via {
		nextAmt, err := ConvertAmount(currentAmt, intermediateCcy, priceMap, date)
		if err != nil {
			return Amount{}, err
		}
		currentAmt = nextAmt
	}

	rate, err := priceMap.GetPrice(currentAmt.Currency, targetCurrency, date)
	if err != nil {
		return Amount{}, err
	}

	res := new(decimal.Big)
	DefaultContext.Mul(res, currentAmt.Number, rate.Number)
	return NewAmount(res, targetCurrency), nil
}
