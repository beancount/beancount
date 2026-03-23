package core

import (
	"fmt"
	"sort"
	"time"

	"github.com/ericlagergren/decimal"
)

// PricePoint represents a price at a specific date.
type PricePoint struct {
	Date   time.Time
	Number Decimal
}

// PriceMap is a map of (base, quote) currency pairs to a list of price points.
type PriceMap map[[2]string][]PricePoint

// Add adds a price point to the map.
func (pm PriceMap) Add(baseCurrency string, date time.Time, amount Amount) {
	bq := [2]string{baseCurrency, amount.Currency}
	pm[bq] = append(pm[bq], PricePoint{
		Date:   date,
		Number: amount.Number,
	})

	// Also add inverse
	if amount.Number.Sign() != 0 {
		invBQ := [2]string{amount.Currency, baseCurrency}
		invNum := new(decimal.Big)
		DefaultContext.Quo(invNum, ONE, amount.Number)
		pm[invBQ] = append(pm[invBQ], PricePoint{
			Date:   date,
			Number: invNum,
		})
	}
}

// GetPrice returns the price as of the given date.
func (pm PriceMap) GetPrice(base, quote string, date time.Time) (Amount, error) {
	if base == quote {
		return NewAmount(ONE, quote), nil
	}

	bq := [2]string{base, quote}
	points, ok := pm[bq]
	if !ok {
		return Amount{}, fmt.Errorf("no price found for %s/%s", base, quote)
	}

	if date.IsZero() {
		if len(points) == 0 {
			return Amount{}, fmt.Errorf("no price found for %s/%s", base, quote)
		}
		p := points[len(points)-1]
		return NewAmount(p.Number, quote), nil
	}

	// Binary search for the latest price <= date.
	idx := sort.Search(len(points), func(i int) bool {
		return points[i].Date.After(date)
	})

	if idx == 0 {
		return Amount{}, fmt.Errorf("no price found for %s/%s as of %s", base, quote, date.Format("2006-01-02"))
	}
	p := points[idx-1]
	return NewAmount(p.Number, quote), nil
}

// BuildPriceMap builds a price map from a list of directives.
func BuildPriceMap(entries []Directive) PriceMap {
	priceMap := make(PriceMap)

	for _, entry := range entries {
		if p, ok := entry.(*Price); ok {
			priceMap.Add(p.Currency, p.Date, p.Amount)
		}
	}

	// For each pair, sort and uniquify.
	for bq, points := range priceMap {
		sort.Slice(points, func(i, j int) bool {
			return points[i].Date.Before(points[j].Date)
		})

		// Uniquify: keep the last one for each date.
		if len(points) > 0 {
			newPoints := []PricePoint{points[0]}
			for i := 1; i < len(points); i++ {
				if points[i].Date.Equal(newPoints[len(newPoints)-1].Date) {
					newPoints[len(newPoints)-1] = points[i]
				} else {
					newPoints = append(newPoints, points[i])
				}
			}
			priceMap[bq] = newPoints
		}
	}

	return priceMap
}


// GetLastPriceEntries returns the last price entry encountered for each (currency, cost-currency) pair.
func GetLastPriceEntries(entries []Directive, date time.Time) []Directive {
	priceEntryMap := make(map[[2]string]*Price)
	for _, entry := range entries {
		if !date.IsZero() && !entry.GetDate().Before(date) {
			break
		}
		if p, ok := entry.(*Price); ok {
			baseQuote := [2]string{p.Currency, p.Amount.Currency}
			priceEntryMap[baseQuote] = p
		}
	}

	var result []Directive
	for _, p := range priceEntryMap {
		result = append(result, p)
	}
	SortDirectives(result)
	return result
}

// Project projects all prices with a quote currency to another quote currency.
func Project(origPriceMap PriceMap, fromCurrency, toCurrency string, baseCurrencies map[string]bool) PriceMap {
	if fromCurrency == toCurrency {
		return origPriceMap
	}

	// Clone the original map.
	priceMap := make(PriceMap)
	for k, v := range origPriceMap {
		priceMap[k] = append([]PricePoint(nil), v...)
	}

	for bq, points := range origPriceMap {
		base, quote := bq[0], bq[1]
		if quote != fromCurrency {
			continue
		}
		if baseCurrencies != nil && !baseCurrencies[base] {
			continue
		}

		targetBQ := [2]string{base, toCurrency}
		existingDates := make(map[time.Time]bool)
		for _, p := range priceMap[targetBQ] {
			existingDates[p.Date] = true
		}

		var newProjected []PricePoint
		for _, p := range points {
			rateAmt, err := priceMap.GetPrice(fromCurrency, toCurrency, p.Date)
			if err != nil {
				continue
			}
			rate := rateAmt.Number

			newPrice := new(decimal.Big)
			DefaultContext.Mul(newPrice, p.Number, rate)
			newProjected = append(newProjected, PricePoint{Date: p.Date, Number: newPrice})
		}

		if len(newProjected) > 0 {
			projected := append(priceMap[targetBQ], newProjected...)
			sort.Slice(projected, func(i, j int) bool {
				return projected[i].Date.Before(projected[j].Date)
			})
			priceMap[targetBQ] = projected

			// Inverted
			invTargetBQ := [2]string{toCurrency, base}
			var invNewProjected []PricePoint
			for _, p := range newProjected {
				if p.Number.Sign() != 0 {
					invNum := new(decimal.Big)
					DefaultContext.Quo(invNum, ONE, p.Number)
					invNewProjected = append(invNewProjected, PricePoint{Date: p.Date, Number: invNum})
				}
			}
			inverted := append(priceMap[invTargetBQ], invNewProjected...)
			sort.Slice(inverted, func(i, j int) bool {
				return inverted[i].Date.Before(inverted[j].Date)
			})
			priceMap[invTargetBQ] = inverted
		}
	}

	return priceMap
}
