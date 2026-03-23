package parser

import (
	"fmt"

	"github.com/beancount/beancount/v3/core"
	"github.com/ericlagergren/decimal"
)

// BookingError represents an error during the booking process.
type BookingError struct {
	Source  core.Meta
	Message string
	Entry   *core.Transaction
}

func (e BookingError) Error() string {
	filename, _ := e.Source["filename"].(string)
	lineno, _ := e.Source["lineno"].(int)
	return fmt.Sprintf("%s:%d: %s", filename, lineno, e.Message)
}

// Book completes all positions with incomplete numbers in the given entries.
func Book(incompleteEntries []core.Directive, options *Options) ([]core.Directive, []error) {
	var completeEntries []core.Directive
	var errors []error

	balances := make(map[string]*core.Inventory)
	methods := make(map[string]core.Booking)
	
	defaultMethod := core.BookingStrict
	if options != nil {
		defaultMethod = options.BookingMethod
	}

	for _, entry := range incompleteEntries {
		switch e := entry.(type) {
		case *core.Open:
			method := e.Booking
			if method == "" {
				method = defaultMethod
			}
			methods[e.Account] = method
			completeEntries = append(completeEntries, e)
		case *core.Transaction:
			bookedTxn, errs := BookTransaction(e, balances, methods, options)
			if len(errs) > 0 {
				errors = append(errors, errs...)
			}
			
			// Update balances with final postings.
			for _, p := range bookedTxn.Postings {
				if _, ok := balances[p.Account]; !ok {
					balances[p.Account] = core.NewInventory()
				}
				// Ensure CostSpec is converted to Cost for the inventory update.
				balances[p.Account].AddPosition(core.Position{Units: p.Units, Cost: p.Cost})
			}
			completeEntries = append(completeEntries, bookedTxn)
		default:
			completeEntries = append(completeEntries, entry)
		}
	}

	return completeEntries, errors
}

// BookTransaction books a single transaction.
func BookTransaction(
	entry *core.Transaction,
	balances map[string]*core.Inventory,
	methods map[string]core.Booking,
	options *Options,
) (*core.Transaction, []error) {
	var allErrors []error

	defaultMethod := core.BookingStrict
	if options != nil {
		defaultMethod = options.BookingMethod
	}

	// Clone postings to avoid modifying the original entry (especially CostSpec pointers).
	clonedPostings := make([]core.Posting, len(entry.Postings))
	for i, p := range entry.Postings {
		clonedPostings[i] = p
		if p.CostSpec != nil {
			spec := *p.CostSpec
			clonedPostings[i].CostSpec = &spec
		}
	}

	// 1. Group postings by currency.
	groups, errs := CategorizeByCurrency(entry, balances)
	if len(errs) > 0 {
		allErrors = append(allErrors, errs...)
		return entry, allErrors
	}
	postingGroups := ReplaceCurrencies(clonedPostings, groups)

	// 2. Resolve reductions and interpolate.
	var finalPostings []core.Posting
	for _, pg := range postingGroups {
		// Resolve reductions.
		bookedPostings, errs := BookReductions(entry, pg.Postings, balances, methods, defaultMethod)
		if len(errs) > 0 {
			allErrors = append(allErrors, errs...)
		}

		// Interpolate missing numbers.
		interPostings, errs, _ := InterpolateGroup(bookedPostings, pg.Currency)
		if len(errs) > 0 {
			allErrors = append(allErrors, errs...)
		}
		finalPostings = append(finalPostings, interPostings...)
	}

	bookedTxn := *entry
	bookedTxn.Postings = finalPostings
	return &bookedTxn, allErrors
}

// MissingType represents the type of missing value.
type MissingType int

const (
	MissingUnits MissingType = iota
	MissingCostPer
	MissingCostTotal
	MissingPrice
)

// InterpolateGroup handles the interpolation of a single currency group.
func InterpolateGroup(postings []core.Posting, currency string) ([]core.Posting, []error, bool) {
	var incomplete []struct {
		typ   MissingType
		index int
	}

	for i, p := range postings {
		if p.Units.Number == core.MISSING {
			incomplete = append(incomplete, struct {
				typ   MissingType
				index int
			}{MissingUnits, i})
		}
		if p.CostSpec != nil {
			if p.CostSpec.NumberPer == core.MISSING {
				incomplete = append(incomplete, struct {
					typ   MissingType
					index int
				}{MissingCostPer, i})
			}
			if p.CostSpec.NumberTotal == core.MISSING {
				incomplete = append(incomplete, struct {
					typ   MissingType
					index int
				}{MissingCostTotal, i})
			}
		}
		if p.Price.Number == core.MISSING {
			incomplete = append(incomplete, struct {
				typ   MissingType
				index int
			}{MissingPrice, i})
		}
	}

	if len(incomplete) == 0 {
		var outPostings []core.Posting
		for _, p := range postings {
			finalP := ConvertCostSpecToCost(p)
			if finalP.Units.Number.Sign() != 0 {
				outPostings = append(outPostings, finalP)
			}
		}
		return outPostings, nil, false
	}

	if len(incomplete) > 1 {
		return postings, []error{fmt.Errorf("too many missing numbers for currency group %q", currency)}, true
	}

	missing := incomplete[0]
	incompletePosting := postings[missing.index]

	tempPostings := make([]core.Posting, len(postings))
	for i, p := range postings {
		if i == missing.index {
			tempPostings[i] = p
		} else {
			tempPostings[i] = ConvertCostSpecToCost(p)
		}
	}

	// Compute residual of other postings.
	residual := core.NewInventory()
	for i, p := range tempPostings {
		if i == missing.index { continue }
		if p.Units.Number.Sign() == 0 { continue }
		if p.Cost != nil {
			res := new(decimal.Big)
			core.DefaultContext.Mul(res, p.Units.Number, p.Cost.Number)
			residual.AddAmount(core.NewAmount(res, p.Cost.Currency), nil)
		} else if p.Price.Number != nil {
			res := new(decimal.Big)
			core.DefaultContext.Mul(res, p.Units.Number, p.Price.Number)
			residual.AddAmount(core.NewAmount(res, p.Price.Currency), nil)
		} else {
			residual.AddAmount(p.Units, nil)
		}
	}

	weight := core.ZERO
	if !residual.IsEmpty() {
		units := residual.GetCurrencyUnits(currency)
		weight = new(decimal.Big).Neg(units.Number)
	}

	var interpolatedPosting *core.Posting

	switch missing.typ {
	case MissingUnits:
		p := incompletePosting
		var unitsNum *decimal.Big
		if p.CostSpec != nil {
			costPer := p.CostSpec.NumberPer
			costTotal := p.CostSpec.NumberTotal
			if costPer != nil && costPer.Sign() != 0 {
				if costTotal == nil { costTotal = core.ZERO }
				num := new(decimal.Big)
				core.DefaultContext.Sub(num, weight, costTotal)
				core.DefaultContext.Quo(num, num, costPer)
				unitsNum = num
			}
		} else if p.Price.Number != nil && p.Price.Number.Sign() != 0 {
			num := new(decimal.Big)
			core.DefaultContext.Quo(num, weight, p.Price.Number)
			unitsNum = num
		} else {
			unitsNum = weight
		}
		
		if unitsNum != nil {
			p.Units.Number = unitsNum
			if p.Units.Currency == "" { p.Units.Currency = currency }
			interpolatedPosting = &p
		}

	case MissingCostPer:
		p := incompletePosting
		if p.Units.Number.Sign() != 0 {
			total := p.CostSpec.NumberTotal
			if total == nil { total = core.ZERO }
			num := new(decimal.Big)
			core.DefaultContext.Sub(num, weight, total)
			core.DefaultContext.Quo(num, num, p.Units.Number)
			p.CostSpec.NumberPer = num
			interpolatedPosting = &p
		}

	case MissingCostTotal:
		p := incompletePosting
		term := new(decimal.Big)
		core.DefaultContext.Mul(term, p.CostSpec.NumberPer, p.Units.Number)
		num := new(decimal.Big)
		core.DefaultContext.Sub(num, weight, term)
		p.CostSpec.NumberTotal = num
		interpolatedPosting = &p

	case MissingPrice:
		p := incompletePosting
		if p.Units.Number.Sign() != 0 {
			num := new(decimal.Big)
			core.DefaultContext.Quo(num, weight, p.Units.Number)
			core.DefaultContext.Abs(num, num)
			p.Price.Number = num
			if p.Price.Currency == "" { p.Price.Currency = currency }
			interpolatedPosting = &p
		}
	}

	if interpolatedPosting != nil {
		tempPostings[missing.index] = ConvertCostSpecToCost(*interpolatedPosting)
		var outPostings []core.Posting
		for _, p := range tempPostings {
			finalP := ConvertCostSpecToCost(p)
			if finalP.Units.Number.Sign() != 0 {
				outPostings = append(outPostings, finalP)
			}
		}
		return outPostings, nil, true
	}

	var outPostings []core.Posting
	for _, p := range tempPostings {
		finalP := ConvertCostSpecToCost(p)
		if finalP.Units.Number.Sign() != 0 {
			outPostings = append(outPostings, finalP)
		}
	}
	return outPostings, nil, false
}

// ConvertCostSpecToCost converts a CostSpec to a Cost if possible.
func ConvertCostSpecToCost(p core.Posting) core.Posting {
	if p.CostSpec == nil {
		return p
	}
	
	costNumber := ComputeCostNumber(p.CostSpec, p.Units)
	if costNumber == nil {
		costNumber = core.ZERO
	}

	p.Cost = &core.Cost{
		Number:   costNumber,
		Currency: p.CostSpec.Currency,
		Date:     p.CostSpec.Date,
		Label:    p.CostSpec.Label,
	}
	p.CostSpec = nil
	return p
}
