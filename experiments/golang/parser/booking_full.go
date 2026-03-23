package parser

import (
	"fmt"
	"sort"

	"github.com/beancount/beancount/v3/core"
	"github.com/ericlagergren/decimal"
)

// ReductionError represents an error during inventory reduction.
type ReductionError struct {
	Source  core.Meta
	Message string
	Entry   *core.Transaction
}

func (e ReductionError) Error() string {
	filename, _ := e.Source["filename"].(string)
	lineno, _ := e.Source["lineno"].(int)
	return fmt.Sprintf("%s:%d: %s", filename, lineno, e.Message)
}

// CategorizationError represents an error during currency categorization.
type CategorizationError struct {
	Source  core.Meta
	Message string
	Entry   *core.Transaction
}

func (e CategorizationError) Error() string {
	filename, _ := e.Source["filename"].(string)
	lineno, _ := e.Source["lineno"].(int)
	return fmt.Sprintf("%s:%d: %s", filename, lineno, e.Message)
}

// Refer represents a reference to a posting within a transaction.
type Refer struct {
	Index         int
	UnitsCurrency string
	CostCurrency  string
	PriceCurrency string
	HasCost       bool
	HasPrice      bool
}

// GroupEntry represents a currency group and its referring postings.
type GroupEntry struct {
	Currency string
	Refers   []Refer
}

func getBucketCurrency(refer Refer) string {
	if refer.CostCurrency != "" {
		return refer.CostCurrency
	}
	if refer.PriceCurrency != "" {
		return refer.PriceCurrency
	}
	if !refer.HasCost && !refer.HasPrice {
		return refer.UnitsCurrency
	}
	return ""
}

// CategorizeByCurrency groups postings by their weight currency.
func CategorizeByCurrency(
	entry *core.Transaction,
	balances map[string]*core.Inventory,
) ([]GroupEntry, []error) {
	var errors []error
	
	groups := make(map[string][]Refer)
	sortdict := make(map[string]int)
	var autoPostings []Refer
	var unknown []Refer
	allCurrencies := make(map[string]bool)

	for i, p := range entry.Postings {
		unitsCcy := p.Units.Currency
		var costCcy string
		var hasCost bool
		if p.Cost != nil {
			costCcy = p.Cost.Currency
			hasCost = true
		} else if p.CostSpec != nil {
			costCcy = p.CostSpec.Currency
			hasCost = true
		}
		var priceCcy string
		var hasPrice bool
		if p.Price.Number != nil {
			priceCcy = p.Price.Currency
			hasPrice = true
		}

		if hasCost && costCcy == "" && priceCcy != "" {
			costCcy = priceCcy
		} else if hasPrice && priceCcy == "" && costCcy != "" {
			priceCcy = costCcy
		}

		refer := Refer{i, unitsCcy, costCcy, priceCcy, hasCost, hasPrice}

		if p.Units.Number == core.MISSING && !hasPrice {
			autoPostings = append(autoPostings, refer)
		} else {
			currency := getBucketCurrency(refer)
			if currency != "" {
				if _, ok := sortdict[currency]; !ok {
					sortdict[currency] = i
				}
				groups[currency] = append(groups[currency], refer)
				allCurrencies[currency] = true
			} else {
				unknown = append(unknown, refer)
			}
			if unitsCcy != "" { allCurrencies[unitsCcy] = true }
		}
	}

	if len(unknown) == 1 && len(groups) == 1 {
		r := unknown[0]
		var otherCurrency string
		for c := range groups { otherCurrency = c }
		
		if !r.HasPrice && !r.HasCost {
			r.UnitsCurrency = otherCurrency
		} else {
			if r.HasPrice && r.PriceCurrency == "" { r.PriceCurrency = otherCurrency }
			if r.HasCost && r.CostCurrency == "" { r.CostCurrency = otherCurrency }
		}
		
		currency := getBucketCurrency(r)
		if currency != "" {
			if _, ok := sortdict[currency]; !ok {
				sortdict[currency] = r.Index
			}
			groups[currency] = append(groups[currency], r)
			unknown = nil
		}
	}

	var stillUnknown []Refer
	for _, r := range unknown {
		p := entry.Postings[r.Index]
		balance, ok := balances[p.Account]
		if !ok {
			balance = core.NewInventory()
		}

		if r.UnitsCurrency == "" {
			ccys := balance.Currencies()
			if len(ccys) == 1 {
				for c := range ccys { r.UnitsCurrency = c }
			}
		}

		if (r.HasCost && r.CostCurrency == "") || (r.HasPrice && r.PriceCurrency == "") {
			costCcys := balance.CostCurrencies()
			if len(costCcys) == 1 {
				var c string
				for k := range costCcys { c = k }
				if r.CostCurrency == "" { r.CostCurrency = c }
				if r.PriceCurrency == "" { r.PriceCurrency = c }
			}
		}

		currency := getBucketCurrency(r)
		if currency != "" {
			if _, ok := sortdict[currency]; !ok {
				sortdict[currency] = r.Index
			}
			groups[currency] = append(groups[currency], r)
		} else {
			stillUnknown = append(stillUnknown, r)
		}
	}

	for _, r := range stillUnknown {
		errors = append(errors, CategorizationError{
			Source:  entry.Postings[r.Index].Meta,
			Message: fmt.Sprintf("Failed to categorize posting %d", r.Index+1),
			Entry:   entry,
		})
	}

	for _, refers := range groups {
		for i, r := range refers {
			if r.UnitsCurrency == "" {
				p := entry.Postings[r.Index]
				if balance, ok := balances[p.Account]; ok {
					ccys := balance.Currencies()
					if len(ccys) == 1 {
						for c := range ccys {
							r.UnitsCurrency = c
							refers[i] = r
						}
					}
				}
			}
		}
	}

	if len(autoPostings) > 1 {
		errors = append(errors, CategorizationError{
			Source:  entry.Postings[autoPostings[len(autoPostings)-1].Index].Meta,
			Message: "You may not have more than one auto-posting per currency",
			Entry:   entry,
		})
		autoPostings = autoPostings[:1]
	}
	for _, r := range autoPostings {
		for currency := range groups {
			if _, ok := sortdict[currency]; !ok {
				sortdict[currency] = r.Index
			}
			groups[currency] = append(groups[currency], Refer{
				Index:         r.Index,
				UnitsCurrency: currency,
				CostCurrency:  "",
				PriceCurrency: "",
				HasCost:       false,
				HasPrice:      false,
			})
		}
	}

	for _, refersList := range groups {
		for _, r := range refersList {
			p := entry.Postings[r.Index]
			if r.UnitsCurrency == "" {
				errors = append(errors, CategorizationError{Source: p.Meta, Message: "Could not resolve units currency", Entry: entry})
			}
		}
	}

	var sortedGroups []GroupEntry
	for c, r := range groups {
		sortedGroups = append(sortedGroups, GroupEntry{c, r})
	}
	sort.Slice(sortedGroups, func(i, j int) bool {
		return sortdict[sortedGroups[i].Currency] < sortdict[sortedGroups[j].Currency]
	})

	return sortedGroups, errors
}

// ReplaceCurrencies replaces resolved currencies in the entry's postings.
func ReplaceCurrencies(postings []core.Posting, groups []GroupEntry) []struct {
	Currency string
	Postings []core.Posting
} {
	var result []struct {
		Currency string
		Postings []core.Posting
	}

	for _, g := range groups {
		var newPostings []core.Posting
		sort.Slice(g.Refers, func(i, j int) bool {
			return g.Refers[i].Index < g.Refers[j].Index
		})

		for _, refer := range g.Refers {
			p := postings[refer.Index]
			if p.Units.Number == core.MISSING && p.Units.Currency == "" {
				p.Units.Currency = refer.UnitsCurrency
			} else {
				if p.Units.Currency == "" {
					p.Units.Currency = refer.UnitsCurrency
				}
				if p.Cost != nil && p.Cost.Currency == "" {
					p.Cost.Currency = refer.CostCurrency
				}
				if p.CostSpec != nil && p.CostSpec.Currency == "" {
					p.CostSpec.Currency = refer.CostCurrency
				}
				if p.Price.Number != nil && p.Price.Currency == "" {
					p.Price.Currency = refer.PriceCurrency
				}
			}
			newPostings = append(newPostings, p)
		}
		result = append(result, struct {
			Currency string
			Postings []core.Posting
		}{g.Currency, newPostings})
	}

	return result
}

// BookReductions identifies postings that reduce an existing inventory and matches them to specific lots.
func BookReductions(
	txn *core.Transaction,
	postings []core.Posting,
	balances map[string]*core.Inventory,
	methods map[string]core.Booking,
	defaultMethod core.Booking,
) ([]core.Posting, []error) {
	var errors []error
	var bookedPostings []core.Posting

	localBalances := make(map[string]*core.Inventory)

	for _, p := range postings {
		account := p.Account
		method, ok := methods[account]
		if !ok {
			method = defaultMethod
		}

		if _, ok := localBalances[account]; !ok {
			if inv, ok := balances[account]; ok {
				localBalances[account] = inv.Clone()
			} else {
				localBalances[account] = core.NewInventory()
			}
		}
		balance := localBalances[account]

		if p.CostSpec != nil && p.Units.Number != core.MISSING && method != core.BookingNone && balance.IsReducedBy(p.Units) {
			costNumber := ComputeCostNumber(p.CostSpec, p.Units)
			
			var matches []core.Position
			for _, pos := range balance.GetPositions() {
				if pos.Units.Currency != p.Units.Currency {
					continue
				}
				if core.SameSign(pos.Units.Number, p.Units.Number) {
					continue
				}
				if pos.Cost == nil {
					continue
				}
				
				if costNumber != nil && pos.Cost.Number.Cmp(costNumber) != 0 {
					continue
				}
				if p.CostSpec.Currency != "" && pos.Cost.Currency != p.CostSpec.Currency {
					continue
				}
				if !p.CostSpec.Date.IsZero() && !pos.Cost.Date.Equal(p.CostSpec.Date) {
					continue
				}
				if p.CostSpec.Label != "" && pos.Cost.Label != p.CostSpec.Label {
					continue
				}
				
				matches = append(matches, pos)
			}

			if len(matches) == 0 {
				errors = append(errors, ReductionError{
					Source:  p.Meta,
					Message: fmt.Sprintf("No position matches %v against balance %v", p, balance),
					Entry:   txn,
				})
				bookedPostings = append(bookedPostings, p)
				continue
			}

			reductionPostings, errs := handleAmbiguousMatches(txn, p, matches, method)
			if len(errs) > 0 {
				errors = append(errors, errs...)
				if len(reductionPostings) == 0 {
					bookedPostings = append(bookedPostings, p)
					continue
				}
			}

			bookedPostings = append(bookedPostings, reductionPostings...)
			for _, rp := range reductionPostings {
				balance.AddPosition(core.Position{Units: rp.Units, Cost: rp.Cost})
			}
		} else {
			if p.CostSpec != nil && p.CostSpec.Date.IsZero() {
				p.CostSpec.Date = txn.Date
			}
			bookedPostings = append(bookedPostings, p)
		}
	}

	return bookedPostings, errors
}

func ComputeCostNumber(spec *core.CostSpec, units core.Amount) core.Decimal {
	if spec.NumberPer == core.MISSING || spec.NumberTotal == core.MISSING {
		return nil
	}
	
	if spec.NumberTotal != nil {
		total := new(decimal.Big).Set(spec.NumberTotal)
		absUnits := new(decimal.Big).Abs(units.Number)
		if spec.NumberPer != nil {
			term := new(decimal.Big)
			core.DefaultContext.Mul(term, spec.NumberPer, absUnits)
			core.DefaultContext.Add(total, total, term)
		}
		res := new(decimal.Big)
		core.DefaultContext.Quo(res, total, absUnits)
		return res
	}
	
	return spec.NumberPer
}

func handleAmbiguousMatches(
	txn *core.Transaction,
	p core.Posting,
	matches []core.Position,
	method core.Booking,
) ([]core.Posting, []error) {
	switch method {
	case core.BookingStrict, core.BookingStrictWithSize:
		return bookingMethodStrict(txn, p, matches, method == core.BookingStrictWithSize)
	case core.BookingFIFO:
		return bookingMethodXIFO(txn, p, matches, "FIFO")
	case core.BookingLIFO:
		return bookingMethodXIFO(txn, p, matches, "LIFO")
	case core.BookingHIFO:
		return bookingMethodXIFO(txn, p, matches, "HIFO")
	default:
		return nil, []error{ReductionError{
			Source:  p.Meta,
			Message: fmt.Sprintf("Unsupported booking method: %s", method),
			Entry:   txn,
		}}
	}
}

func bookingMethodStrict(txn *core.Transaction, p core.Posting, matches []core.Position, withSize bool) ([]core.Posting, []error) {
	if len(matches) > 1 {
		if withSize {
			// Find matches with the same absolute size.
			number := new(decimal.Big).Abs(p.Units.Number)
			var sizeMatches []core.Position
			for _, m := range matches {
				if new(decimal.Big).Abs(m.Units.Number).Cmp(number) == 0 {
					sizeMatches = append(sizeMatches, m)
				}
			}
			if len(sizeMatches) > 0 {
				// If there are one or more matching size lots, accept the oldest lot.
				sort.Slice(sizeMatches, func(i, j int) bool {
					return sizeMatches[i].Cost.Date.Before(sizeMatches[j].Cost.Date)
				})
				match := sizeMatches[0]
				rp := p
				rp.Units = match.Units.Neg()
				rp.Cost = match.Cost
				rp.CostSpec = nil
				return []core.Posting{rp}, nil
			}
		}

		sumMatches := new(decimal.Big)
		for _, m := range matches {
			core.DefaultContext.Add(sumMatches, sumMatches, m.Units.Number)
		}
		
		negUnits := new(decimal.Big).Neg(p.Units.Number)
		if sumMatches.Cmp(negUnits) == 0 {
			var reductions []core.Posting
			for _, m := range matches {
				rp := p
				rp.Units = m.Units.Neg()
				rp.Cost = m.Cost
				rp.CostSpec = nil
				reductions = append(reductions, rp)
			}
			return reductions, nil
		}
		
		return nil, []error{ReductionError{
			Source:  p.Meta,
			Message: fmt.Sprintf("Ambiguous matches for %v: %v", p, matches),
			Entry:   txn,
		}}
	}
	
	match := matches[0]
	if withSize {
		if new(decimal.Big).Abs(match.Units.Number).Cmp(new(decimal.Big).Abs(p.Units.Number)) != 0 {
			return nil, []error{ReductionError{
				Source:  p.Meta,
				Message: fmt.Sprintf("Strict with size failed: expected %v, got %v", p.Units, match.Units),
				Entry:   txn,
			}}
		}
	}

	sign := 1
	if p.Units.Number.Sign() < 0 {
		sign = -1
	}
	
	absMatchUnits := new(decimal.Big).Abs(match.Units.Number)
	absPostingUnits := new(decimal.Big).Abs(p.Units.Number)
	
	var num *decimal.Big
	if absMatchUnits.Cmp(absPostingUnits) < 0 {
		num = absMatchUnits
	} else {
		num = absPostingUnits
	}
	
	reductionUnits := core.NewAmount(new(decimal.Big).Mul(num, decimal.New(int64(sign), 0)), match.Units.Currency)
	
	rp := p
	rp.Units = reductionUnits
	rp.Cost = match.Cost
	rp.CostSpec = nil
	
	var errs []error
	if reductionUnits.Number.Cmp(p.Units.Number) != 0 {
		errs = append(errs, ReductionError{
			Source:  p.Meta,
			Message: fmt.Sprintf("Not enough lots to reduce %v", p),
			Entry:   txn,
		})
	}
	
	return []core.Posting{rp}, errs
}

func bookingMethodXIFO(txn *core.Transaction, p core.Posting, matches []core.Position, method string) ([]core.Posting, []error) {
	sort.Slice(matches, func(i, j int) bool {
		switch method {
		case "FIFO":
			return matches[i].Cost.Date.Before(matches[j].Cost.Date)
		case "LIFO":
			return matches[i].Cost.Date.After(matches[j].Cost.Date)
		case "HIFO":
			if c := matches[i].Cost.Number.Cmp(matches[j].Cost.Number); c != 0 {
				return c > 0
			}
			return matches[i].Cost.Date.Before(matches[j].Cost.Date)
		default:
			return false
		}
	})
	
	sign := 1
	if p.Units.Number.Sign() < 0 {
		sign = -1
	}
	
	remaining := new(decimal.Big).Abs(p.Units.Number)
	var reductions []core.Posting
	
	for _, match := range matches {
		if remaining.Sign() <= 0 {
			break
		}
		
		if core.SameSign(match.Units.Number, p.Units.Number) {
			continue
		}
		
		absMatchUnits := new(decimal.Big).Abs(match.Units.Number)
		size := new(decimal.Big)
		if absMatchUnits.Cmp(remaining) < 0 {
			size.Set(absMatchUnits)
		} else {
			size.Set(remaining)
		}
		
		rp := p
		rp.Units = core.NewAmount(new(decimal.Big).Mul(size, decimal.New(int64(sign), 0)), match.Units.Currency)
		rp.Cost = match.Cost
		rp.CostSpec = nil
		reductions = append(reductions, rp)
		
		core.DefaultContext.Sub(remaining, remaining, size)
	}
	
	var errs []error
	if remaining.Sign() > 0 {
		errs = append(errs, ReductionError{
			Source:  p.Meta,
			Message: fmt.Sprintf("Not enough lots to reduce %v", p),
			Entry:   txn,
		})
	}
	
	return reductions, errs
}
