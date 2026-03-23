package ops

import (
	"fmt"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
	"github.com/ericlagergren/decimal"
)

// ValidationError represents an error from one of the checks.
type ValidationError struct {
	Source  core.Meta
	Message string
	Entry   core.Directive
}

func (e ValidationError) Error() string {
	filename, _ := e.Source["filename"].(string)
	lineno, _ := e.Source["lineno"].(int)
	return fmt.Sprintf("%s:%d: %s", filename, lineno, e.Message)
}

// Validate performs all the standard checks on parsed contents.
func Validate(directives []core.Directive, options *parser.Options) []error {
	var errors []error

	errors = append(errors, validateOpenClose(directives)...)
	errors = append(errors, validateDuplicateBalances(directives)...)
	errors = append(errors, validateActiveAccounts(directives)...)
	errors = append(errors, validateCurrencyConstraints(directives)...)
	errors = append(errors, validateTransactionBalances(directives, options)...)

	return errors
}

func validateOpenClose(directives []core.Directive) []error {
	var errors []error
	openMap := make(map[string]*core.Open)
	closeMap := make(map[string]*core.Close)

	for _, d := range directives {
		switch e := d.(type) {
		case *core.Open:
			if _, ok := openMap[e.Account]; ok {
				errors = append(errors, ValidationError{
					Source:  e.Meta,
					Message: fmt.Sprintf("Duplicate open directive for %s", e.Account),
					Entry:   e,
				})
			}
			openMap[e.Account] = e
		case *core.Close:
			if _, ok := closeMap[e.Account]; ok {
				errors = append(errors, ValidationError{
					Source:  e.Meta,
					Message: fmt.Sprintf("Duplicate close directive for %s", e.Account),
					Entry:   e,
				})
			}
			closeMap[e.Account] = e
		}
	}
	return errors
}

func validateDuplicateBalances(directives []core.Directive) []error {
	var errors []error
	seen := make(map[string]map[string]bool) // date -> account -> seen

	for _, d := range directives {
		if b, ok := d.(*core.Balance); ok {
			dateStr := b.Date.Format("2006-01-02")
			if _, ok := seen[dateStr]; !ok {
				seen[dateStr] = make(map[string]bool)
			}
			if seen[dateStr][b.Account] {
				// Beancount allows multiple balances on same day if they are for different currencies.
				// For now, let's just allow it or implement a more refined check.
			}
			seen[dateStr][b.Account] = true
		}
	}
	return errors
}

func validateActiveAccounts(directives []core.Directive) []error {
	var errors []error
	openMap := make(map[string]*core.Open)
	closeMap := make(map[string]*core.Close)

	for _, d := range directives {
		switch e := d.(type) {
		case *core.Open:
			openMap[e.Account] = e
		case *core.Close:
			closeMap[e.Account] = e
		}
	}

	for _, d := range directives {
		if txn, ok := d.(*core.Transaction); ok {
			for _, p := range txn.Postings {
				if _, ok := openMap[p.Account]; !ok {
					errors = append(errors, ValidationError{
						Source:  txn.Meta,
						Message: fmt.Sprintf("Account %s is not open", p.Account),
						Entry:   txn,
					})
				}
				if cl, ok := closeMap[p.Account]; ok && txn.Date.After(cl.Date) {
					errors = append(errors, ValidationError{
						Source:  txn.Meta,
						Message: fmt.Sprintf("Account %s is closed on %s", p.Account, cl.Date.Format("2006-01-02")),
						Entry:   txn,
					})
				}
			}
		}
	}
	return errors
}

func validateCurrencyConstraints(directives []core.Directive) []error {
	var errors []error
	openMap := make(map[string]*core.Open)
	for _, d := range directives {
		if e, ok := d.(*core.Open); ok && len(e.Currencies) > 0 {
			openMap[e.Account] = e
		}
	}

	for _, d := range directives {
		if txn, ok := d.(*core.Transaction); ok {
			for _, p := range txn.Postings {
				if openEntry, ok := openMap[p.Account]; ok {
					found := false
					for _, c := range openEntry.Currencies {
						if p.Units.Currency == c {
							found = true
							break
						}
					}
					if !found {
						errors = append(errors, ValidationError{
							Source:  txn.Meta,
							Message: fmt.Sprintf("Invalid currency %s for account '%s'", p.Units.Currency, p.Account),
							Entry:   txn,
						})
					}
				}
			}
		}
	}
	return errors
}

func validateTransactionBalances(directives []core.Directive, options *parser.Options) []error {
	var errors []error
	for _, d := range directives {
		if txn, ok := d.(*core.Transaction); ok {
			residual := computeResidual(txn.Postings)
			tolerances := inferTolerances(txn.Postings, options)
			if !residual.IsSmall(tolerances) {
				errors = append(errors, ValidationError{
					Source:  txn.Meta,
					Message: fmt.Sprintf("Transaction does not balance: %s", residual.String()),
					Entry:   txn,
				})
			}
		}
	}
	return errors
}

func computeResidual(postings []core.Posting) *core.Inventory {
	inv := core.NewInventory()
	for _, p := range postings {
		if p.Cost != nil {
			res := new(decimal.Big)
			core.DefaultContext.Mul(res, p.Units.Number, p.Cost.Number)
			weight := core.Amount{
				Number:   res,
				Currency: p.Cost.Currency,
			}
			inv.AddAmount(weight, nil)
		} else if p.Price.Number != nil {
			res := new(decimal.Big)
			core.DefaultContext.Mul(res, p.Units.Number, p.Price.Number)
			weight := core.Amount{
				Number:   res,
				Currency: p.Price.Currency,
			}
			inv.AddAmount(weight, nil)
		} else {
			inv.AddAmount(p.Units, nil)
		}
	}
	return inv
}

func inferTolerances(postings []core.Posting, options *parser.Options) map[string]core.Decimal {
	tolerances := make(map[string]core.Decimal)
	// multiplier := core.D("0.5")
	// if options != nil && options.ToleranceMultiplier != nil {
	// 	multiplier = options.ToleranceMultiplier
	// }

	for _, p := range postings {
		currency := p.Units.Currency
		if p.Cost != nil {
			currency = p.Cost.Currency
		} else if p.Price.Number != nil {
			currency = p.Price.Currency
		}

		if currency == "" {
			continue
		}

		if _, ok := tolerances[currency]; !ok {
			// Very simplified: just use a small constant for now.
			tolerances[currency] = core.D("0.005")
		}
	}
	return tolerances
}
