package ops

import (
	"fmt"
	"sort"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
	"github.com/ericlagergren/decimal"
)

// BalanceError represents an error from a balance check.
type BalanceError struct {
	Source  core.Meta
	Message string
	Entry   *core.Balance
	Diff    core.Amount
}

func (e BalanceError) Error() string {
	filename, _ := e.Source["filename"].(string)
	lineno, _ := e.Source["lineno"].(int)
	return fmt.Sprintf("%s:%d: %s", filename, lineno, e.Message)
}

// CheckBalances verifies that balance directives match the accumulated inventory.
func CheckBalances(directives []core.Directive, options *parser.Options) []error {
	var errors []error

	// Group directives by account to maintain running balances.
	accountEntries := make(map[string][]interface{})
	for _, d := range directives {
		switch e := d.(type) {
		case *core.Transaction:
			for i := range e.Postings {
				p := &e.Postings[i]
				accountEntries[p.Account] = append(accountEntries[p.Account], core.TxnPosting{Txn: e, Posting: p})
			}
		case *core.Balance:
			accountEntries[e.Account] = append(accountEntries[e.Account], e)
		}
	}

	for accountName, entries := range accountEntries {
		// Sort entries by date, type order, and lineno.
		sort.Slice(entries, func(i, j int) bool {
			var di, dj core.Directive
			switch ei := entries[i].(type) {
			case core.TxnPosting:
				di = ei.Txn
			case core.Directive:
				di = ei
			}
			switch ej := entries[j].(type) {
			case core.TxnPosting:
				dj = ej.Txn
			case core.Directive:
				dj = ej
			}
			ki := core.GetDirectiveSortKey(di)
			kj := core.GetDirectiveSortKey(dj)
			return core.CompareDirectiveSortKeys(ki, kj) < 0
		})

		runningInventory := core.NewInventory()
		for _, entry := range entries {
			switch e := entry.(type) {
			case core.TxnPosting:
				runningInventory.AddPosition(core.Position{Units: e.Posting.Units, Cost: e.Posting.Cost})
			case *core.Balance:
				if e.Account != accountName {
					continue
				}

				expectedAmount := e.Amount
				balanceAmount := runningInventory.GetCurrencyUnits(expectedAmount.Currency)

				diffNumber := new(decimal.Big)
				core.DefaultContext.Sub(diffNumber, expectedAmount.Number, balanceAmount.Number)

				tolerance := GetBalanceTolerance(e, options)

				absDiff := new(decimal.Big)
				core.DefaultContext.Abs(absDiff, diffNumber)

				if absDiff.Cmp(tolerance) > 0 {
					errors = append(errors, BalanceError{
						Source:  e.Meta,
						Message: fmt.Sprintf("Balance check failed for %s: expected %v, got %v (diff: %v)", accountName, expectedAmount, balanceAmount, diffNumber),
						Entry:   e,
						Diff:    core.Amount{Number: diffNumber, Currency: expectedAmount.Currency},
					})
				}
			}
		}
	}

	return errors
}
