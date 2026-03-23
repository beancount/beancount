package ops

import (
	"sort"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
	"github.com/ericlagergren/decimal"
)

// PadError represents an error encountered during padding.
type PadError struct {
	Source  core.Meta
	Message string
	Entry   *core.Pad
}

func (e PadError) Error() string {
	return e.Message
}

// Pad inserts transaction entries to fulfill a subsequent balance check.
func Pad(directives []core.Directive, options *parser.Options) ([]core.Directive, []error) {
	var padErrors []error

	// Find all pad entries and group them by account.
	padDict := make(map[string][]*core.Pad)
	for _, d := range directives {
		if p, ok := d.(*core.Pad); ok {
			padDict[p.Account] = append(padDict[p.Account], p)
		}
	}

	if len(padDict) == 0 {
		return directives, nil
	}

	// Group directives by account to maintain running balances.
	accountEntries := make(map[string][]interface{})
	for _, d := range directives {
		switch e := d.(type) {
		case *core.Transaction:
			for i := range e.Postings {
				p := &e.Postings[i]
				accountEntries[p.Account] = append(accountEntries[p.Account], core.TxnPosting{Txn: e, Posting: p})
			}
		case *core.Pad:
			accountEntries[e.Account] = append(accountEntries[e.Account], e)
		case *core.Balance:
			accountEntries[e.Account] = append(accountEntries[e.Account], e)
		}
	}

	// New entries to be inserted.
	newEntriesMap := make(map[*core.Pad][]core.Directive)

	for accountName, entries := range accountEntries {
		if _, ok := padDict[accountName]; !ok {
			continue
		}

		// Sort entries by date, type order, and lineno.
		sort.Slice(entries, func(i, j int) bool {
			var di, dj core.Directive
			switch ei := entries[i].(type) {
			case core.TxnPosting: di = ei.Txn
			case core.Directive: di = ei
			}
			switch ej := entries[j].(type) {
			case core.TxnPosting: dj = ej.Txn
			case core.Directive: dj = ej
			}
			ki := core.GetDirectiveSortKey(di)
			kj := core.GetDirectiveSortKey(dj)
			return core.CompareDirectiveSortKeys(ki, kj) < 0
		})

		var activePad *core.Pad
		runningInventory := core.NewInventory()
		paddedCurrencies := make(map[string]bool)

		for _, entry := range entries {
			switch e := entry.(type) {
			case core.TxnPosting:
				runningInventory.AddPosition(core.Position{Units: e.Posting.Units, Cost: e.Posting.Cost})
			case *core.Pad:
				activePad = e
				paddedCurrencies = make(map[string]bool)
			case *core.Balance:
				expectedAmount := e.Amount
				balanceAmount := runningInventory.GetCurrencyUnits(expectedAmount.Currency)

				diffNumber := new(decimal.Big)
				core.DefaultContext.Sub(diffNumber, expectedAmount.Number, balanceAmount.Number)

				tolerance := GetBalanceTolerance(e, options)

				absDiff := new(decimal.Big)
				core.DefaultContext.Abs(absDiff, diffNumber)

				if absDiff.Cmp(tolerance) > 0 {
					if activePad != nil && !paddedCurrencies[expectedAmount.Currency] {
						// Create padding transaction.
						diffAmount := core.Amount{Number: diffNumber, Currency: expectedAmount.Currency}

						txn := &core.Transaction{
							Meta:      activePad.Meta,
							Date:      activePad.Date,
							Flag:      "P",
							Narration: "Padding for balance assertion",
							Postings: []core.Posting{
								{
									Account: activePad.Account,
									Units:   diffAmount,
								},
								{
									Account: activePad.SourceAccount,
									Units:   diffAmount.Neg(),
								},
							},
						}
						newEntriesMap[activePad] = append(newEntriesMap[activePad], txn)
						runningInventory.AddAmount(diffAmount, nil)
						paddedCurrencies[expectedAmount.Currency] = true
					}
				}
			}
		}
	}

	// Reconstruct the list of directives with new padding transactions.
	var finalDirectives []core.Directive
	for _, d := range directives {
		finalDirectives = append(finalDirectives, d)
		if p, ok := d.(*core.Pad); ok {
			if txns, ok := newEntriesMap[p]; ok {
				finalDirectives = append(finalDirectives, txns...)
			}
		}
	}

	return finalDirectives, padErrors
}

func GetBalanceTolerance(b *core.Balance, options *parser.Options) core.Decimal {
	if b.Tolerance != nil {
		return b.Tolerance
	}
	// Simplified default tolerance.
	return core.D("0.005")
}
