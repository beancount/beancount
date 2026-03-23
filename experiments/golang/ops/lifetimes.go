package ops

import (
	"time"

	"github.com/beancount/beancount/v3/core"
)

// AccountLifetime represents the first and last date an account was seen.
type AccountLifetime struct {
	First time.Time
	Last  time.Time
}

// GetAccountLifetimes returns a map of account names to their lifetimes.
func GetAccountLifetimes(directives []core.Directive) map[string]AccountLifetime {
	lifetimes := make(map[string]AccountLifetime)

	update := func(account string, date time.Time) {
		if account == "" {
			return
		}
		lt, ok := lifetimes[account]
		if !ok {
			lifetimes[account] = AccountLifetime{First: date, Last: date}
			return
		}
		if date.Before(lt.First) {
			lt.First = date
		}
		if date.After(lt.Last) {
			lt.Last = date
		}
		lifetimes[account] = lt
	}

	for _, d := range directives {
		date := d.GetDate()
		switch e := d.(type) {
		case *core.Open:
			update(e.Account, date)
		case *core.Close:
			update(e.Account, date)
		case *core.Transaction:
			for _, p := range e.Postings {
				update(p.Account, date)
			}
		case *core.Balance:
			update(e.Account, date)
		case *core.Pad:
			update(e.Account, date)
			update(e.SourceAccount, date)
		case *core.Note:
			update(e.Account, date)
		case *core.Document:
			update(e.Account, date)
		}
	}

	return lifetimes
}
