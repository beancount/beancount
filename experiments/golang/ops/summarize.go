package ops

import (
	"fmt"
	"sort"
	"time"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

// SummarizeOptions contains parameters for the summarize functions.
type SummarizeOptions struct {
	AccountTypes       core.AccountTypes
	ConversionCurrency string
	AccountEarnings    string
	AccountOpening     string
	AccountConversions string
}

// Open summarizes entries before a date and transfer income/expenses to equity.
func Open(
	entries []core.Directive,
	date time.Time,
	opts SummarizeOptions,
) ([]core.Directive, int) {
	entries = Conversions(entries, opts.AccountConversions, opts.ConversionCurrency, date)
	entries, _ = Clear(entries, date, opts.AccountTypes, opts.AccountEarnings)
	return Summarize(entries, date, opts.AccountOpening)
}

// Close truncates entries that occur after a particular date and ensure balance.
func Close(
	entries []core.Directive,
	date time.Time,
	opts SummarizeOptions,
) ([]core.Directive, int) {
	if !date.IsZero() {
		entries = Truncate(entries, date)
	}
	index := len(entries)
	entries = Conversions(entries, opts.AccountConversions, opts.ConversionCurrency, date)
	return entries, index
}

// Clear transfers income and expenses balances at the given date to equity.
func Clear(
	entries []core.Directive,
	date time.Time,
	accountTypes core.AccountTypes,
	accountEarnings string,
) ([]core.Directive, int) {
	index := len(entries)
	pred := func(account string) bool {
		return core.IsIncomeStatementAccount(account, accountTypes)
	}
	newEntries := TransferBalances(entries, date, pred, accountEarnings)
	return newEntries, index
}

// Clamp filters entries to include only those during a specified time period.
func Clamp(
	entries []core.Directive,
	beginDate time.Time,
	endDate time.Time,
	opts SummarizeOptions,
) ([]core.Directive, int) {
	pred := func(account string) bool {
		return core.IsIncomeStatementAccount(account, opts.AccountTypes)
	}
	entries = TransferBalances(entries, beginDate, pred, opts.AccountEarnings)
	entries, index := Summarize(entries, beginDate, opts.AccountOpening)
	entries = Truncate(entries, endDate)
	entries = Conversions(entries, opts.AccountConversions, opts.ConversionCurrency, endDate)
	return entries, index
}

// Cap transfers net income to equity and insert a final conversion entry.
func Cap(
	entries []core.Directive,
	accountTypes core.AccountTypes,
	conversionCurrency string,
	accountEarnings string,
	accountConversions string,
) []core.Directive {
	pred := func(account string) bool {
		return core.IsIncomeStatementAccount(account, accountTypes)
	}
	entries = TransferBalances(entries, time.Time{}, pred, accountEarnings)
	entries = Conversions(entries, accountConversions, conversionCurrency, time.Time{})
	return entries
}

// TransferBalances synthesizes transactions to transfer balances from some accounts at a given date.
func TransferBalances(
	entries []core.Directive,
	date time.Time,
	accountPred func(string) bool,
	transferAccount string,
) []core.Directive {
	if len(entries) == 0 {
		return entries
	}

	balances, index := BalanceByAccount(entries, date, false)
	transferBalances := make(map[string]*core.Inventory)
	for account, balance := range balances {
		if accountPred(account) {
			transferBalances[account] = balance
		}
	}

	var transferDate time.Time
	if !date.IsZero() {
		transferDate = date.AddDate(0, 0, -1)
	} else {
		transferDate = entries[len(entries)-1].GetDate()
	}

	transferEntries := CreateEntriesFromBalances(
		transferBalances,
		transferDate,
		transferAccount,
		false,
		core.NewMetadata("<transfer_balances>", 0),
		core.FLAG_TRANSFER,
		"Transfer balance for '%s' (Transfer balance)",
	)

	afterEntries := []core.Directive{}
	for _, entry := range entries[index:] {
		if b, ok := entry.(*core.Balance); ok {
			if _, ok := transferBalances[b.Account]; ok {
				continue
			}
		}
		afterEntries = append(afterEntries, entry)
	}

	res := make([]core.Directive, 0, index+len(transferEntries)+len(afterEntries))
	res = append(res, entries[:index]...)
	res = append(res, transferEntries...)
	res = append(res, afterEntries...)
	return res
}

// Summarize all entries before a date by replacing then with summarization entries.
func Summarize(
	entries []core.Directive,
	date time.Time,
	accountOpening string,
) ([]core.Directive, int) {
	balances, index := BalanceByAccount(entries, date, false)
	summarizeDate := date.AddDate(0, 0, -1)

	summarizingEntries := CreateEntriesFromBalances(
		balances,
		summarizeDate,
		accountOpening,
		true,
		core.NewMetadata("<summarize>", 0),
		core.FLAG_SUMMARIZE,
		"Opening balance for '%s' (Summarization)",
	)

	priceEntries := core.GetLastPriceEntries(entries, date)
	openEntries := GetOpenEntries(entries, date)

	beforeEntries := make([]core.Directive, 0, len(openEntries)+len(priceEntries)+len(summarizingEntries))
	for _, e := range openEntries {
		beforeEntries = append(beforeEntries, e)
	}
	for _, e := range priceEntries {
		beforeEntries = append(beforeEntries, e)
	}
	for _, e := range summarizingEntries {
		beforeEntries = append(beforeEntries, e)
	}
	core.SortDirectives(beforeEntries)

	afterEntries := entries[index:]
	res := make([]core.Directive, 0, len(beforeEntries)+len(afterEntries))
	res = append(res, beforeEntries...)
	res = append(res, afterEntries...)
	return res, len(beforeEntries)
}

// Conversions inserts a conversion entry at date 'date' at the given account.
func Conversions(
	entries []core.Directive,
	conversionAccount string,
	conversionCurrency string,
	date time.Time,
) []core.Directive {
	balances, index := BalanceByAccount(entries, date, false)
	totalInv := core.NewInventory()
	for _, inv := range balances {
		totalInv.AddInventory(inv)
	}

	conversionCostBalance := totalInv.Reduce(core.GetWeight)
	if conversionCostBalance.IsEmpty() {
		return entries
	}

	var lastDate time.Time
	if !date.IsZero() {
		lastDate = date.AddDate(0, 0, -1)
	} else {
		if len(entries) > 0 {
			lastDate = entries[len(entries)-1].GetDate()
		} else {
			lastDate = time.Now()
		}
	}

	meta := core.NewMetadata("<conversions>", -1)
	invStr := totalInv.String()
	fmt.Printf("DEBUG: totalInv.String() = %q\n", invStr)
	narration := fmt.Sprintf("Conversion for %s", invStr)
	conversionEntry := &core.Transaction{
		Meta:      meta,
		Date:      lastDate,
		Flag:      core.FLAG_CONVERSIONS,
		Narration: narration,
		Postings:  []core.Posting{},
	}

	positions := conversionCostBalance.GetPositions()
	core.SortPositions(positions)
	for _, position := range positions {
		price := core.NewAmount(core.ZERO, conversionCurrency)
		negPos := position.Neg()
		conversionEntry.Postings = append(conversionEntry.Postings, core.Posting{
			Account: conversionAccount,
			Units:   negPos.Units,
			Cost:    negPos.Cost,
			Price:   price,
		})
	}

	newEntries := make([]core.Directive, 0, len(entries)+1)
	newEntries = append(newEntries, entries[:index]...)
	newEntries = append(newEntries, conversionEntry)
	newEntries = append(newEntries, entries[index:]...)
	return newEntries
}

// Truncate filters out all the entries at and after date.
func Truncate(entries []core.Directive, date time.Time) []core.Directive {
	index := len(entries)
	for i, e := range entries {
		if !e.GetDate().Before(date) {
			index = i
			break
		}
	}
	return entries[:index]
}

// CreateEntriesFromBalances creates a list of entries from a dict of balances.
func CreateEntriesFromBalances(
	balances map[string]*core.Inventory,
	date time.Time,
	sourceAccount string,
	direction bool,
	meta core.Meta,
	flag string,
	narrationTemplate string,
) []core.Directive {
	var newEntries []core.Directive
	
	accounts := make([]string, 0, len(balances))
	for a := range balances {
		accounts = append(accounts, a)
	}
	sort.Strings(accounts)

	for _, account := range accounts {
		accountBalance := balances[account]
		if accountBalance.IsEmpty() {
			continue
		}

		narration := fmt.Sprintf(narrationTemplate, account)

		if !direction {
			accountBalance = accountBalance.Neg()
		}

		postings := []core.Posting{}
		positions := accountBalance.GetPositions()
		core.SortPositions(positions)
		for _, position := range positions {
			postings = append(postings, core.Posting{
				Account: account,
				Units:   position.Units,
				Cost:    position.Cost,
			})
			cost := core.GetWeight(position).Neg()
			postings = append(postings, core.Posting{
				Account: sourceAccount,
				Units:   cost,
			})
		}

		newEntries = append(newEntries, &core.Transaction{
			Meta:      meta,
			Date:      date,
			Flag:      flag,
			Narration: narration,
			Postings:  postings,
		})
	}

	return newEntries
}

// BalanceByAccount sums up the balance per account for all entries strictly before 'date'.
func BalanceByAccount(
	entries []core.Directive,
	date time.Time,
	compressUnbooked bool,
) (map[string]*core.Inventory, int) {
	balances := make(map[string]*core.Inventory)
	index := len(entries)

	for i, entry := range entries {
		if !date.IsZero() && !entry.GetDate().Before(date) {
			index = i
			break
		}

		if txn, ok := entry.(*core.Transaction); ok {
			for _, posting := range txn.Postings {
				if _, ok := balances[posting.Account]; !ok {
					balances[posting.Account] = core.NewInventory()
				}
				balances[posting.Account].AddPosition(core.Position{Units: posting.Units, Cost: posting.Cost})
			}
		}
	}

	if compressUnbooked {
		for acc, inv := range balances {
			balances[acc] = inv.Average()
		}
	}

	return balances, index
}

// GetOpenEntries gathers the list of active Open entries at date.
func GetOpenEntries(entries []core.Directive, date time.Time) []core.Directive {
	type entryWithIndex struct {
		index int
		entry *core.Open
	}
	openEntries := make(map[string]entryWithIndex)

	for index, entry := range entries {
		if !date.IsZero() && !entry.GetDate().Before(date) {
			break
		}

		switch e := entry.(type) {
		case *core.Open:
			if ex, ok := openEntries[e.Account]; ok {
				if e.Date.Before(ex.entry.Date) {
					openEntries[e.Account] = entryWithIndex{index, e}
				}
			} else {
				openEntries[e.Account] = entryWithIndex{index, e}
			}
		case *core.Close:
			delete(openEntries, e.Account)
		}
	}

	var result []entryWithIndex
	for _, v := range openEntries {
		result = append(result, v)
	}
	sort.Slice(result, func(i, j int) bool {
		return result[i].index < result[j].index
	})

	final := make([]core.Directive, len(result))
	for i, v := range result {
		final[i] = v.entry
	}
	return final
}

func GetSummarizeOptions(opts *parser.Options) SummarizeOptions {
	sopts := SummarizeOptions{
		AccountTypes:       core.DEFAULT_ACCOUNT_TYPES,
		ConversionCurrency: "USD",
		AccountEarnings:    "Equity:Earnings",
		AccountOpening:     "Equity:Opening-Balances",
		AccountConversions: "Equity:Conversions",
	}
	if opts != nil {
		// Populate sopts from opts if needed.
	}
	return sopts
}
