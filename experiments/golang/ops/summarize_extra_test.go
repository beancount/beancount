package ops

import (
	"strings"
	"testing"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

const inputSummarize = `
;; These should be preserved after summarization.
2010-01-01 open  Assets:US:Checking
2010-01-01 open  Assets:US:Investing:HOOL
2010-01-01 open  Assets:CA:BMO:Checking
2010-01-01 open  Liabilities:US:Chase:CreditCard
2010-01-01 open  Income:US:Employer:Salary
2010-01-01 open  Expenses:Taxes
2010-01-01 open  Expenses:Restaurant
2010-01-01 open  Expenses:Flights
2010-01-01 open  Expenses:Internet

;; These prices are redundant; only the last price will be preserved after
;; summarization.
2010-02-01 price USD  1.10 CAD
2010-03-01 price USD  1.11 CAD
2010-04-01 price USD  1.12 CAD
2010-05-01 price USD  1.13 CAD
2010-08-01 price USD  1.14 CAD
2010-10-01 price USD  1.15 CAD

;; This is the last price before the period, will be preserved.
2010-12-01 price USD  1.16 CAD

;; An account that gets capped before the period, should not appear in the
;; output.

2010-01-01 open  Assets:US:Temporary
2010-11-22 close  Assets:US:Temporary

2010-11-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Checking            3000.00 USD
  Expenses:Taxes                2000.00 USD

2010-11-20 * "First hit on credit card account"
  Liabilities:US:Chase:CreditCard   -67.20 USD
  Expenses:Restaurant                67.20 USD

2010-11-26 * "Second hit on credit card account (same account)"
  Liabilities:US:Chase:CreditCard   -345.23 USD
  Expenses:Flights                   345.23 USD

2010-11-30 *
  Assets:US:Checking            -80.02 USD
  Expenses:Internet              80.02 USD

2010-12-05 * "Unit held at cost"
  Assets:US:Investing:HOOL        5 HOOL {510.00 USD}
  Assets:US:Checking          -2550 USD

2010-12-05 * "Conversion"
  Assets:US:Checking          -910.00 USD
  Assets:CA:BMO:Checking      1000.00 CAD @ 0.91 USD

2010-12-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Checking            3000.00 USD
  Expenses:Taxes                2000.00 USD

2011-02-01 price USD  1.17 CAD
2011-04-01 price USD  1.18 CAD

2011-01-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Checking            3000.00 USD
  Expenses:Taxes                2000.00 USD

2011-01-20 * "Dinner at Cull & Pistol"
  Liabilities:US:Chase:CreditCard   -89.23 USD
  Expenses:Restaurant                89.23 USD

2011-02-01 open  Assets:Cash

2011-02-02 * "Cafe Mogador"
  Expenses:Restaurant      37.92 USD
  Assets:Cash             -37.92 USD

2011-02-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Checking            3000.00 USD
  Expenses:Taxes                2000.00 USD

2011-03-15 balance Assets:US:Checking    8459.98 USD
`

func TestTransferBalances(t *testing.T) {
	transferAccount := "Equity:Transfer"
	rawEntries, _, rawOptions := parser.ParseString(inputSummarize)
	opts, _ := parser.ProcessOptions(rawOptions)
	entries, _ := parser.Book(rawEntries, opts)
	core.SortDirectives(entries)

	t.Run("empty", func(t *testing.T) {
		xferEntries := TransferBalances([]core.Directive{}, parseDate("2011-01-01"), func(s string) bool { return strings.HasPrefix(s, "Assets:US:") }, transferAccount)
		if len(xferEntries) != 0 {
			t.Errorf("Expected empty entries, got %d", len(xferEntries))
		}
	})

	t.Run("middle_assets", func(t *testing.T) {
		date := parseDate("2011-01-01")
		xferEntries := TransferBalances(entries, date, func(s string) bool { return strings.HasPrefix(s, "Assets:US:Checking") }, transferAccount)
		
		var xferTxns []core.Directive
		for _, e := range xferEntries {
			if txn, ok := e.(*core.Transaction); ok && txn.Flag == core.FLAG_TRANSFER {
				xferTxns = append(xferTxns, e)
			}
		}
		
		compareEntries(t, `
2010-12-31 T "Transfer balance for 'Assets:US:Checking' (Transfer balance)"
  Assets:US:Checking  -2459.98 USD
  Equity:Transfer      2459.98 USD
`, xferTxns)
	})

	t.Run("middle_at_cost", func(t *testing.T) {
		date := parseDate("2011-01-01")
		xferEntries := TransferBalances(entries, date, func(s string) bool { return strings.HasPrefix(s, "Assets:US:Investing") }, transferAccount)
		
		var xferTxns []core.Directive
		for _, e := range xferEntries {
			if txn, ok := e.(*core.Transaction); ok && txn.Flag == core.FLAG_TRANSFER {
				xferTxns = append(xferTxns, e)
			}
		}

		compareEntries(t, `
2010-12-31 T "Transfer balance for 'Assets:US:Investing:HOOL' (Transfer balance)"
  Assets:US:Investing:HOOL  -5 HOOL {510 USD, 2010-12-05}
  Equity:Transfer            2550 USD
`, xferTxns)
	})

	t.Run("end_assets_implicit", func(t *testing.T) {
		date := parseDate("2011-03-01")
		xferEntries := TransferBalances(entries, date, func(s string) bool { return strings.HasPrefix(s, "Assets:US:Checking") }, transferAccount)
		
		var xferTxns []core.Directive
		for _, e := range xferEntries {
			if txn, ok := e.(*core.Transaction); ok && txn.Flag == core.FLAG_TRANSFER {
				xferTxns = append(xferTxns, e)
			}
		}

		compareEntries(t, `
2011-02-28 T "Transfer balance for 'Assets:US:Checking' (Transfer balance)"
  Assets:US:Checking  -8459.98 USD
  Equity:Transfer      8459.98 USD
`, xferTxns)
	})

	t.Run("middle_income", func(t *testing.T) {
		date := parseDate("2011-01-01")
		xferEntries := TransferBalances(entries, date, func(s string) bool {
			return strings.HasPrefix(s, "Income:") || strings.HasPrefix(s, "Expenses:")
		}, transferAccount)
		
		var xferTxns []core.Directive
		for _, e := range xferEntries {
			if txn, ok := e.(*core.Transaction); ok && txn.Flag == core.FLAG_TRANSFER {
				xferTxns = append(xferTxns, e)
			}
		}

		compareEntries(t, `
2010-12-31 T "Transfer balance for 'Expenses:Flights' (Transfer balance)"
  Expenses:Flights  -345.23 USD
  Equity:Transfer    345.23 USD

2010-12-31 T "Transfer balance for 'Expenses:Internet' (Transfer balance)"
  Expenses:Internet  -80.02 USD
  Equity:Transfer     80.02 USD

2010-12-31 T "Transfer balance for 'Expenses:Restaurant' (Transfer balance)"
  Expenses:Restaurant  -67.2 USD
  Equity:Transfer       67.2 USD

2010-12-31 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
  Expenses:Taxes  -4000 USD
  Equity:Transfer   4000 USD

2010-12-31 T "Transfer balance for 'Income:US:Employer:Salary' (Transfer balance)"
  Income:US:Employer:Salary  10000 USD
  Equity:Transfer           -10000 USD
`, xferTxns)
	})
}

func TestSummarize(t *testing.T) {
	openingAccount := "Equity:Opening-Balances"
	rawEntries, _, rawOptions := parser.ParseString(inputSummarize)
	opts, _ := parser.ProcessOptions(rawOptions)
	entries, _ := parser.Book(rawEntries, opts)
	core.SortDirectives(entries)

	t.Run("complete", func(t *testing.T) {
		summarizeDate := parseDate("2011-01-01")
		summarizedEntries, _ := Summarize(entries, summarizeDate, openingAccount)

		var summarizingEntries []core.Directive
		for _, entry := range summarizedEntries {
			if txn, ok := entry.(*core.Transaction); ok && txn.Flag == core.FLAG_SUMMARIZE {
				summarizingEntries = append(summarizingEntries, entry)
			}
		}

		compareEntries(t, `
2010-12-31 S "Opening balance for 'Assets:CA:BMO:Checking' (Summarization)"
  Assets:CA:BMO:Checking   1000 CAD
  Equity:Opening-Balances -1000 CAD

2010-12-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
  Assets:US:Checking  2459.98 USD
  Equity:Opening-Balances -2459.98 USD

2010-12-31 S "Opening balance for 'Assets:US:Investing:HOOL' (Summarization)"
  Assets:US:Investing:HOOL   5 HOOL {510 USD, 2010-12-05}
  Equity:Opening-Balances -2550 USD

2010-12-31 S "Opening balance for 'Expenses:Flights' (Summarization)"
  Expenses:Flights   345.23 USD
  Equity:Opening-Balances -345.23 USD

2010-12-31 S "Opening balance for 'Expenses:Internet' (Summarization)"
  Expenses:Internet   80.02 USD
  Equity:Opening-Balances -80.02 USD

2010-12-31 S "Opening balance for 'Expenses:Restaurant' (Summarization)"
  Expenses:Restaurant   67.2 USD
  Equity:Opening-Balances -67.2 USD

2010-12-31 S "Opening balance for 'Expenses:Taxes' (Summarization)"
  Expenses:Taxes   4000 USD
  Equity:Opening-Balances -4000 USD

2010-12-31 S "Opening balance for 'Income:US:Employer:Salary' (Summarization)"
  Income:US:Employer:Salary -10000 USD
  Equity:Opening-Balances   10000 USD

2010-12-31 S "Opening balance for 'Liabilities:US:Chase:CreditCard' (Summarization)"
  Liabilities:US:Chase:CreditCard  -412.43 USD
  Equity:Opening-Balances   412.43 USD
`, summarizingEntries)
	})
}
