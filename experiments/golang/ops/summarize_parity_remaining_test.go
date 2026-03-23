package ops

import (
	"testing"
	"time"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

func TestConversions(t *testing.T) {
	t.Run("empty", func(t *testing.T) {
		res := Conversions([]core.Directive{}, "Equity:Conversions", "XFER", time.Time{})
		if len(res) != 0 {
			t.Errorf("Expected empty, got %d", len(res))
		}
	})

	t.Run("not_needed", func(t *testing.T) {
		input := `
2012-01-01 open Assets:Checking
2012-01-01 * "Initial"
  Assets:Checking  100 USD
  Equity:Opening   -100 USD
`
		directives, _, rawOptions := parser.ParseString(input)
		options, _ := parser.ProcessOptions(rawOptions)
		entries, _ := parser.Book(directives, options)
		res := Conversions(entries, "Equity:Conversions", "XFER", parseDate("2012-02-01"))
		if len(res) < 2 {
			t.Errorf("Expected at least 2 entries, got %d", len(res))
		}
	})

	t.Run("needed_middle", func(t *testing.T) {
		input := `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1.0 CAD
  Assets:CAD   800 CAD
`
		directives, _, rawOptions := parser.ParseString(input)
		options, _ := parser.ProcessOptions(rawOptions)
		entries, _ := parser.Book(directives, options)
		core.SortDirectives(entries)

		xferEntries := Conversions(entries, "Equity:Conversions", "XFER", parseDate("2012-03-02"))
		
		compareEntries(t, `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD

2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1 CAD
  Assets:CAD   800 CAD

2012-03-01 C "Conversion for (-800 USD, 800 CAD)"
  Equity:Conversions   800 USD @ 0 XFER
  Equity:Conversions  -800 CAD @ 0 XFER
`, xferEntries)
	})

	t.Run("with_transactions_at_cost", func(t *testing.T) {
		input := `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-01-01 open Assets:NT
2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1.0 CAD
  Assets:CAD   800 CAD
2012-03-03 * "Buy"
  Assets:NT     60 NT {10 CAD}
  Assets:CAD  -600 CAD
`
		directives, _, rawOptions := parser.ParseString(input)
		options, _ := parser.ProcessOptions(rawOptions)
		entries, _ := parser.Book(directives, options)
		core.SortDirectives(entries)

		xferEntries := Conversions(entries, "Equity:Conversions", "XFER", parseDate("2012-03-09"))
		
		compareEntries(t, `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-01-01 open Assets:NT

2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1 CAD
  Assets:CAD   800 CAD

2012-03-03 * "Buy"
  Assets:NT     60 NT {10 CAD, 2012-03-03}
  Assets:CAD  -600 CAD

2012-03-08 C "Conversion for (-800 USD, 200 CAD, 60 NT {10 CAD, 2012-03-03})"
  Equity:Conversions   800 USD @ 0 XFER
  Equity:Conversions  -800 CAD @ 0 XFER
`, xferEntries)
	})

	t.Run("multiple", func(t *testing.T) {
		input := `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-01-01 open Assets:NT
2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1.0 CAD
  Assets:CAD   800 CAD
2012-03-03 * "Buy"
  Assets:NT     60 NT {10 CAD}
  Assets:CAD  -600 CAD
2012-03-05 * "Div"
  Income:Salary  -100 USD
  Assets:USD      100 USD
`
		directives, _, rawOptions := parser.ParseString(input)
		options, _ := parser.ProcessOptions(rawOptions)
		entries, _ := parser.Book(directives, options)
		core.SortDirectives(entries)

		xferEntries := Conversions(entries, "Equity:Conversions", "XFER", parseDate("2012-03-09"))
		
		compareEntries(t, `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-01-01 open Assets:NT

2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1 CAD
  Assets:CAD   800 CAD

2012-03-03 * "Buy"
  Assets:NT     60 NT {10 CAD, 2012-03-03}
  Assets:CAD  -600 CAD

2012-03-05 * "Div"
  Income:Salary  -100 USD
  Assets:USD      100 USD

2012-03-08 C "Conversion for (-800 USD, 200 CAD, 60 NT {10 CAD, 2012-03-03})"
  Equity:Conversions   800 USD @ 0 XFER
  Equity:Conversions  -800 CAD @ 0 XFER
`, xferEntries)
	})

	t.Run("no_date", func(t *testing.T) {
		input := `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-01-01 open Assets:NT
2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1.0 CAD
  Assets:CAD   800 CAD
2012-03-03 * "Buy"
  Assets:NT     60 NT {10 CAD}
  Assets:CAD  -600 CAD
2012-03-05 * "Div"
  Income:Salary  -100 USD
  Assets:USD      100 USD
`
		directives, _, rawOptions := parser.ParseString(input)
		options, _ := parser.ProcessOptions(rawOptions)
		entries, _ := parser.Book(directives, options)
		core.SortDirectives(entries)

		xferEntries := Conversions(entries, "Equity:Conversions", "XFER", time.Time{})
		
		compareEntries(t, `
2012-01-01 open Assets:USD
2012-01-01 open Assets:CAD
2012-01-01 open Assets:NT

2012-03-01 * "Conversion"
  Assets:USD  -800 USD @ 1 CAD
  Assets:CAD   800 CAD

2012-03-03 * "Buy"
  Assets:NT     60 NT {10 CAD, 2012-03-03}
  Assets:CAD  -600 CAD

2012-03-05 * "Div"
  Income:Salary  -100 USD
  Assets:USD      100 USD

2012-03-05 C "Conversion for (-800 USD, 200 CAD, 60 NT {10 CAD, 2012-03-03})"
  Equity:Conversions   800 USD @ 0 XFER
  Equity:Conversions  -800 CAD @ 0 XFER
`, xferEntries)
	})

	t.Run("non_empty_but_empty_cost", func(t *testing.T) {
		input := `
2012-01-01 open Assets:USD
2012-01-01 * "Initial"
  Assets:USD  100 USD
  Equity:Opening -100 USD
`
		directives, _, rawOptions := parser.ParseString(input)
		options, _ := parser.ProcessOptions(rawOptions)
		entries, _ := parser.Book(directives, options)
		res := Conversions(entries, "Equity:Conversions", "XFER", parseDate("2012-02-01"))
		if len(res) < 2 {
			t.Errorf("Expected at least 2 entries, got %d", len(res))
		}
	})
}

func TestOpenCloseClear(t *testing.T) {
	input := `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-03-01 * "Some income and expense to be summarized"
  Income:Salary        10000 USD
  Expenses:Taxes        3600 USD
  Assets:US:Checking  -13600 USD

2012-03-02 * "Some conversion to be summarized"
  Assets:US:Checking   -5000 USD @ 1.2 CAD
  Assets:CA:Checking    6000 CAD

2012-08-01 * "Some income and expense to show"
  Income:Salary        11000 USD
  Expenses:Taxes        3200 USD
  Assets:US:Checking  -14200 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking  -3000 USD @ 1.25 CAD
  Assets:CA:Checking   3750 CAD
`
	directives, _, rawOptions := parser.ParseString(input)
	options, _ := parser.ProcessOptions(rawOptions)
	entries, _ := parser.Book(directives, options)
	core.SortDirectives(entries)

	opts := SummarizeOptions{
		AccountTypes:       options.AccountTypes,
		ConversionCurrency: "NOTHING",
		AccountEarnings:    "Equity:Earnings",
		AccountOpening:     "Equity:Opening-Balances",
		AccountConversions: "Equity:Conversions",
	}

	t.Run("test_open", func(t *testing.T) {
		openDate := parseDate("2012-06-01")
		openedEntries, _ := Open(entries, openDate, opts)
		
		compareEntries(t, `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-05-31 S "Opening balance for 'Assets:CA:Checking' (Summarization)"
  Assets:CA:Checking        6000 CAD
  Equity:Opening-Balances  -6000 CAD

2012-05-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
  Assets:US:Checking       -18600 USD
  Equity:Opening-Balances   18600 USD

2012-05-31 S "Opening balance for 'Equity:Conversions' (Summarization)"
  Equity:Conversions        5000 USD
  Equity:Opening-Balances  -5000 USD
  Equity:Conversions       -6000 CAD
  Equity:Opening-Balances   6000 CAD

2012-05-31 S "Opening balance for 'Equity:Earnings' (Summarization)"
  Equity:Earnings           13600 USD
  Equity:Opening-Balances  -13600 USD

2012-08-01 * "Some income and expense to show"
  Income:Salary        11000 USD
  Expenses:Taxes        3200 USD
  Assets:US:Checking  -14200 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking  -3000 USD @ 1.25 CAD
  Assets:CA:Checking   3750 CAD
`, openedEntries)
	})

	t.Run("test_close", func(t *testing.T) {
		closeDate := parseDate("2012-09-01")
		closedEntries, _ := Close(entries, closeDate, opts)
		
		compareEntries(t, `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-03-01 * "Some income and expense to be summarized"
  Income:Salary        10000 USD
  Expenses:Taxes        3600 USD
  Assets:US:Checking  -13600 USD

2012-03-02 * "Some conversion to be summarized"
  Assets:US:Checking   -5000 USD @ 1.2 CAD
  Assets:CA:Checking    6000 CAD

2012-08-01 * "Some income and expense to show"
  Income:Salary        11000 USD
  Expenses:Taxes        3200 USD
  Assets:US:Checking  -14200 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking  -3000 USD @ 1.25 CAD
  Assets:CA:Checking   3750 CAD

2012-08-31 C "Conversion for (-8000 USD, 9750 CAD)"
  Equity:Conversions   8000 USD @ 0 NOTHING
  Equity:Conversions  -9750 CAD @ 0 NOTHING
`, closedEntries)
	})

	t.Run("test_clear", func(t *testing.T) {
		clearDate := parseDate("2012-09-01")
		clearedEntries, _ := Clear(entries, clearDate, options.AccountTypes, "Equity:Earnings")
		
		compareEntries(t, `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-03-01 * "Some income and expense to be summarized"
  Income:Salary        10000 USD
  Expenses:Taxes        3600 USD
  Assets:US:Checking  -13600 USD

2012-03-02 * "Some conversion to be summarized"
  Assets:US:Checking   -5000 USD @ 1.2 CAD
  Assets:CA:Checking    6000 CAD

2012-08-01 * "Some income and expense to show"
  Income:Salary        11000 USD
  Expenses:Taxes        3200 USD
  Assets:US:Checking  -14200 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking  -3000 USD @ 1.25 CAD
  Assets:CA:Checking   3750 CAD

2012-08-31 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
  Expenses:Taxes   -6800 USD
  Equity:Earnings   6800 USD

2012-08-31 T "Transfer balance for 'Income:Salary' (Transfer balance)"
  Income:Salary    -21000 USD
  Equity:Earnings   21000 USD
`, clearedEntries)
	})

	t.Run("test_open_close_clear", func(t *testing.T) {
		beginDate := parseDate("2012-06-01")
		endDate := parseDate("2012-09-01")
		
		// Initial state before period.
		pred := func(account string) bool { return core.IsIncomeStatementAccount(account, options.AccountTypes) }
		clonedEntries := make([]core.Directive, len(entries))
		copy(clonedEntries, entries)
		
		processedEntries := TransferBalances(clonedEntries, beginDate, pred, "Equity:Earnings:Previous")
		processedEntries = Conversions(processedEntries, "Equity:Conversions:Previous", "NOTHING", beginDate)
		processedEntries, _ = Summarize(processedEntries, beginDate, "Equity:Opening-Balances")
		
		// Final state after period.
		processedEntries, _ = Clear(processedEntries, endDate, options.AccountTypes, "Equity:Earnings:Current")
		processedEntries = Conversions(processedEntries, "Equity:Conversions:Current", "NOTHING", endDate)
		processedEntries = Truncate(processedEntries, endDate)

		compareEntries(t, `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-05-31 S "Opening balance for 'Assets:CA:Checking' (Summarization)"
  Assets:CA:Checking        6000 CAD
  Equity:Opening-Balances  -6000 CAD

2012-05-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
  Assets:US:Checking       -18600 USD
  Equity:Opening-Balances   18600 USD

2012-05-31 S "Opening balance for 'Equity:Conversions:Previous' (Summarization)"
  Equity:Conversions:Previous   5000 USD
  Equity:Opening-Balances      -5000 USD
  Equity:Conversions:Previous  -6000 CAD
  Equity:Opening-Balances       6000 CAD

2012-05-31 S "Opening balance for 'Equity:Earnings:Previous' (Summarization)"
  Equity:Earnings:Previous   13600 USD
  Equity:Opening-Balances   -13600 USD

2012-08-01 * "Some income and expense to show"
  Income:Salary        11000 USD
  Expenses:Taxes        3200 USD
  Assets:US:Checking  -14200 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking  -3000 USD @ 1.25 CAD
  Assets:CA:Checking   3750 CAD

2012-08-31 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
  Expenses:Taxes           -3200 USD
  Equity:Earnings:Current   3200 USD

2012-08-31 T "Transfer balance for 'Income:Salary' (Transfer balance)"
  Income:Salary            -11000 USD
  Equity:Earnings:Current   11000 USD

2012-08-31 C "Conversion for (-3000 USD, 3750 CAD)"
  Equity:Conversions:Current   3000 USD @ 0 NOTHING
  Equity:Conversions:Current  -3750 CAD @ 0 NOTHING
`, processedEntries)
	})
}

func TestClamp(t *testing.T) {
	input := `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-03-01 * "Some income and expense to be summarized"
  Income:Salary        10000.00 USD
  Expenses:Taxes        3600.00 USD
  Assets:US:Checking  -13600.00 USD

2012-03-02 * "Some conversion to be summarized"
  Assets:US:Checking   -5000.00 USD @ 1.2 CAD
  Assets:CA:Checking    6000.00 CAD

2012-08-01 * "Some income and expense to show"
  Income:Salary        11000.00 USD
  Expenses:Taxes        3200.00 USD
  Assets:US:Checking  -14200.00 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking  -3000.00 USD @ 1.25 CAD
  Assets:CA:Checking   3750.00 CAD
`
	directives, _, rawOptions := parser.ParseString(input)
	options, _ := parser.ProcessOptions(rawOptions)
	entries, _ := parser.Book(directives, options)
	core.SortDirectives(entries)

	beginDate := parseDate("2012-06-01")
	endDate := parseDate("2012-09-01")

	opts := SummarizeOptions{
		AccountTypes:       options.AccountTypes,
		ConversionCurrency: "NOTHING",
		AccountEarnings:    "Equity:Earnings",
		AccountOpening:     "Equity:Opening-Balances",
		AccountConversions: "Equity:Conversions",
	}

	clampedEntries, _ := Clamp(entries, beginDate, endDate, opts)

	compareEntries(t, `
2012-01-01 open Income:Salary
2012-01-01 open Expenses:Taxes
2012-01-01 open Assets:US:Checking
2012-01-01 open Assets:CA:Checking

2012-05-31 S "Opening balance for 'Assets:CA:Checking' (Summarization)"
  Assets:CA:Checking              6000 CAD
  Equity:Opening-Balances         -6000 CAD

2012-05-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
  Assets:US:Checking            -18600 USD
  Equity:Opening-Balances         18600 USD

2012-05-31 S "Opening balance for 'Equity:Earnings' (Summarization)"
  Equity:Earnings                13600 USD
  Equity:Opening-Balances        -13600 USD

2012-08-01 * "Some income and expense to show"
  Income:Salary                  11000 USD
  Expenses:Taxes                  3200 USD
  Assets:US:Checking            -14200 USD

2012-08-02 * "Some other conversion to be summarized"
  Assets:US:Checking             -3000 USD  @ 1.25 CAD
  Assets:CA:Checking              3750 CAD

2012-08-31 C "Conversion for (-3000 USD, 3750 CAD)"
  Equity:Conversions              3000 USD  @ 0 NOTHING
  Equity:Conversions             -3750 CAD  @ 0 NOTHING
`, clampedEntries)
}

func TestCap(t *testing.T) {
	input := `
2014-03-01 * "Some income and expense"
  Income:Salary        10000.00 USD
  Expenses:Taxes        3200.00 USD
  Assets:US:Checking  -13200.00 USD

2014-03-02 * "Some conversion"
  Assets:US:Checking   -5000.00 USD @ 1.2 CAD
  Assets:CA:Checking    6000.00 CAD
`
	directives, _, rawOptions := parser.ParseString(input)
	options, _ := parser.ProcessOptions(rawOptions)
	entries, _ := parser.Book(directives, options)
	core.SortDirectives(entries)

	cappedEntries := Cap(entries, options.AccountTypes, "NOTHING", "Equity:Earnings", "Equity:Conversions")

	compareEntries(t, `
2014-03-01 * "Some income and expense"
  Income:Salary        10000 USD
  Expenses:Taxes        3200 USD
  Assets:US:Checking  -13200 USD

2014-03-02 * "Some conversion"
  Assets:US:Checking   -5000 USD @ 1.2 CAD
  Assets:CA:Checking    6000 CAD

2014-03-02 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
  Expenses:Taxes   -3200 USD
  Equity:Earnings   3200 USD

2014-03-02 T "Transfer balance for 'Income:Salary' (Transfer balance)"
  Income:Salary    -10000 USD
  Equity:Earnings   10000 USD

2014-03-02 C "Conversion for (-5000 USD, 6000 CAD)"
  Equity:Conversions   5000 USD @ 0 NOTHING
  Equity:Conversions  -6000 CAD @ 0 NOTHING
`, cappedEntries)
}
