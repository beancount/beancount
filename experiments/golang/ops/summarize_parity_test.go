package ops

import (
	"strings"
	"testing"
	"time"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

func parseDate(s string) time.Time {
	t, _ := time.Parse("2006-01-02", s)
	return t
}

func compareEntries(t *testing.T, expected string, actual []core.Directive) {
	t.Helper()
	actualStr := parser.PrintDirectives(actual)
	
	// Normalize: trim leading/trailing whitespace, and replace multiple spaces/newlines
	normalize := func(s string) string {
		lines := strings.Split(s, "\n")
		var cleanLines []string
		for _, line := range lines {
			line = strings.TrimSpace(line)
			if line != "" {
				// Internal space normalization
				words := strings.Fields(line)
				cleanLines = append(cleanLines, strings.Join(words, " "))
			}
		}
		return strings.Join(cleanLines, "\n")
	}

	normExpected := normalize(expected)
	normActual := normalize(actualStr)

	if normExpected != normActual {
		t.Errorf("Entries mismatch.\nExpected:\n%s\nActual:\n%s", normExpected, normActual)
	}
}

func TestBalanceByAccount(t *testing.T) {
	input := `
2013-01-01 open Assets:AccountA
2013-01-01 open Assets:AccountB

2013-01-02 * "Initial"
  Assets:AccountA  100.00 USD
  Assets:AccountB  -100.00 USD

2013-02-01 * "Second"
  Assets:AccountA  50.00 USD
  Assets:AccountB  -50.00 USD
`
	directives, _, rawOptions := parser.ParseString(input)
	options, _ := parser.ProcessOptions(rawOptions)
	entries, _ := parser.Book(directives, options)

	t.Run("no_end_date", func(t *testing.T) {
		balances, index := BalanceByAccount(entries, time.Time{}, false)
		if len(balances) != 2 {
			t.Errorf("Expected 2 balances, got %d", len(balances))
		}
		if index != 4 {
			t.Errorf("Expected index 4, got %d", index)
		}
	})

	t.Run("first_date", func(t *testing.T) {
		balances, index := BalanceByAccount(entries, parseDate("2013-01-01"), false)
		if len(balances) != 0 {
			t.Errorf("Expected 0 balances, got %d", len(balances))
		}
		if index != 0 {
			t.Errorf("Expected index 0, got %d", index)
		}
	})

	t.Run("middle", func(t *testing.T) {
		balances, index := BalanceByAccount(entries, parseDate("2013-01-15"), false)
		if len(balances) != 2 {
			t.Errorf("Expected 2 balances, got %d", len(balances))
		}
		if index != 3 {
			t.Errorf("Expected index 3, got %d", index)
		}
	})
}

func TestTruncate(t *testing.T) {
	input := `
2013-01-01 open Assets:AccountA
2013-01-02 * "Txn1"
  Assets:AccountA  100 USD
  Equity:Opening -100 USD
2013-01-03 * "Txn2"
  Assets:AccountA  50 USD
  Equity:Opening -50 USD
`
	entries, _, _ := parser.ParseString(input)

	t.Run("before", func(t *testing.T) {
		res := Truncate(entries, parseDate("2013-01-01"))
		if len(res) != 0 {
			t.Errorf("Expected 0 entries, got %d", len(res))
		}
	})

	t.Run("normal1", func(t *testing.T) {
		res := Truncate(entries, parseDate("2013-01-02"))
		if len(res) != 1 {
			t.Errorf("Expected 1 entry, got %d", len(res))
		}
	})

	t.Run("normal2", func(t *testing.T) {
		res := Truncate(entries, parseDate("2013-01-03"))
		if len(res) != 2 {
			t.Errorf("Expected 2 entries, got %d", len(res))
		}
	})

	t.Run("after", func(t *testing.T) {
		res := Truncate(entries, parseDate("2013-01-04"))
		if len(res) != 3 {
			t.Errorf("Expected 3 entries, got %d", len(res))
		}
	})
}

func TestCreateEntriesFromBalances(t *testing.T) {
	sourceAccount := "Equity:Opening-Balances"
	meta := core.NewMetadata("test.beancount", 1)

	t.Run("empty", func(t *testing.T) {
		balances := make(map[string]*core.Inventory)
		entries := CreateEntriesFromBalances(balances, parseDate("2014-01-01"), sourceAccount, true, meta, core.FLAG_WARNING, "Narration for %s at 2014-01-01")
		if len(entries) != 0 {
			t.Errorf("Expected 0 entries, got %d", len(entries))
		}
	})

	t.Run("simple", func(t *testing.T) {
		balances := make(map[string]*core.Inventory)
		inv1 := core.NewInventory()
		inv1.AddPosition(core.Position{Units: core.NewAmount(core.D("1823.23"), "USD")})
		balances["Assets:US:Bank:Checking"] = inv1

		inv2 := core.NewInventory()
		inv2.AddPosition(core.Position{
			Units: core.NewAmount(core.D("10"), "HOOL"),
			Cost:  &core.Cost{Number: core.D("500.00"), Currency: "USD"},
		})
		balances["Assets:US:Investment"] = inv2

		entries := CreateEntriesFromBalances(
			balances,
			parseDate("2014-01-01"),
			sourceAccount,
			true,
			meta,
			"!",
			"Narration for %s at 2014-01-01",
		)
		compareEntries(t, `
2014-01-01 ! "Narration for Assets:US:Bank:Checking at 2014-01-01"
  Assets:US:Bank:Checking   1823.23 USD
  Equity:Opening-Balances  -1823.23 USD

2014-01-01 ! "Narration for Assets:US:Investment at 2014-01-01"
  Assets:US:Investment       10 HOOL {500 USD}
  Equity:Opening-Balances  -5000 USD
`, entries)
	})

	t.Run("reverse", func(t *testing.T) {
		balances := make(map[string]*core.Inventory)
		inv1 := core.NewInventory()
		inv1.AddPosition(core.Position{Units: core.NewAmount(core.D("1823.23"), "USD")})
		balances["Assets:US:Bank:Checking"] = inv1

		inv2 := core.NewInventory()
		inv2.AddPosition(core.Position{
			Units: core.NewAmount(core.D("10"), "HOOL"),
			Cost:  &core.Cost{Number: core.D("500.00"), Currency: "USD"},
		})
		balances["Assets:US:Investment"] = inv2

		entries := CreateEntriesFromBalances(
			balances,
			parseDate("2014-01-01"),
			sourceAccount,
			false,
			meta,
			"*",
			"Narration for %s at 2014-01-01",
		)
		compareEntries(t, `
2014-01-01 * "Narration for Assets:US:Bank:Checking at 2014-01-01"
  Assets:US:Bank:Checking  -1823.23 USD
  Equity:Opening-Balances   1823.23 USD

2014-01-01 * "Narration for Assets:US:Investment at 2014-01-01"
  Assets:US:Investment      -10 HOOL {500 USD}
  Equity:Opening-Balances   5000 USD
`, entries)
	})
}

func TestGetOpenEntries(t *testing.T) {
	input := `
2011-01-01 open Assets:AccountA
2011-02-01 open Assets:AccountB
2011-03-01 close Assets:AccountA
2011-04-01 open Assets:AccountC
2011-05-01 open Assets:AccountA
`
	entries, _, _ := parser.ParseString(input)

	t.Run("before", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2011-01-01"))
		compareEntries(t, "", openEntries)
	})

	t.Run("first_entry_open", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2011-01-02"))
		compareEntries(t, "2011-01-01 open Assets:AccountA", openEntries)
	})

	t.Run("after_first_entry_open", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2011-02-02"))
		compareEntries(t, `
2011-01-01 open Assets:AccountA
2011-02-01 open Assets:AccountB
`, openEntries)
	})

	t.Run("first_close", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2011-03-02"))
		compareEntries(t, "2011-02-01 open Assets:AccountB", openEntries)
	})

	t.Run("after_first_close", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2011-04-02"))
		compareEntries(t, `
2011-02-01 open Assets:AccountB
2011-04-01 open Assets:AccountC
`, openEntries)
	})

	t.Run("after_new_opens", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2011-05-02"))
		compareEntries(t, `
2011-02-01 open Assets:AccountB
2011-04-01 open Assets:AccountC
2011-05-01 open Assets:AccountA
`, openEntries)
	})

	t.Run("after_all_opens", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, parseDate("2013-01-01"))
		compareEntries(t, `
2011-02-01 open Assets:AccountB
2011-04-01 open Assets:AccountC
2011-05-01 open Assets:AccountA
`, openEntries)
	})

	t.Run("after_all_entries", func(t *testing.T) {
		openEntries := GetOpenEntries(entries, time.Time{})
		compareEntries(t, `
2011-02-01 open Assets:AccountB
2011-04-01 open Assets:AccountC
2011-05-01 open Assets:AccountA
`, openEntries)
	})
}

func TestGetOpenEntries_DuplicateOpen(t *testing.T) {
	input := `
2011-01-01 open Assets:AccountA
2011-02-01 open Assets:AccountA
`
	entries, _, _ := parser.ParseString(input)
	openEntries := GetOpenEntries(entries, parseDate("2013-01-01"))
	// Should pick the first one.
	compareEntries(t, "2011-01-01 open Assets:AccountA", openEntries)
}

func TestGetOpenEntries_ClosedTwice(t *testing.T) {
	input := `
2011-01-01 open Assets:AccountA
2011-02-01 close Assets:AccountA
2011-03-01 close Assets:AccountA
`
	entries, _, _ := parser.ParseString(input)
	openEntries := GetOpenEntries(entries, parseDate("2013-01-01"))
	compareEntries(t, "", openEntries)
}

func TestGetOpenEntries_ClosedWithoutOpen(t *testing.T) {
	input := `
2011-02-02 close Assets:AccountA
`
	entries, _, _ := parser.ParseString(input)
	openEntries := GetOpenEntries(entries, parseDate("2013-01-01"))
	compareEntries(t, "", openEntries)
}
