package parser

import (
	"strings"
	"testing"
	"time"

	"github.com/beancount/beancount/v3/core"
)

func TestPrintDirectives(t *testing.T) {
	d1 := &core.Open{
		Date:       time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
		Account:    "Assets:Checking",
		Currencies: []string{"USD", "EUR"},
	}

	d2 := &core.Commodity{
		Date:     time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
		Currency: "USD",
		Meta:     core.Meta{"name": "US Dollar"},
	}

	d3 := &core.Transaction{
		Date:      time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
		Flag:      "*",
		Payee:     "Grocery Store",
		Narration: "Weekly groceries",
		Tags:      map[string]struct{}{"food": {}},
		Postings: []core.Posting{
			{
				Account: "Expenses:Food",
				Units:   core.NewAmount(core.D("50"), "USD"),
				Meta:    core.Meta{"category": "vegetables"},
			},
			{
				Account: "Assets:Checking",
				Units:   core.NewAmount(core.D("-50"), "USD"),
			},
		},
	}

	d4 := &core.Price{
		Date:     time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
		Currency: "EUR",
		Amount:   core.NewAmount(core.D("1.1"), "USD"),
	}

	directives := []core.Directive{d1, d2, d3, d4}
	output := PrintDirectives(directives)

	expectedParts := []string{
		"2023-01-01 open Assets:Checking USD, EUR",
		"2023-01-01 commodity USD",
		"name: US Dollar",
		"2023-01-01 * \"Grocery Store\" \"Weekly groceries\" #food",
		"Expenses:Food        50 USD",
		"category: vegetables",
		"Assets:Checking      -50 USD",
		"2023-01-01 price EUR 1.1 USD",
	}

	for _, part := range expectedParts {
		if !strings.Contains(output, part) {
			t.Errorf("Expected part missing or misformatted:\n%q\nOutput:\n%s", part, output)
		}
	}
}

func TestPrintTransactionWithCostAndPrice(t *testing.T) {
	txn := &core.Transaction{
		Date:      time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
		Flag:      "*",
		Narration: "Buying stock",
		Postings: []core.Posting{
			{
				Account: "Assets:Brokerage",
				Units:   core.NewAmount(core.D("10"), "HOOL"),
				Cost: &core.Cost{
					Number:   core.D("500"),
					Currency: "USD",
					Date:     time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
				},
				Price: core.NewAmount(core.D("510"), "USD"),
			},
			{
				Account: "Assets:Checking",
				Units:   core.NewAmount(core.D("-5000"), "USD"),
			},
		},
	}

	output := PrintDirectives([]core.Directive{txn})
	expected := "Assets:Brokerage     10 HOOL {500 USD, 2023-01-01} @ 510 USD"
	if !strings.Contains(output, expected) {
		t.Errorf("Transaction with cost and price misformatted:\n%s", output)
	}
}

func TestPrintBalance(t *testing.T) {
	b := &core.Balance{
		Date:    time.Date(2023, 1, 1, 0, 0, 0, 0, time.UTC),
		Account: "Assets:Checking",
		Amount:  core.NewAmount(core.D("1000"), "USD"),
	}

	output := PrintDirectives([]core.Directive{b})
	if !strings.Contains(output, "2023-01-01 balance Assets:Checking 1000 USD") {
		t.Errorf("Balance directive misformatted:\n%s", output)
	}
}

func TestAlignPositionStrings(t *testing.T) {
	p1 := core.Posting{Account: "Assets:Cash", Units: core.NewAmount(core.D("100"), "USD")}
	p2 := core.Posting{Account: "Expenses:Food", Units: core.NewAmount(core.D("50.25"), "USD")}

	s1 := PrintPosting(p1)
	s2 := PrintPosting(p2)

	if !strings.Contains(s1, "Assets:Cash          100 USD") || !strings.Contains(s2, "Expenses:Food        50.25 USD") {
		t.Errorf("Postings misformatted:\n%s\n%s", s1, s2)
	}
}
