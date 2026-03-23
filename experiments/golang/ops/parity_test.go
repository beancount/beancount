package ops

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
	"github.com/beancount/beancount/v3/parser"
)

func TestBalanceParity(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		wantErr bool
		diffAmt string
	}{
		{
			"simple_error",
			`
2014-01-01 open Assets:Checking
2014-01-02 *
  Assets:Checking  100.00 USD
  Income:Salary
2014-01-03 balance Assets:Checking 110.00 USD
`,
			true,
			"10.00",
		},
		{
			"simple_first",
			`
2014-01-01 open Assets:Checking
2014-01-03 balance Assets:Checking 0.00 USD
`,
			false,
			"",
		},
		{
			"simple_cont",
			`
2014-01-01 open Assets:Checking
2014-01-02 *
  Assets:Checking  100.00 USD
  Income:Salary
2014-01-03 balance Assets:Checking 100.00 USD
`,
			false,
			"",
		},
		{
			"simple_partial_currency_first",
			`
2014-01-01 open Assets:Checking
2014-01-02 *
  Assets:Checking  100.00 USD
  Income:Salary
2014-01-03 balance Assets:Checking 100.00 USD
2014-01-04 *
  Assets:Checking  50.00 CAD
  Income:Salary
2014-01-05 balance Assets:Checking 100.00 USD
`,
			false,
			"",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			directives, errs, rawOptions := parser.ParseString(tt.input)
			if len(errs) > 0 {
				t.Fatalf("Parse errors: %v", errs)
			}
			options, _ := parser.ProcessOptions(rawOptions)
			booked, _ := parser.Book(directives, options)

			balErrors := CheckBalances(booked, options)
			if tt.wantErr {
				if len(balErrors) == 0 {
					t.Errorf("Expected balance error, got none")
				} else {
					if tt.diffAmt != "" {
						found := false
						for _, e := range balErrors {
							if be, ok := e.(BalanceError); ok {
								found = true
								if be.Diff.Number.Cmp(core.D(tt.diffAmt)) != 0 {
									t.Errorf("Expected diff amount %v, got %v", tt.diffAmt, be.Diff.Number)
								}
							}
						}
						if !found {
							t.Errorf("Could not find BalanceError")
						}
					}
				}
			} else {
				if len(balErrors) > 0 {
					t.Errorf("Unexpected balance errors: %v", balErrors)
				}
			}
		})
	}
}
