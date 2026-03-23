package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/beancount/beancount/v3/core"
)

// Options holds all Beancount configuration.
type Options struct {
	Title                      string
	AccountTypes               core.AccountTypes
	AccountPreviousBalances    string
	AccountPreviousEarnings    string
	AccountPreviousConversions string
	AccountCurrentEarnings     string
	AccountCurrentConversions  string
	AccountUnrealizedGains     string
	AccountRounding            *string
	ConversionCurrency         string
	DisplayPrecision           map[string]string
	InferredToleranceDefault   map[string]core.Decimal
	ToleranceMultiplier        core.Decimal
	InferToleranceFromCost     bool
	Documents                  []string
	OperatingCurrency          []string
	RenderCommas               bool
	PluginProcessingMode       string
	LongStringMaxLines         int
	BookingMethod              core.Booking
	AllowPipeSeparator         bool
	AllowDeprecatedNone        bool
	InsertPythonPath           bool
}

// NewOptions returns an Options struct with default values.
func NewOptions() *Options {
	return &Options{
		Title: "Beancount",
		AccountTypes: core.AccountTypes{
			Assets:      "Assets",
			Liabilities: "Liabilities",
			Equity:      "Equity",
			Income:      "Income",
			Expenses:    "Expenses",
		},
		AccountPreviousBalances:    "Opening-Balances",
		AccountPreviousEarnings:    "Earnings:Previous",
		AccountPreviousConversions: "Conversions:Previous",
		AccountCurrentEarnings:     "Earnings:Current",
		AccountCurrentConversions:  "Conversions:Current",
		AccountUnrealizedGains:     "Earnings:Unrealized",
		ConversionCurrency:         "NOTHING",
		DisplayPrecision:           make(map[string]string),
		InferredToleranceDefault:   make(map[string]core.Decimal),
		ToleranceMultiplier:        core.D("0.5"),
		InferToleranceFromCost:     false,
		Documents:                  []string{},
		OperatingCurrency:          []string{},
		RenderCommas:               false,
		PluginProcessingMode:       "default",
		LongStringMaxLines:         64,
		BookingMethod:              core.BookingStrict,
		AllowPipeSeparator:         false,
		AllowDeprecatedNone:        false,
		InsertPythonPath:           false,
	}
}

// ProcessOptions validates raw options and populates the Options struct.
func ProcessOptions(rawOptions map[string][]string) (*Options, []error) {
	opts := NewOptions()
	var errs []error

	for key, vals := range rawOptions {
		if len(vals) == 0 {
			continue
		}
		val := vals[len(vals)-1] // Take the last value by default, except for list-like options

		switch key {
		case "title":
			opts.Title = val
		case "name_assets":
			opts.AccountTypes.Assets = val
		case "name_liabilities":
			opts.AccountTypes.Liabilities = val
		case "name_equity":
			opts.AccountTypes.Equity = val
		case "name_income":
			opts.AccountTypes.Income = val
		case "name_expenses":
			opts.AccountTypes.Expenses = val
		case "account_previous_balances":
			opts.AccountPreviousBalances = val
		case "account_previous_earnings":
			opts.AccountPreviousEarnings = val
		case "account_previous_conversions":
			opts.AccountPreviousConversions = val
		case "account_current_earnings":
			opts.AccountCurrentEarnings = val
		case "account_current_conversions":
			opts.AccountCurrentConversions = val
		case "account_unrealized_gains":
			opts.AccountUnrealizedGains = val
		case "account_rounding":
			opts.AccountRounding = &val
		case "conversion_currency":
			opts.ConversionCurrency = val
		case "display_precision":
			for _, v := range vals {
				parts := strings.SplitN(v, ":", 2)
				if len(parts) == 2 {
					opts.DisplayPrecision[parts[0]] = parts[1]
				} else {
					errs = append(errs, fmt.Errorf("invalid display_precision value: %s", v))
				}
			}
		case "inferred_tolerance_default":
			for _, v := range vals {
				parts := strings.SplitN(v, ":", 2)
				if len(parts) == 2 {
					opts.InferredToleranceDefault[parts[0]] = core.D(parts[1])
				} else {
					errs = append(errs, fmt.Errorf("invalid inferred_tolerance_default value: %s", v))
				}
			}
		case "tolerance_multiplier":
			opts.ToleranceMultiplier = core.D(val)
		case "inferred_tolerance_multiplier":
			// Alias for tolerance_multiplier
			opts.ToleranceMultiplier = core.D(val)
		case "infer_tolerance_from_cost":
			opts.InferToleranceFromCost = parseBool(val)
		case "documents":
			opts.Documents = append(opts.Documents, vals...)
		case "operating_currency":
			opts.OperatingCurrency = append(opts.OperatingCurrency, vals...)
		case "render_commas":
			opts.RenderCommas = parseBool(val)
		case "plugin_processing_mode":
			if val == "raw" || val == "default" {
				opts.PluginProcessingMode = val
			} else {
				errs = append(errs, fmt.Errorf("invalid plugin_processing_mode: %s", val))
			}
		case "long_string_maxlines":
			if i, err := strconv.Atoi(val); err == nil {
				opts.LongStringMaxLines = i
			} else {
				errs = append(errs, fmt.Errorf("invalid long_string_maxlines: %s", val))
			}
		case "booking_method":
			method := core.Booking(strings.ToUpper(val))
			switch method {
			case core.BookingStrict, core.BookingStrictWithSize, core.BookingNone,
				core.BookingAverage, core.BookingFIFO, core.BookingLIFO, core.BookingHIFO:
				opts.BookingMethod = method
			default:
				errs = append(errs, fmt.Errorf("invalid booking_method: %s", val))
			}
		case "allow_pipe_separator":
			opts.AllowPipeSeparator = parseBool(val)
		case "allow_deprecated_none_for_tags_and_links":
			opts.AllowDeprecatedNone = parseBool(val)
		case "insert_pythonpath":
			opts.InsertPythonPath = parseBool(val)
		}
	}

	return opts, errs
}

func parseBool(s string) bool {
	s = strings.ToLower(s)
	return s == "true" || s == "yes" || s == "1"
}
