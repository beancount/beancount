package core

import (
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/ericlagergren/decimal"
)

// Meta represents the arbitrary key-value pairs attached to entries.
type Meta map[string]interface{}

// NewMetadata creates a new metadata container.
func NewMetadata(filename string, lineno int) Meta {
	return Meta{
		"filename": filename,
		"lineno":   lineno,
	}
}

// Decimal is a type alias for *decimal.Big to maintain consistency.
// Note: This is already in core/number.go, but we need it here for type references if not imported.
// Actually, data.go is in the same package 'core'.

// Cost represents the cost of a lot.
type Cost struct {
	Number   Decimal
	Currency string
	Date     time.Time
	Label    string
}

// String returns a human-readable string representation of the cost.
func (c *Cost) String() string {
	if c == nil {
		return ""
	}
	res := Format(c.Number) + " " + c.Currency
	var parts []string
	if !c.Date.IsZero() {
		parts = append(parts, c.Date.Format("2006-01-02"))
	}
	if c.Label != "" {
		parts = append(parts, fmt.Sprintf("%q", c.Label))
	}
	if len(parts) > 0 {
		res += ", " + strings.Join(parts, ", ")
	}
	return "{" + res + "}"
}

// CostSpec represents an incomplete cost specification.
type CostSpec struct {
	NumberPer   Decimal
	NumberTotal Decimal
	Currency    string
	Date        time.Time
	Label       string
	Merge       bool
}

// DirectiveType represents the type of a directive.
type DirectiveType int

const (
	TypeOpen DirectiveType = iota
	TypeClose
	TypeCommodity
	TypePad
	TypeBalance
	TypeTransaction
	TypeNote
	TypeEvent
	TypeQuery
	TypePrice
	TypeDocument
	TypeCustom
)

// Directive is the interface that all Beancount entries must implement.
type Directive interface {
	GetDate() time.Time
	GetMeta() Meta
	Type() DirectiveType
}

// Booking represents the booking method for an account.
type Booking string

const (
	BookingStrict         Booking = "STRICT"
	BookingStrictWithSize Booking = "STRICT_WITH_SIZE"
	BookingNone           Booking = "NONE"
	BookingAverage        Booking = "AVERAGE"
	BookingFIFO           Booking = "FIFO"
	BookingLIFO           Booking = "LIFO"
	BookingHIFO           Booking = "HIFO"
)

// Open represents an "open account" directive.
type Open struct {
	Meta       Meta
	Date       time.Time
	Account    string
	Currencies []string
	Booking    Booking
}

func (d Open) GetDate() time.Time   { return d.Date }
func (d Open) GetMeta() Meta        { return d.Meta }
func (d Open) Type() DirectiveType { return TypeOpen }

// Close represents a "close account" directive.
type Close struct {
	Meta    Meta
	Date    time.Time
	Account string
}

func (d Close) GetDate() time.Time   { return d.Date }
func (d Close) GetMeta() Meta        { return d.Meta }
func (d Close) Type() DirectiveType { return TypeClose }

// Commodity represents a commodity declaration directive.
type Commodity struct {
	Meta     Meta
	Date     time.Time
	Currency string
}

func (d Commodity) GetDate() time.Time   { return d.Date }
func (d Commodity) GetMeta() Meta        { return d.Meta }
func (d Commodity) Type() DirectiveType { return TypeCommodity }

// Pad represents a "pad account" directive.
type Pad struct {
	Meta          Meta
	Date          time.Time
	Account       string
	SourceAccount string
}

func (d Pad) GetDate() time.Time   { return d.Date }
func (d Pad) GetMeta() Meta        { return d.Meta }
func (d Pad) Type() DirectiveType { return TypePad }

// Balance represents a "check balance" directive.
type Balance struct {
	Meta       Meta
	Date       time.Time
	Account    string
	Amount     Amount
	Tolerance  Decimal
	DiffAmount Amount
}

func (d Balance) GetDate() time.Time   { return d.Date }
func (d Balance) GetMeta() Meta        { return d.Meta }
func (d Balance) Type() DirectiveType { return TypeBalance }

// Posting represents a single leg of a transaction.
type Posting struct {
	Account  string
	Units    Amount
	Cost     *Cost
	CostSpec *CostSpec
	Price    Amount
	Flag     string
	Meta     Meta
}

// Transaction represents a financial transaction.
type Transaction struct {
	Meta      Meta
	Date      time.Time
	Flag      string
	Payee     string
	Narration string
	Tags      map[string]struct{}
	Links     map[string]struct{}
	Postings  []Posting
}

func (d Transaction) GetDate() time.Time   { return d.Date }
func (d Transaction) GetMeta() Meta        { return d.Meta }
func (d Transaction) Type() DirectiveType { return TypeTransaction }

// TxnPosting pairs a Posting with its parent Transaction.
type TxnPosting struct {
	Txn     *Transaction
	Posting *Posting
}

// Note represents a general note attached to an account.
type Note struct {
	Meta    Meta
	Date    time.Time
	Account string
	Comment string
}

func (d Note) GetDate() time.Time   { return d.Date }
func (d Note) GetMeta() Meta        { return d.Meta }
func (d Note) Type() DirectiveType { return TypeNote }

// Event represents a variable value change over time.
type Event struct {
	Meta        Meta
	Date        time.Time
	EventType   string
	Description string
}

func (d Event) GetDate() time.Time   { return d.Date }
func (d Event) GetMeta() Meta        { return d.Meta }
func (d Event) Type() DirectiveType { return TypeEvent }

// Query represents a named query declaration.
type Query struct {
	Meta        Meta
	Date        time.Time
	Name        string
	QueryString string
}

func (d Query) GetDate() time.Time   { return d.Date }
func (d Query) GetMeta() Meta        { return d.Meta }
func (d Query) Type() DirectiveType { return TypeQuery }

// Price represents a price declaration directive.
type Price struct {
	Meta     Meta
	Date     time.Time
	Currency string
	Amount   Amount
}

func (d Price) GetDate() time.Time   { return d.Date }
func (d Price) GetMeta() Meta        { return d.Meta }
func (d Price) Type() DirectiveType { return TypePrice }

// Document represents a document file declaration directive.
type Document struct {
	Meta     Meta
	Date     time.Time
	Account  string
	Filename string
	Tags     map[string]struct{}
	Links    map[string]struct{}
}

func (d Document) GetDate() time.Time   { return d.Date }
func (d Document) GetMeta() Meta        { return d.Meta }
func (d Document) Type() DirectiveType { return TypeDocument }

// Custom represents a custom directive for experimental features.
type Custom struct {
	Meta    Meta
	Date    time.Time
	TypeStr string
	Values  []interface{}
}

func (d Custom) GetDate() time.Time   { return d.Date }
func (d Custom) GetMeta() Meta        { return d.Meta }
func (d Custom) Type() DirectiveType { return TypeCustom }

// PostingHasConversion returns true if this posting involves a conversion.
func PostingHasConversion(p Posting) bool {
	return p.Cost == nil && p.Price.Number != nil && p.Price.Number.Sign() != 0
}

// TransactionHasConversion returns true if any posting has a conversion.
func TransactionHasConversion(t *Transaction) bool {
	for _, p := range t.Postings {
		if PostingHasConversion(p) {
			return true
		}
	}
	return false
}

// GetWeightPosting returns the weight of the posting.
func GetWeightPosting(p Posting) Amount {
	if p.Cost != nil {
		res := new(decimal.Big)
		DefaultContext.Mul(res, p.Units.Number, p.Cost.Number)
		return NewAmount(res, p.Cost.Currency)
	}
	if p.Price.Number != nil {
		res := new(decimal.Big)
		DefaultContext.Mul(res, p.Units.Number, p.Price.Number)
		return NewAmount(res, p.Price.Currency)
	}
	return p.Units
}

// Sorting order of directives on the same day.
var TypeOrder = map[DirectiveType]int{
	TypeOpen:        -2,
	TypeBalance:     -1,
	TypeTransaction: 0,
	TypeNote:        0,
	TypeEvent:       0,
	TypeQuery:       0,
	TypePrice:       0,
	TypeDocument:    1,
	TypeClose:       2,
}

// DirectiveSortKey returns a value used for sorting directives.
type DirectiveSortKey struct {
	Date   time.Time
	Order  int
	LineNo int
}

func GetDirectiveSortKey(d Directive) DirectiveSortKey {
	lineNo := 0
	if l, ok := d.GetMeta()["lineno"].(int); ok {
		lineNo = l
	}
	return DirectiveSortKey{
		Date:   d.GetDate(),
		Order:  TypeOrder[d.Type()],
		LineNo: lineNo,
	}
}

func CompareDirectiveSortKeys(k1, k2 DirectiveSortKey) int {
	if !k1.Date.Equal(k2.Date) {
		if k1.Date.Before(k2.Date) {
			return -1
		}
		return 1
	}
	if k1.Order != k2.Order {
		if k1.Order < k2.Order {
			return -1
		}
		return 1
	}
	if k1.LineNo != k2.LineNo {
		if k1.LineNo < k2.LineNo {
			return -1
		}
		return 1
	}
	return 0
}

// SortDirectives sorts a slice of directives.
func SortDirectives(directives []Directive) {
	sort.Slice(directives, func(i, j int) bool {
		ki := GetDirectiveSortKey(directives[i])
		kj := GetDirectiveSortKey(directives[j])
		return CompareDirectiveSortKeys(ki, kj) < 0
	})
}

// FilterTransactions returns only the transaction directives.
func FilterTransactions(directives []Directive) []*Transaction {
	var txns []*Transaction
	for _, d := range directives {
		if t, ok := d.(*Transaction); ok {
			txns = append(txns, t)
		}
	}
	return txns
}

// CreateSimplePosting creates a simple posting and adds it to the transaction.
func CreateSimplePosting(txn *Transaction, account string, number string, currency string) Posting {
	posting := Posting{
		Account: account,
		Units:   NewAmount(D(number), currency),
	}
	txn.Postings = append(txn.Postings, posting)
	return posting
}

// CreateSimplePostingWithCost creates a simple posting with cost and adds it to the transaction.
func CreateSimplePostingWithCost(txn *Transaction, account string, number string, currency string, costNumber string, costCurrency string) Posting {
	posting := Posting{
		Account: account,
		Units:   NewAmount(D(number), currency),
		Cost:    &Cost{Number: D(costNumber), Currency: costCurrency},
	}
	txn.Postings = append(txn.Postings, posting)
	return posting
}

// SanityCheckTypes is a placeholder for Go's static typing.
func SanityCheckTypes(entry interface{}) {
	if _, ok := entry.(Directive); !ok {
		panic("Not a directive")
	}
}

// GetEntry returns the entry from a Directive or TxnPosting.
func GetEntry(entry interface{}) Directive {
	switch e := entry.(type) {
	case Directive:
		return e
	case TxnPosting:
		return e.Txn
	default:
		return nil
	}
}

// HasEntryAccountComponent returns true if any of the entry's accounts has the given component.
func HasEntryAccountComponent(entry Directive, component string) bool {
	var accounts []string
	switch e := entry.(type) {
	case *Open:
		accounts = append(accounts, e.Account)
	case *Close:
		accounts = append(accounts, e.Account)
	case *Pad:
		accounts = append(accounts, e.Account, e.SourceAccount)
	case *Balance:
		accounts = append(accounts, e.Account)
	case *Transaction:
		for _, p := range e.Postings {
			accounts = append(accounts, p.Account)
		}
	case *Note:
		accounts = append(accounts, e.Account)
	case *Document:
		accounts = append(accounts, e.Account)
	}

	for _, account := range accounts {
		for _, part := range strings.Split(account, ":") {
			if part == component {
				return true
			}
		}
	}
	return false
}

// FindClosest finds the entry closest to the given line number in the given file.
func FindClosest(entries []Directive, filename string, lineno int) Directive {
	var closest Directive
	minDist := -1

	for _, entry := range entries {
		meta := entry.GetMeta()
		if meta["filename"] != filename {
			continue
		}
		entryLineno := meta["lineno"].(int)
		if entryLineno > lineno {
			continue
		}
		dist := lineno - entryLineno
		if minDist == -1 || dist < minDist {
			minDist = dist
			closest = entry
		}
	}
	return closest
}

// RemoveAccountPostings removes postings to the given account from all transactions.
func RemoveAccountPostings(account string, entries []Directive) []Directive {
	var result []Directive
	for _, entry := range entries {
		if txn, ok := entry.(*Transaction); ok {
			var newPostings []Posting
			for _, p := range txn.Postings {
				if p.Account != account {
					newPostings = append(newPostings, p)
				}
			}
			txn.Postings = newPostings
		}
		result = append(result, entry)
	}
	return result
}

// IterEntryDates iterates over entries within a date range [beginDate, endDate).
func IterEntryDates(entries []Directive, beginDate, endDate time.Time) []Directive {
	var result []Directive
	for _, entry := range entries {
		date := entry.GetDate()
		if (date.After(beginDate) || date.Equal(beginDate)) && date.Before(endDate) {
			result = append(result, entry)
		}
	}
	return result
}
