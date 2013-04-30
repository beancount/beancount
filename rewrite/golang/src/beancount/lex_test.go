// Unit tests for the Beancount 2.0 input syntax lexer.

package beancount

import (
	"testing"
	"regexp"
	"fmt"
)

// Make the types prettyprint.
var Name = map[TokenType]string{
	ERROR			: "ERROR",
	INDENT		: "INDENT",
	EOL				: "EOL",
	EOF				: "OF",
	PIPE			: "PIPE",
	ATAT			: "ATAT",
	AT				: "AT",
	LCURL			: "LCURL",
	RCURL			: "RCURL",
	EQUAL			: "EQUAL",
	COMMA			: "COMMA",
	SLASH			: "SLASH",
	DATE			: "DATE",
	CURRENCY	: "CURRENCY",
	STRING		: "STRING",
	NUMBER		: "NUMBER",
	COMMENT		: "COMMENT",
  TXN				: "TXN",
  TXNFLAG		: "TXNFLAG",
  CHECK			: "CHECK",
  OPEN			: "OPEN",
  CLOSE			: "CLOSE",
  PAD				: "PAD",
  EVENT			: "EVENT",
  PRICE			: "PRICE",
  LOCATION	: "LOCATION",
  BEGINTAG	: "BEGINTAG",
  ENDTAG		: "ENDTAG",
}

type lexTest struct {
	name  string
	input string
	items []item
}

var (
	tEOL      = item{EOL, 0, "\n"}
	tEOF      = item{EOF, 0, ""}
	tINDENT      = item{INDENT, 0, "  "}
	// tCOMMENT      = item{COMMENT, 0, "COMMENT"}
	// tFor      = item{Identifier, 0, "for"}
	// tLeft     = item{LeftDelim, 0, "{{"}
	// tLpar     = item{LeftParen, 0, "("}
	// tPipe     = item{PIPE, 0, "|"}
	// tQuote    = item{STRING, 0, `"abc \n\t\" "`}
	// tRange    = item{Range, 0, "range"}
	// tRight    = item{RightDelim, 0, "}}"}
	// tRpar     = item{RightParen, 0, ")"}
	// tSpace    = item{Space, 0, " "}
	// raw       = "`" + `abc\n\t\" ` + "`"
	// tRawQuote = item{RawString, 0, raw}
)

var lexTests = []lexTest{

	{"empty", "", []item{
		tEOF}},
	{"empty_ln", "\n", []item{
		tEOL,
		tEOF}},
	{"empty_lines", "\n\n", []item{
		tEOL,
		tEOL,
		tEOF}},

	{"indent", "  ", []item{
		tINDENT,
		tEOF}},
	{"indent_nl", "  \n", []item{
		tINDENT,
		tEOL,
		tEOF}},

	{"comment", "; Bla-di-bla", []item{
		//{COMMENT, 0, " Bla-di-bla"},
		tEOF}},
	{"comment_nl", "; Bla-di-bla\n", []item{
		//{COMMENT, 0, " Bla-di-bla"},
		tEOL,
		tEOF}},
	{"indent_comment_nl", "  ; Bla-di-bla\n", []item{
		tINDENT,
		//{COMMENT, 0, " Bla-di-bla"},
		tEOL,
		tEOF}},

	{"date", "2013-04-13", []item{
		{DATE, 0, "2013-04-13"},
		tEOF}},
	{"date_space", "2013-04-13  \n", []item{
		{DATE, 0, "2013-04-13"},
		tEOL,
		tEOF}},

	{"date_flag", "2013-04-13 *", []item{
		{DATE, 0, "2013-04-13"},
		{TXNFLAG, 0, "*"},
		tEOF}},
	{"date_flag", "2013-04-13 %", []item{
		{DATE, 0, "2013-04-13"},
		{TXNFLAG, 0, "%"},
		tEOF}},

	{"flag", "!", []item{{TXNFLAG, 0, "!"}, tEOF}},
	{"flag", "&", []item{{TXNFLAG, 0, "&"}, tEOF}},
	{"flag", "#", []item{{TXNFLAG, 0, "#"}, tEOF}},
	{"flag", "%", []item{{TXNFLAG, 0, "%"}, tEOF}},

	{"pipe", "|", []item{{PIPE, 0, "|"}, tEOF}},
	{"at", "@", []item{{AT, 0, "@"}, tEOF}},
	{"atat", "@@", []item{{ATAT, 0, "@@"}, tEOF}},
	{"lcurl", "{", []item{{LCURL, 0, "{"}, tEOF}},
	{"rcurl", "}", []item{{RCURL, 0, "}"}, tEOF}},
	{"equal", "=", []item{{EQUAL, 0, "="}, tEOF}},
	{"comma", ",", []item{{COMMA, 0, ","}, tEOF}},

	{"string", "\"Bla-di-bla\"", []item{{STRING, 0, "Bla-di-bla"}, tEOF}},
	{"string", "\"Bla\" \"di\" \"bla\"", []item{
		{STRING, 0, "Bla"},
		{STRING, 0, "di"},
		{STRING, 0, "bla"},
		tEOF}},

	{"account", "Assets:US:HSBC", []item{{ACCOUNT, 0, "Assets:US:HSBC"}, tEOF}},
	{"account", "Assets:CA:RBC-Investing", []item{{ACCOUNT, 0, "Assets:CA:RBC-Investing"}, tEOF}},
	{"account", "Assets:CA:RBC_Investing", []item{{ACCOUNT, 0, "Assets:CA:RBC_Investing"}, tEOF}},

	{"currency", "USD", []item{{CURRENCY, 0, "USD"}, tEOF}},
	{"currency", "CAD", []item{{CURRENCY, 0, "CAD"}, tEOF}},
	{"currency", "RBF1001", []item{{CURRENCY, 0, "RBF1001"}, tEOF}},
	{"currency", "AIS512", []item{{CURRENCY, 0, "AIS512"}, tEOF}},

	{"number", "12345", []item{{NUMBER, 0, "12345"}, tEOF}},
	{"number", "123.45", []item{{NUMBER, 0, "123.45"}, tEOF}},
	{"number", "876.6433", []item{{NUMBER, 0, "876.6433"}, tEOF}},
	{"number", "-876.6433", []item{{NUMBER, 0, "-876.6433"}, tEOF}},
	{"number", "+876.6433", []item{{NUMBER, 0, "+876.6433"}, tEOF}},
	{"number", "-876", []item{{NUMBER, 0, "-876"}, tEOF}},
	{"number", "+876", []item{{NUMBER, 0, "+876"}, tEOF}},

	{"inventory_price_only", "987.65 GOOG @ 704.01 USD", []item{
		{NUMBER, 0, "987.65"},
		{CURRENCY, 0, "GOOG"},
		{AT, 0, "@"},
		{NUMBER, 0, "704.01"},
		{CURRENCY, 0, "USD"},
		tEOF}},

	{"inventory_cost_only", "987.65 GOOG {765.03 USD}", []item{
		{NUMBER, 0, "987.65"},
		{CURRENCY, 0, "GOOG"},
		{LCURL, 0, "{"},
		{NUMBER, 0, "765.03"},
		{CURRENCY, 0, "USD"},
		{RCURL, 0, "}"},
		tEOF}},

	{"inventory_cost_with_date", "987.65 GOOG {765.03 USD / 2013-04-18}", []item{
		{NUMBER, 0, "987.65"},
		{CURRENCY, 0, "GOOG"},
		{LCURL, 0, "{"},
		{NUMBER, 0, "765.03"},
		{CURRENCY, 0, "USD"},
		{SLASH, 0, "/"},
		{DATE, 0, "2013-04-18"},
		{RCURL, 0, "}"},
		tEOF}},

	{"inventory_cost_and_price", "987.65 GOOG {765.03 USD} @ 704.01 USD", []item{
		{NUMBER, 0, "987.65"},
		{CURRENCY, 0, "GOOG"},
		{LCURL, 0, "{"},
		{NUMBER, 0, "765.03"},
		{CURRENCY, 0, "USD"},
		{RCURL, 0, "}"},
		{AT, 0, "@"},
		{NUMBER, 0, "704.01"},
		{CURRENCY, 0, "USD"},
		tEOF}},

	// {"inventory", "987.65 GOOG {765.03 USD} @ 704.01 USD", []item{
	// 	{NUMBER, 0, "987.65"},
	// 	{CURRENCY, 0, "GOOG"},
	// 	tEOF}},

	{"directive", "open", []item{{OPEN, 0, "open"}, tEOF}},
	{"directive", "close", []item{{CLOSE, 0, "close"}, tEOF}},
}

// 'collect' gathers the emitted items into a slice.
func collect(t *lexTest) (items []item) {
	l := MakeLexer(t.name, t.input)
	for {
		item := l.NextTok()
		items = append(items, item)
		if item.Type == EOF || item.Type == ERROR {
			break
		}
	}
	return
}

func equal(i1, i2 []item, checkPos bool) bool {
	if len(i1) != len(i2) {
		return false
	}
	for k := range i1 {
		if i1[k].Type != i2[k].Type {
			return false
		}
		if i1[k].val != i2[k].val {
			return false
		}
		if checkPos && i1[k].pos != i2[k].pos {
			return false
		}
	}
	return true
}

func TestLex(t *testing.T) {
	for _, test := range lexTests {
		items := collect(&test)
		if !equal(items, test.items, false) {
			t.Errorf("%s: got\n\t%+v\nexpected\n\t%v", test.name, items, test.items)
		}
	}
}



func TestRegexps(t *testing.T) {
	dateRegexp, _ = regexp.Compile("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
	fmt.Printf("test 1: %v\n", dateRegexp.MatchString("2007-04-04"))
	fmt.Printf("test 2: %v\n", dateRegexp.MatchString("0 CAD\n\n\n20"))
	fmt.Printf("test 3: %v\n", dateRegexp.MatchString("0 CAD\n\n\n2007-04-04"))
}
