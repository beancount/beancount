// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package beancount

import (
	"fmt"
	"testing"
)

// Make the types prettyprint.
var itemName = map[itemType]string{
	itemError			: "ERROR",
	itemIndent		: "INDENT",
	itemEOL				: "EOL",
	itemEOF				: "OF",
	itemPipe			: "PIPE",
	itemAtAt			: "ATAT",
	itemAt				: "AT",
	itemLCurl			: "LCURL",
	itemRCurl			: "RCURL",
	itemEqual			: "EQUAL",
	itemComma			: "COMMA",
	itemDate			: "DATE",
	itemCurrency	: "CURRENCY",
	itemString		: "STRING",
	itemNumber		: "NUMBER",
	itemComment		: "COMMENT",
  itemTXN				: "TXN",
  itemTXNFLAG		: "TXNFLAG",
  itemCHECK			: "CHECK",
  itemOPEN			: "OPEN",
  itemCLOSE			: "CLOSE",
  itemPAD				: "PAD",
  itemEVENT			: "EVENT",
  itemPRICE			: "PRICE",
  itemLOCATION	: "LOCATION",
  itemBEGINTAG	: "BEGINTAG",
  itemENDTAG		: "ENDTAG",
}

func (i itemType) String() string {
	s := itemName[i]
	if s == "" {
		return fmt.Sprintf("item(%d)", int(i))
	}
	return s
}

type lexTest struct {
	name  string
	input string
	items []item
}

var (
	tEOL      = item{itemEOL, 0, "\n"}
	tEOF      = item{itemEOF, 0, ""}
	tINDENT      = item{itemIndent, 0, "  "}
	// tCOMMENT      = item{itemComment, 0, "COMMENT"}
	// tFor      = item{itemIdentifier, 0, "for"}
	// tLeft     = item{itemLeftDelim, 0, "{{"}
	// tLpar     = item{itemLeftParen, 0, "("}
	// tPipe     = item{itemPipe, 0, "|"}
	// tQuote    = item{itemString, 0, `"abc \n\t\" "`}
	// tRange    = item{itemRange, 0, "range"}
	// tRight    = item{itemRightDelim, 0, "}}"}
	// tRpar     = item{itemRightParen, 0, ")"}
	// tSpace    = item{itemSpace, 0, " "}
	// raw       = "`" + `abc\n\t\" ` + "`"
	// tRawQuote = item{itemRawString, 0, raw}
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
		{itemComment, 0, " Bla-di-bla"},
		tEOF}},
	{"comment_nl", "; Bla-di-bla\n", []item{
		{itemComment, 0, " Bla-di-bla"},
		tEOL,
		tEOF}},
	{"indent_comment_nl", "  ; Bla-di-bla\n", []item{
		tINDENT,
		{itemComment, 0, " Bla-di-bla"},
		tEOL,
		tEOF}},

	{"date", "2013-04-13", []item{
		{itemDate, 0, "2013-04-13"},
		tEOF}},
	{"date_space", "2013-04-13  \n", []item{
		{itemDate, 0, "2013-04-13"},
		tEOL,
		tEOF}},

	{"date_flag", "2013-04-13 *", []item{
		{itemDate, 0, "2013-04-13"},
		{itemTXNFLAG, 0, "*"},
		tEOF}},
	{"date_flag", "2013-04-13 %", []item{
		{itemDate, 0, "2013-04-13"},
		{itemTXNFLAG, 0, "%"},
		tEOF}},

	{"flag", "*", []item{{itemTXNFLAG, 0, "*"}, tEOF}},
	{"flag", "!", []item{{itemTXNFLAG, 0, "!"}, tEOF}},
	{"flag", "&", []item{{itemTXNFLAG, 0, "&"}, tEOF}},
	{"flag", "#", []item{{itemTXNFLAG, 0, "#"}, tEOF}},
	{"flag", "%", []item{{itemTXNFLAG, 0, "%"}, tEOF}},

	{"pipe", "|", []item{{itemPipe, 0, "|"}, tEOF}},
	{"at", "@", []item{{itemAt, 0, "@"}, tEOF}},
	{"atat", "@@", []item{{itemAtAt, 0, "@@"}, tEOF}},
	{"lcurl", "{", []item{{itemLCurl, 0, "{"}, tEOF}},
	{"rcurl", "}", []item{{itemRCurl, 0, "}"}, tEOF}},
	{"equal", "=", []item{{itemEqual, 0, "="}, tEOF}},
	{"comma", ",", []item{{itemComma, 0, ","}, tEOF}},

	{"string", "\"Bla-di-bla\"", []item{{itemString, 0, "Bla-di-bla"}, tEOF}},
	{"string", "\"Bla\" \"di\" \"bla\"", []item{
		{itemString, 0, "Bla"},
		{itemString, 0, "di"},
		{itemString, 0, "bla"},
		tEOF}},

	{"account", "Assets:US:HSBC", []item{{itemAccount, 0, "Assets:US:HSBC"}, tEOF}},
	{"account", "Assets:CA:RBC-Investing", []item{{itemAccount, 0, "Assets:CA:RBC-Investing"}, tEOF}},
	{"account", "Assets:CA:RBC_Investing", []item{{itemAccount, 0, "Assets:CA:RBC_Investing"}, tEOF}},

	{"currency", "USD", []item{{itemCurrency, 0, "USD"}, tEOF}},
	{"currency", "CAD", []item{{itemCurrency, 0, "CAD"}, tEOF}},
	{"currency", "RBF1001", []item{{itemCurrency, 0, "RBF1001"}, tEOF}},
	{"currency", "AIS512", []item{{itemCurrency, 0, "AIS512"}, tEOF}},

	{"number", "12345", []item{{itemNumber, 0, "12345"}, tEOF}},
	{"number", "123.45", []item{{itemNumber, 0, "123.45"}, tEOF}},
	{"number", "876.6433", []item{{itemNumber, 0, "876.6433"}, tEOF}},
	{"number", "-876.6433", []item{{itemNumber, 0, "-876.6433"}, tEOF}},
	{"number", "+876.6433", []item{{itemNumber, 0, "+876.6433"}, tEOF}},
	{"number", "-876", []item{{itemNumber, 0, "-876"}, tEOF}},
	{"number", "+876", []item{{itemNumber, 0, "+876"}, tEOF}},

	{"directive", "open", []item{{itemOPEN, 0, "open"}, tEOF}},
	{"directive", "close", []item{{itemCLOSE, 0, "close"}, tEOF}},
}

// 'collect' gathers the emitted items into a slice.
func collect(t *lexTest, left, right string) (items []item) {
	l := lex(t.name, t.input, left, right)
	for {
		item := l.nextItem()
		items = append(items, item)
		if item.typ == itemEOF || item.typ == itemError {
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
		if i1[k].typ != i2[k].typ {
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
		items := collect(&test, "", "")
		if !equal(items, test.items, false) {
			t.Errorf("%s: got\n\t%+v\nexpected\n\t%v", test.name, items, test.items)
		}
	}
}
