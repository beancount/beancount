// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package beancount

import (
	"testing"
	"regexp"
	"fmt"
)

// Make the types prettyprint.
var TokName = map[TokenType]string{
	TokERROR			: "ERROR",
	TokINDENT		: "INDENT",
	TokEOL				: "EOL",
	TokEOF				: "OF",
	TokPIPE			: "PIPE",
	TokATAT			: "ATAT",
	TokAT				: "AT",
	TokLCURL			: "LCURL",
	TokRCURL			: "RCURL",
	TokEQUAL			: "EQUAL",
	TokCOMMA			: "COMMA",
	TokDATE			: "DATE",
	TokCURRENCY	: "CURRENCY",
	TokSTRING		: "STRING",
	TokNUMBER		: "NUMBER",
	TokCOMMENT		: "COMMENT",
  TokTXN				: "TXN",
  TokTXNFLAG		: "TXNFLAG",
  TokCHECK			: "CHECK",
  TokOPEN			: "OPEN",
  TokCLOSE			: "CLOSE",
  TokPAD				: "PAD",
  TokEVENT			: "EVENT",
  TokPRICE			: "PRICE",
  TokLOCATION	: "LOCATION",
  TokBEGINTAG	: "BEGINTAG",
  TokENDTAG		: "ENDTAG",
}

type lexTest struct {
	name  string
	input string
	items []item
}

var (
	tEOL      = item{TokEOL, 0, "\n"}
	tEOF      = item{TokEOF, 0, ""}
	tINDENT      = item{TokINDENT, 0, "  "}
	// tCOMMENT      = item{TokCOMMENT, 0, "COMMENT"}
	// tFor      = item{TokIdentifier, 0, "for"}
	// tLeft     = item{TokLeftDelim, 0, "{{"}
	// tLpar     = item{TokLeftParen, 0, "("}
	// tPipe     = item{TokPIPE, 0, "|"}
	// tQuote    = item{TokSTRING, 0, `"abc \n\t\" "`}
	// tRange    = item{TokRange, 0, "range"}
	// tRight    = item{TokRightDelim, 0, "}}"}
	// tRpar     = item{TokRightParen, 0, ")"}
	// tSpace    = item{TokSpace, 0, " "}
	// raw       = "`" + `abc\n\t\" ` + "`"
	// tRawQuote = item{TokRawString, 0, raw}
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
		{TokCOMMENT, 0, " Bla-di-bla"},
		tEOF}},
	{"comment_nl", "; Bla-di-bla\n", []item{
		{TokCOMMENT, 0, " Bla-di-bla"},
		tEOL,
		tEOF}},
	{"indent_comment_nl", "  ; Bla-di-bla\n", []item{
		tINDENT,
		{TokCOMMENT, 0, " Bla-di-bla"},
		tEOL,
		tEOF}},

	{"date", "2013-04-13", []item{
		{TokDATE, 0, "2013-04-13"},
		tEOF}},
	{"date_space", "2013-04-13  \n", []item{
		{TokDATE, 0, "2013-04-13"},
		tEOL,
		tEOF}},

	{"date_flag", "2013-04-13 *", []item{
		{TokDATE, 0, "2013-04-13"},
		{TokTXNFLAG, 0, "*"},
		tEOF}},
	{"date_flag", "2013-04-13 %", []item{
		{TokDATE, 0, "2013-04-13"},
		{TokTXNFLAG, 0, "%"},
		tEOF}},

	{"flag", "*", []item{{TokTXNFLAG, 0, "*"}, tEOF}},
	{"flag", "!", []item{{TokTXNFLAG, 0, "!"}, tEOF}},
	{"flag", "&", []item{{TokTXNFLAG, 0, "&"}, tEOF}},
	{"flag", "#", []item{{TokTXNFLAG, 0, "#"}, tEOF}},
	{"flag", "%", []item{{TokTXNFLAG, 0, "%"}, tEOF}},

	{"pipe", "|", []item{{TokPIPE, 0, "|"}, tEOF}},
	{"at", "@", []item{{TokAT, 0, "@"}, tEOF}},
	{"atat", "@@", []item{{TokATAT, 0, "@@"}, tEOF}},
	{"lcurl", "{", []item{{TokLCURL, 0, "{"}, tEOF}},
	{"rcurl", "}", []item{{TokRCURL, 0, "}"}, tEOF}},
	{"equal", "=", []item{{TokEQUAL, 0, "="}, tEOF}},
	{"comma", ",", []item{{TokCOMMA, 0, ","}, tEOF}},

	{"string", "\"Bla-di-bla\"", []item{{TokSTRING, 0, "Bla-di-bla"}, tEOF}},
	{"string", "\"Bla\" \"di\" \"bla\"", []item{
		{TokSTRING, 0, "Bla"},
		{TokSTRING, 0, "di"},
		{TokSTRING, 0, "bla"},
		tEOF}},

	{"account", "Assets:US:HSBC", []item{{TokACCOUNT, 0, "Assets:US:HSBC"}, tEOF}},
	{"account", "Assets:CA:RBC-Investing", []item{{TokACCOUNT, 0, "Assets:CA:RBC-Investing"}, tEOF}},
	{"account", "Assets:CA:RBC_Investing", []item{{TokACCOUNT, 0, "Assets:CA:RBC_Investing"}, tEOF}},

	{"currency", "USD", []item{{TokCURRENCY, 0, "USD"}, tEOF}},
	{"currency", "CAD", []item{{TokCURRENCY, 0, "CAD"}, tEOF}},
	{"currency", "RBF1001", []item{{TokCURRENCY, 0, "RBF1001"}, tEOF}},
	{"currency", "AIS512", []item{{TokCURRENCY, 0, "AIS512"}, tEOF}},

	{"number", "12345", []item{{TokNUMBER, 0, "12345"}, tEOF}},
	{"number", "123.45", []item{{TokNUMBER, 0, "123.45"}, tEOF}},
	{"number", "876.6433", []item{{TokNUMBER, 0, "876.6433"}, tEOF}},
	{"number", "-876.6433", []item{{TokNUMBER, 0, "-876.6433"}, tEOF}},
	{"number", "+876.6433", []item{{TokNUMBER, 0, "+876.6433"}, tEOF}},
	{"number", "-876", []item{{TokNUMBER, 0, "-876"}, tEOF}},
	{"number", "+876", []item{{TokNUMBER, 0, "+876"}, tEOF}},

	{"directive", "open", []item{{TokOPEN, 0, "open"}, tEOF}},
	{"directive", "close", []item{{TokCLOSE, 0, "close"}, tEOF}},
}

// 'collect' gathers the emitted items into a slice.
func collect(t *lexTest) (items []item) {
	l := Lex(t.name, t.input)
	for {
		item := l.NextTok()
		items = append(items, item)
		if item.Type == TokEOF || item.Type == TokERROR {
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
