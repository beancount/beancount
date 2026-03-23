package parser

import (
	"testing"
)

func TestLexerBasic(t *testing.T) {
	input := `2014-01-01 * "Payee" "Narration" #tag ^link
  Assets:Bank:Checking  -100.00 USD
  Expenses:Food          100.00 USD`

	lexer := NewLexer(input, "test.beancount")

	expected := []struct {
		typ TokenType
		lit string
	}{
		{DATE, "2014-01-01"},
		{FLAG, "*"},
		{STRING, "Payee"},
		{STRING, "Narration"},
		{TAG, "tag"},
		{LINK, "link"},
		{EOL, "\n"},
		{INDENT, "  "},
		{ACCOUNT, "Assets:Bank:Checking"},
		{NUMBER, "-100.00"},
		{CURRENCY, "USD"},
		{EOL, "\n"},
		{INDENT, "  "},
		{ACCOUNT, "Expenses:Food"},
		{NUMBER, "100.00"},
		{CURRENCY, "USD"},
		{EOF, ""},
	}

	for i, exp := range expected {
		tok := lexer.NextToken()
		if tok.Type != exp.typ || tok.Literal != exp.lit {
			t.Errorf("Token %d: expected %v %q, got %v %q", i, exp.typ, exp.lit, tok.Type, tok.Literal)
		}
	}
}

func TestLexerKeywords(t *testing.T) {
	input := `txn balance open close commodity pad event price note document query custom pushtag poptag pushmeta popmeta option include plugin`
	lexer := NewLexer(input, "test.beancount")

	expectedTypes := []TokenType{
		TXN, BALANCE, OPEN, CLOSE, COMMODITY, PAD, EVENT, PRICE, NOTE, DOCUMENT, QUERY, CUSTOM, PUSHTAG, POPTAG, PUSHMETA, POPMETA, OPTION, INCLUDE, PLUGIN,
	}

	for i, expType := range expectedTypes {
		tok := lexer.NextToken()
		if tok.Type != expType {
			t.Errorf("Token %d: expected %v, got %v", i, expType, tok.Type)
		}
	}
}

func TestLexerComments(t *testing.T) {
	input := `2014-01-01 open Assets:Bank ; A comment
; Another comment
  Assets:Bank`
	
	lexer := NewLexer(input, "test.beancount")
	expected := []struct {
		typ TokenType
		lit string
	}{
		{DATE, "2014-01-01"},
		{OPEN, "open"},
		{ACCOUNT, "Assets:Bank"},
		{EOL, "\n"},
		{EOL, "\n"}, // Next line has comment and ends
		{INDENT, "  "},
		{ACCOUNT, "Assets:Bank"},
		{EOF, ""},
	}

	for i, exp := range expected {
		tok := lexer.NextToken()
		if tok.Type != exp.typ || tok.Literal != exp.lit {
			t.Errorf("Token %d: expected %v %q, got %v %q", i, exp.typ, exp.lit, tok.Type, tok.Literal)
		}
	}
}

func TestLexerUnicodeAccount(t *testing.T) {
	input := `Other:Bank Óthяr:Bあnk
ΑβγⅠ:ΑβγⅠ ابجا:ابجا`
	lexer := NewLexer(input, "test.beancount")

	expected := []struct {
		typ TokenType
		lit string
	}{
		{ACCOUNT, "Other:Bank"},
		{ACCOUNT, "Óthяr:Bあnk"},
		{EOL, "\n"},
		{ACCOUNT, "ΑβγⅠ:ΑβγⅠ"},
		{ACCOUNT, "ابجا:ابجا"},
		{EOF, ""},
	}

	for i, exp := range expected {
		tok := lexer.NextToken()
		if tok.Type != exp.typ || tok.Literal != exp.lit {
			t.Errorf("Token %d: expected %v %q, got %v %q", i, exp.typ, exp.lit, tok.Type, tok.Literal)
		}
	}
}
