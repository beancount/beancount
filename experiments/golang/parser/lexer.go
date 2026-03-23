package parser

import (
	"regexp"

	"github.com/beancount/beancount/v3/core"
)

// TokenType represents a lexical token type.
type TokenType int

const (
	ILLEGAL TokenType = iota
	EOF
	EOL
	INDENT

	// Literals
	DATE
	ACCOUNT
	CURRENCY
	NUMBER
	STRING
	TAG
	LINK
	KEY
	BOOL
	NONE

	// Keywords
	TXN
	BALANCE
	OPEN
	CLOSE
	COMMODITY
	PAD
	EVENT
	PRICE
	NOTE
	DOCUMENT
	QUERY
	CUSTOM
	PUSHTAG
	POPTAG
	PUSHMETA
	POPMETA
	OPTION
	INCLUDE
	PLUGIN

	// Punctuation
	PIPE      // |
	ATAT      // @@
	AT        // @
	LCURLCURL // {{
	RCURLCURL // }}
	LCURL     // {
	RCURL     // }
	COMMA     // ,
	TILDE     // ~
	HASH      // #
	ASTERISK  // *
	SLASH     // /
	COLON     // :
	PLUS      // +
	MINUS     // -
	LPAREN    // (
	RPAREN    // )

	// Characters used for flags
	FLAG
)

func (t TokenType) String() string {
	switch t {
	case EOF: return "EOF"
	case EOL: return "EOL"
	case INDENT: return "INDENT"
	case DATE: return "DATE"
	case ACCOUNT: return "ACCOUNT"
	case CURRENCY: return "CURRENCY"
	case NUMBER: return "NUMBER"
	case STRING: return "STRING"
	case TAG: return "TAG"
	case LINK: return "LINK"
	case KEY: return "KEY"
	case BOOL: return "BOOL"
	case NONE: return "NONE"
	case TXN: return "TXN"
	case BALANCE: return "BALANCE"
	case OPEN: return "OPEN"
	case CLOSE: return "CLOSE"
	case COMMODITY: return "COMMODITY"
	case PAD: return "PAD"
	case EVENT: return "EVENT"
	case PRICE: return "PRICE"
	case NOTE: return "NOTE"
	case DOCUMENT: return "DOCUMENT"
	case QUERY: return "QUERY"
	case CUSTOM: return "CUSTOM"
	case PUSHTAG: return "PUSHTAG"
	case POPTAG: return "POPTAG"
	case PUSHMETA: return "PUSHMETA"
	case POPMETA: return "POPMETA"
	case OPTION: return "OPTION"
	case INCLUDE: return "INCLUDE"
	case PLUGIN: return "PLUGIN"
	case FLAG: return "FLAG"
	case PIPE: return "PIPE"
	case ATAT: return "ATAT"
	case AT: return "AT"
	case LCURL: return "LCURL"
	case RCURL: return "RCURL"
	case LCURLCURL: return "LCURLCURL"
	case RCURLCURL: return "RCURLCURL"
	case COMMA: return "COMMA"
	case TILDE: return "TILDE"
	case HASH: return "HASH"
	case ASTERISK: return "ASTERISK"
	case SLASH: return "SLASH"
	case COLON: return "COLON"
	case PLUS: return "PLUS"
	case MINUS: return "MINUS"
	case LPAREN: return "LPAREN"
	case RPAREN: return "RPAREN"
	default: return "UNKNOWN"
	}
}

// Token represents a lexical token.
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Col     int
}

// Lexer produces tokens from a string.
type Lexer struct {
	input     string
	pos       int
	line      int
	col       int
	atLineBeg bool
	peeked    []Token
	Filename  string
}

// NewLexer creates a new lexer.
func NewLexer(input string, filename string) *Lexer {
	return &Lexer{
		input:     input,
		pos:       0,
		line:      1,
		col:       1,
		atLineBeg: true,
		Filename:  filename,
	}
}

var (
	dateRe     = regexp.MustCompile(`^[0-9]{4}[-/][0-9]{2}[-/][0-9]{2}`)
	currencyRe = regexp.MustCompile(`^(?:[A-Z][A-Z0-9\'\.\_\-]*[A-Z0-9]?)|(?:/[A-Z0-9\'\.\_\-]*[A-Z](?:[A-Z0-9\'\.\_\-]*[A-Z0-9])?)`)
	numberRe   = regexp.MustCompile(`^[-+]?[0-9,]+(?:\.[0-9]*)?`)
	tagRe      = regexp.MustCompile(`^#[A-Za-z0-9\-_/.]+`)
	linkRe     = regexp.MustCompile(`^\^[A-Za-z0-9\-_/.]+`)
	keyRe      = regexp.MustCompile(`^[a-z][a-zA-Z0-9\-_]+:`)
)

func isKeyword(s string) (TokenType, bool) {
	keywords := map[string]TokenType{
		"TRUE":       BOOL,
		"FALSE":      BOOL,
		"NULL":       NONE,
		"txn":        TXN,
		"balance":    BALANCE,
		"open":       OPEN,
		"close":      CLOSE,
		"commodity":  COMMODITY,
		"pad":        PAD,
		"event":      EVENT,
		"price":      PRICE,
		"note":       NOTE,
		"document":   DOCUMENT,
		"query":      QUERY,
		"custom":     CUSTOM,
		"pushtag":    PUSHTAG,
		"poptag":     POPTAG,
		"pushmeta":   PUSHMETA,
		"popmeta":    POPMETA,
		"option":     OPTION,
		"include":    INCLUDE,
		"plugin":     PLUGIN,
	}
	tt, ok := keywords[s]
	return tt, ok
}

// NextToken returns the next token.
func (l *Lexer) NextToken() Token {
	if len(l.peeked) > 0 {
		t := l.peeked[0]
		l.peeked = l.peeked[1:]
		return t
	}

	for l.pos < len(l.input) {
		ch := l.input[l.pos]

		// Handle spaces and INDENT
		if ch == ' ' || ch == '\t' {
			if l.atLineBeg {
				startPos := l.pos
				startCol := l.col
				for l.pos < len(l.input) && (l.input[l.pos] == ' ' || l.input[l.pos] == '\t') {
					l.pos++
					l.col++
				}
				l.atLineBeg = false
				return Token{Type: INDENT, Literal: l.input[startPos:l.pos], Line: l.line, Col: startCol}
			}
			l.pos++
			l.col++
			continue
		}

		// Comments
		if ch == ';' {
			for l.pos < len(l.input) && l.input[l.pos] != '\n' && l.input[l.pos] != '\r' {
				l.pos++
				l.col++
			}
			continue
		}

		startCol := l.col
		startLine := l.line

		if ch == '\n' || ch == '\r' {
			literal := string(ch)
			if ch == '\r' && l.pos+1 < len(l.input) && l.input[l.pos+1] == '\n' {
				literal = "\r\n"
				l.pos++
			}
			l.pos++
			l.line++
			l.col = 1
			l.atLineBeg = true
			return Token{Type: EOL, Literal: literal, Line: startLine, Col: startCol}
		}

		l.atLineBeg = false

		// Strings
		if ch == '"' {
			start := l.pos
			l.pos++
			l.col++
			for l.pos < len(l.input) && l.input[l.pos] != '"' {
				if l.input[l.pos] == '\n' {
					l.line++
					l.col = 1
				} else {
					l.col++
				}
				l.pos++
			}
			if l.pos < len(l.input) {
				l.pos++
				l.col++
			}
			return Token{Type: STRING, Literal: l.input[start+1 : l.pos-1], Line: startLine, Col: startCol}
		}

		// Two-character punctuation
		if ch == '{' && l.pos+1 < len(l.input) && l.input[l.pos+1] == '{' {
			l.pos += 2
			l.col += 2
			return Token{Type: LCURLCURL, Literal: "{{", Line: startLine, Col: startCol}
		}
		if ch == '}' && l.pos+1 < len(l.input) && l.input[l.pos+1] == '}' {
			l.pos += 2
			l.col += 2
			return Token{Type: RCURLCURL, Literal: "}}", Line: startLine, Col: startCol}
		}
		if ch == '@' && l.pos+1 < len(l.input) && l.input[l.pos+1] == '@' {
			l.pos += 2
			l.col += 2
			return Token{Type: ATAT, Literal: "@@", Line: startLine, Col: startCol}
		}

		// Single character punctuation or FLAG
		isPunct := true
		var pType TokenType
		switch ch {
		case '|': pType = PIPE
		case ',': pType = COMMA
		case '~': pType = TILDE
		case '/': pType = SLASH
		case '+': pType = PLUS
		case '-': 
			if l.pos+1 < len(l.input) && l.input[l.pos+1] >= '0' && l.input[l.pos+1] <= '9' {
				isPunct = false
			} else {
				pType = MINUS
			}
		case '(': pType = LPAREN
		case ')': pType = RPAREN
		case '{': pType = LCURL
		case '}': pType = RCURL
		case '@': pType = AT
		case '#': 
			if l.pos+1 < len(l.input) && l.input[l.pos+1] != ' ' && l.input[l.pos+1] != '\t' && l.input[l.pos+1] != '\n' && l.input[l.pos+1] != '\r' {
				isPunct = false
			} else {
				pType = HASH
			}
		case ':': pType = COLON
		default: isPunct = false
		}

		// Special Beancount flags: ! & ? % and also * 
		if !isPunct {
			if ch == '!' || ch == '&' || ch == '?' || ch == '%' || ch == '*' {
				isPunct = true
				pType = FLAG
			} else if ch >= 'A' && ch <= 'Z' {
				if l.pos+1 >= len(l.input) || l.input[l.pos+1] == ' ' || l.input[l.pos+1] == '\t' || l.input[l.pos+1] == '\n' || l.input[l.pos+1] == '\r' {
					isPunct = true
					pType = FLAG
				}
			}
		}

		if isPunct {
			l.pos++
			l.col++
			return Token{Type: pType, Literal: string(ch), Line: startLine, Col: startCol}
		}

		// Read Word
		startWord := l.pos
		for l.pos < len(l.input) {
			c := l.input[l.pos]
			if c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ';' || c == '{' || c == '}' || c == '(' || c == ')' || c == '@' || c == ',' {
				break
			}
			l.pos++
			l.col++
		}
		word := l.input[startWord:l.pos]

		if dateRe.MatchString(word) {
			return Token{Type: DATE, Literal: word, Line: startLine, Col: startCol}
		}
		if keyRe.MatchString(word) {
			return Token{Type: KEY, Literal: word[:len(word)-1], Line: startLine, Col: startCol}
		}
		if tt, ok := isKeyword(word); ok {
			return Token{Type: tt, Literal: word, Line: startLine, Col: startCol}
		}
		if core.IsValid(word) {
			return Token{Type: ACCOUNT, Literal: word, Line: startLine, Col: startCol}
		}
		if currencyRe.MatchString(word) {
			return Token{Type: CURRENCY, Literal: word, Line: startLine, Col: startCol}
		}
		if tagRe.MatchString(word) {
			return Token{Type: TAG, Literal: word[1:], Line: startLine, Col: startCol}
		}
		if linkRe.MatchString(word) {
			return Token{Type: LINK, Literal: word[1:], Line: startLine, Col: startCol}
		}
		if numberRe.MatchString(word) {
			return Token{Type: NUMBER, Literal: word, Line: startLine, Col: startCol}
		}

		return Token{Type: ILLEGAL, Literal: word, Line: startLine, Col: startCol}
	}

	return Token{Type: EOF, Literal: "", Line: l.line, Col: l.col}
}

// PeekToken looks ahead by n tokens
func (l *Lexer) PeekToken(n int) Token {
	for len(l.peeked) < n {
		l.peeked = append(l.peeked, l.NextToken())
	}
	return l.peeked[n-1]
}
