// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package beancount

import (
	"fmt"
	//"os"
	"strings"
	"unicode"
	"unicode/utf8"
	"regexp"
)

type Pos int

// Tok represents a token or text string returned from the scanner.
type item struct {
	Type TokenType // The type of this item.
	pos Pos      // The starting position, in bytes, of this item in the input string.
	val string   // The value of this item.
}

func (i item) String() string {
	switch {
	case i.Type == TokEOF:
		return "EOF"
	// case i.Type == TokEOL:
	// 	return "EOL"
	case i.Type == TokERROR:
		return i.val
	default:
		return fmt.Sprintf("<%v %#v>", i.Type, i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// TokenType identifies the type of lex items.
type TokenType int

const (
	TokERROR    TokenType = iota		// error occurred; value is text of error
  TokINDENT											// Initial indent IF at the beginning of a line
  TokEOL												// End-of-line
	TokEOF												// End-of-file
	TokCOMMENT  									// A comment
  TokPIPE												// |
  TokATAT												// @@
  TokAT													// @
  TokLCURL											// {
  TokRCURL											// }
  TokEQUAL											// =
  TokCOMMA											// ,

  TokTXN												// 'txn' keyword
  TokTXNFLAG										// Valid characters for flags
  TokCHECK											// 'check' keyword
  TokOPEN												// 'open' keyword
  TokCLOSE											// 'close' keyword
  TokPAD												// 'pad' keyword
  TokEVENT											// 'event' keyword
  TokPRICE											// 'price' keyword
  TokLOCATION										// 'location' keyword
  TokNOTE    										// 'note' keyword

  TokBEGINTAG										// 'begintag' keyword
  TokENDTAG											// 'endtag' keyword

	TokDATE												// A date object
	TokCURRENCY										// A currency specification
	TokACCOUNT  									// The name of an account
  TokSTRING											// A quoted string, with any characters inside
	TokNUMBER											// A floating-point number
)

// Make the types prettyprint.
var itemName = map[TokenType]string{
	TokERROR			: "ERROR",
	TokINDENT			: "INDENT",
	TokEOL				: "EOL",
	TokEOF				: "EOF",
	TokCOMMENT    : "COMMENT",
	TokPIPE				: "PIPE",
	TokATAT				: "ATAT",
	TokAT					: "AT",
	TokLCURL			: "LCURL",
	TokRCURL			: "RCURL",
	TokEQUAL			: "EQUAL",
	TokCOMMA			: "COMMA",
	TokTXN				: "TXN",
	TokTXNFLAG		: "TXNFLAG",
	TokCHECK			: "CHECK",
	TokOPEN				: "OPEN",
	TokCLOSE			: "CLOSE",
	TokPAD				: "PAD",
	TokEVENT			: "EVENT",
	TokPRICE			: "PRICE",
	TokLOCATION		: "LOCATION",
	TokNOTE   		: "NOTE",
	TokBEGINTAG		: "BEGINTAG",
	TokENDTAG			: "ENDTAG",
	TokDATE				: "DATE",
	TokCURRENCY		: "CURRENCY",
	TokACCOUNT		: "ACCOUNT",
	TokSTRING			: "STRING",
	TokNUMBER			: "NUMBER",
}

func (it TokenType) String() string {
	return itemName[it]
}



var keywords = map[string]TokenType{
  "txn"					: TokTXN,
  "check"				: TokCHECK,
  "open"				: TokOPEN,
  "close"				: TokCLOSE,
  "pad"					: TokPAD,
  "event"				: TokEVENT,
  "price"				: TokPRICE,
  "location"		: TokLOCATION, // FIXME: remove, make this just an event
  "note"  		  : TokNOTE,
  "begintag"		: TokBEGINTAG,
  "endtag"			: TokENDTAG,
}

const eof = -1

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// 'lexer' holds the state of the scanner.
type lexer struct {
	name       string    // The name of the input; used only for error reports
	input      string    // The string being scanned
	state      stateFn   // The next lexing function to enter
	pos        Pos       // Current position in the input
	start      Pos       // Start position of this item
	width      Pos       // Width of last rune read from input
	lastPos    Pos       // Position of most recent item returned by NextTok
	lineStart  Pos       // Position of the beginning the current line
	lineNo     int       // Current line number
	items      chan item // Channel of scanned items
	parenDepth int       // Nesting depth of ( ) exprs
}

// 'next' returns the next rune in the input.
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = Pos(w)
	l.pos += l.width
	return r
}

// 'peek' returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// 'length' returns the number of runes parsed.
func (l *lexer) length() Pos {
	return l.pos - l.start
}

// 'backup' steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
}

// 'emit' passes an item back to the client.
func (l *lexer) emit(t TokenType) {
	l.items <- item{t, l.start, l.input[l.start:l.pos]}
	l.start = l.pos
}

// 'ignore' skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

// 'accept' consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.IndexRune(valid, l.next()) >= 0 {
		return true
	}
	l.backup()
	return false
}

// 'acceptRun' consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.IndexRune(valid, l.next()) >= 0 {
	}
	l.backup()
}

// 'lineNumber' reports which line we're on, based on the position of
// the previous item returned by NextTok. Doing it this way
// means we don't have to worry about peek double counting.
func (l *lexer) lineNumber() int {
	return 1 + strings.Count(l.input[:l.lastPos], "\n")
}

// 'errorf' returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.NextTok.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{TokERROR, l.start, fmt.Sprintf(format, args...)}
	return nil
}

// 'NextTok' returns the next item from the input.
func (l *lexer) NextTok() item {
	item := <-l.items
	l.lastPos = item.pos
	return item
}

// 'lex' creates a new scanner for the input string.
func Lex(name, input string) *lexer {
	l := &lexer{
		name:       name,
		input:      input,
		items:      make(chan item),
	}
	go l.run()
	return l
}

// 'run' runs the state machine for the lexer.
func (l *lexer) run() {
	for l.state = lexTopLevel; l.state != nil; {
		l.state = l.state(l)
	}
}

var (
	dateRegexp, _ = regexp.Compile("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
)

// 'lexTopLevel' is the top-level scanner that looks for new things
func lexTopLevel(l *lexer) stateFn {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		l.emit(TokEOF)
		return nil
	}

	var ninput = l.input[l.pos:]
	switch r := l.peek(); {

	case r == eof:
		return l.errorf("Unexpected end of file")

	case isEndOfLine(r):
		l.next()
		l.emit(TokEOL)
		l.lineNo++
		l.lineStart = l.pos

	case isSpace(r):
		return lexSpace

	case r == ';':
		return lexComment

	case unicode.IsDigit(r):
		if len(ninput) >= 10 && dateRegexp.MatchString(ninput) {
			l.pos += 10
			l.emit(TokDATE)
		} else {
			return lexNumber
		}

	case r == '.':
			return lexNumber

	case l.accept("+-"):
		l.backup()
		return lexNumber

	case l.accept("|"):
		l.emit(TokPIPE)

	case strings.HasPrefix(ninput, "@@"):
		l.pos += 2
		l.emit(TokATAT)

	case l.accept("@"):
		l.emit(TokAT)

	case l.accept("{"):
		l.emit(TokLCURL)

	case l.accept("}"):
		l.emit(TokRCURL)

	case l.accept("="):
		l.emit(TokEQUAL)

	case l.accept(","):
		l.emit(TokCOMMA)

	case l.accept("\""):
		return lexString

	case l.accept("*!&#?%"):
		l.emit(TokTXNFLAG)

	case l.accept("ABCDEFGHIJKLMNOPQRSTUVWXYZ"):
		return lexFreeWord

	case l.accept("abcdefghijklmnopqrstuvwxyz"):
		return lexKeyword

	default:
		return l.errorf("Invalid input: %v", l.input[l.pos:l.pos+80])
	}

	return lexTopLevel
}

// 'lexFreeWord' looks for a free string, which could be an account, currency,
// or a Dr/Cr marker.
func lexFreeWord(l *lexer) stateFn {
	l.backup()

	var allUpper = true;
	var hasSpecial = false;
Loop:
	for {
		switch r := l.next(); {

		case unicode.IsLetter(r):
			if unicode.IsLower(r) {
				allUpper = false
			}

		case unicode.IsDigit(r):

		case r == ':' || r == '_' || r == '-':
			hasSpecial = true;

		case r == '\'':

		default:
			l.backup()
			break Loop;
		}
	}

	if hasSpecial {
		l.emit(TokACCOUNT)
	} else if allUpper && l.length() >= 2 {
		l.emit(TokCURRENCY)
	} else {
		return l.errorf("Invalid string")
	}
	return lexTopLevel
}

// 'lexKeyword' scans a keyword
func lexKeyword(l *lexer) stateFn {
	l.acceptRun("abcdefghijklmnopqrstuvwxyz")
	keyword := l.input[l.start:l.pos]
	if item, found := keywords[keyword]; found {
		l.emit(item)
	} else {
		return l.errorf("Invalid keyword: '%v'", keyword)
	}
	return lexTopLevel
}

// 'lexComment' scans a comment. The left comment marker is known to be present.
func lexComment(l *lexer) stateFn {
	for l.peek() == ';' {
		l.next()
	}
	l.ignore()

	for !isEndOfLine(l.peek()) && l.peek() != eof {
		//fmt.Printf("C %v\n", l.next())
		l.next()
	}

	l.emit(TokCOMMENT)
	return lexTopLevel
}

// 'lexString' parse a quoted string literal.
func lexString(l *lexer) stateFn {
	l.ignore()
Loop:
	for {
		switch l.next() {
		case '\\':
			if r := l.next(); r != eof && r != '\n' {
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("unterminated quoted string")
		case '"':
			break Loop
		}
	}
	l.pos -= 1
	l.emit(TokSTRING)
	l.pos += 1
	l.ignore()
	return lexTopLevel
}

// 'lexSpace' scans a run of space characters.
// One space has already been seen.
func lexSpace(l *lexer) stateFn {
	for isSpace(l.peek()) {
		l.next()
	}
	if l.start == l.lineStart {
		l.emit(TokINDENT)
	} else {
		l.ignore()
	}
	return lexTopLevel
}

// 'isSpace' reports whether r is a space character.
func isSpace(r rune) bool {
	return r == ' ' || r == '\t'
}

// 'isEndOfLine' reports whether r is an end-of-line character.
func isEndOfLine(r rune) bool {
	return r == '\r' || r == '\n'
}

// 'isAlphaNumeric' reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// 'lexNumber' scans a number: decimal, octal, hex, float, or imaginary. This
// isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// and "089" - but when it's wrong the input is invalid and the parser (via
// strconv) will notice.
func lexNumber(l *lexer) stateFn {
	// Optional leading sign.
	l.accept("+-")
	digits := "0123456789"
	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}
	l.emit(TokNUMBER)
	return lexTopLevel
}
