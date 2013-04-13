// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package beancount

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
	"regexp"
)

type Pos int

// Item represents a token or text string returned from the scanner.
type item struct {
	typ itemType // The type of this item.
	pos Pos      // The starting position, in bytes, of this item in the input string.
	val string   // The value of this item.
}

func (i item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	// case i.typ == itemEOL:
	// 	return "EOL"
	case i.typ == itemError:
		return i.val
	default:
		return fmt.Sprintf("<%v %#v>", i.typ, i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError    itemType = iota	// error occurred; value is text of error
  itemIndent										// Initial indent IF at the beginning of a line
  itemEOL												// End-of-line
	itemEOF                       // End-of-file
	itemComment										// A comment
  itemPipe											// |
  itemAtAt											// @@
  itemAt												// @
  itemLCurl											// {
  itemRCurl											// }
  itemEqual											// =
  itemComma											// ,
  itemTXNFLAG										// [*\!&R#c\?SP]
  itemTXN												// The 'txn' keyword
  itemOFX												// The 'ofx' keyword
  itemACCID											// The 'accid' keyword
  itemDEFACCOUNT								// @defaccount
  itemVAR												// @var
  itemPAD												// @pad
  itemCHECK											// @check
  itemBEGINTAG									// @begintag
  itemENDTAG										// @endtag
  itemPRICE											// @price
  itemLOCATION									// @location
	itemDate											// A date object
	itemCurrency									// A currency specification
	itemDrCr											// "Dr" or "Cr"
  itemString										// A quoted string, with any characters inside
	itemNumber										// A floating-point number
)

var key = map[string]itemType{
  "|"						: itemPipe,
  "@@"					: itemAtAt,
  "@"						: itemAt,
  "{"						: itemLCurl,
  "}"						: itemRCurl,
  "="						: itemEqual,
  ","						: itemComma,
  "txn"					: itemTXN,
  "ofx"					: itemOFX,
  "accid"				: itemACCID,
  "@defaccount" : itemDEFACCOUNT,
  "@var"				: itemVAR,
  "@pad"				: itemPAD,
  "@check"			: itemCHECK,
  "@begintag"		: itemBEGINTAG,
  "@endtag"			: itemENDTAG,
  "@price"			: itemPRICE,
  "@location"		: itemLOCATION,
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
	lastPos    Pos       // Position of most recent item returned by nextItem
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

// 'backup' steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
}

// 'emit' passes an item back to the client.
func (l *lexer) emit(t itemType) {
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
// the previous item returned by nextItem. Doing it this way
// means we don't have to worry about peek double counting.
func (l *lexer) lineNumber() int {
	return 1 + strings.Count(l.input[:l.lastPos], "\n")
}

// 'errorf' returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{itemError, l.start, fmt.Sprintf(format, args...)}
	return nil
}

// 'nextItem' returns the next item from the input.
func (l *lexer) nextItem() item {
	item := <-l.items
	l.lastPos = item.pos
	return item
}

// 'lex' creates a new scanner for the input string.
func lex(name, input, left, right string) *lexer {
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
	dateRegexp, _ = regexp.Compile("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
)

// 'lexTopLevel' is the top-level scanner
func lexTopLevel(l *lexer) stateFn {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		l.emit(itemEOF)
		return nil
	}

	switch r := l.peek(); {

	case r == eof:
		return l.errorf("Unexpected end of file")

	case isEndOfLine(r):
		l.next()
		l.emit(itemEOL)
		l.lineNo++
		l.lineStart = l.pos

	case isSpace(r):
		return lexSpace

	case r == ';':
		return lexComment

	case unicode.IsDigit(r):
		if dateRegexp.MatchString(l.input[l.pos:]) {
			l.pos += 10
			l.emit(itemDate)
		}
		// FIXME: parse a number here

	case l.accept("*!&R#c?SP"):
		l.emit(itemTXNFLAG)

	case l.accept("|"):
		l.emit(itemPipe)



// FIXME: continue here
	// case strings.HasPrefix(l.input[l.pos:], "@@"):
	// 	l.emit(itemAtAt)

  // "@@"					: itemAtAt,
  // "@"						: itemAt,
  // "{"						: itemLCurl,
  // "}"						: itemRCurl,
  // "="						: itemEqual,
  // ","						: itemComma,



	}
	return lexTopLevel

	// for {
	// 	if strings.HasPrefix(l.input[l.pos:], l.leftDelim) {
	// 		if l.pos > l.start {
	// 			l.emit(itemText)
	// 		}
	// 		return lexLeftDelim
	// 	}
	// 	if l.next() == eof {
	// 		break
	// 	}
	// }
	// // Correctly reached EOF.
	// if l.pos > l.start {
	// 	l.emit(itemText)
	// }
	// l.emit(itemEOF)
	// return nil
}

// // lexLeftDelim scans the left delimiter, which is known to be present.
// func lexLeftDelim(l *lexer) stateFn {
// 	l.pos += Pos(len(l.leftDelim))
// 	if strings.HasPrefix(l.input[l.pos:], leftComment) {
// 		return lexComment
// 	}
// 	l.emit(itemLeftDelim)
// 	l.parenDepth = 0
// 	return lexInsideAction
// }

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

	l.emit(itemComment)
	return lexTopLevel
}

// // lexRightDelim scans the right delimiter, which is known to be present.
// func lexRightDelim(l *lexer) stateFn {
// 	l.pos += Pos(len(l.rightDelim))
// 	l.emit(itemRightDelim)
// 	return lexText
// }
//
// // lexInsideAction scans the elements inside action delimiters.
// func lexInsideAction(l *lexer) stateFn {
// 	// Either number, quoted string, or identifier.
// 	// Spaces separate arguments; runs of spaces turn into itemSpace.
// 	// Pipe symbols separate and are emitted.
// 	if strings.HasPrefix(l.input[l.pos:], l.rightDelim) {
// 		if l.parenDepth == 0 {
// 			return lexRightDelim
// 		}
// 		return l.errorf("unclosed left paren")
// 	}
// 	switch r := l.next(); {
// 	case r == eof || isEndOfLine(r):
// 		return l.errorf("unclosed action")
// 	case isSpace(r):
// 		return lexSpace
// 	case r == ':':
// 		if l.next() != '=' {
// 			return l.errorf("expected :=")
// 		}
// 		l.emit(itemColonEquals)
// 	case r == '|':
// 		l.emit(itemPipe)
// 	case r == '"':
// 		return lexQuote
// 	case r == '`':
// 		return lexRawQuote
// 	case r == '$':
// 		return lexVariable
// 	case r == '\'':
// 		return lexChar
// 	case r == '.':
// 		// special look-ahead for ".field" so we don't break l.backup().
// 		if l.pos < Pos(len(l.input)) {
// 			r := l.input[l.pos]
// 			if r < '0' || '9' < r {
// 				return lexField
// 			}
// 		}
// 		fallthrough // '.' can start a number.
// 	case r == '+' || r == '-' || ('0' <= r && r <= '9'):
// 		l.backup()
// 		return lexNumber
// 	case isAlphaNumeric(r):
// 		l.backup()
// 		return lexIdentifier
// 	case r == '(':
// 		l.emit(itemLeftParen)
// 		l.parenDepth++
// 		return lexInsideAction
// 	case r == ')':
// 		l.emit(itemRightParen)
// 		l.parenDepth--
// 		if l.parenDepth < 0 {
// 			return l.errorf("unexpected right paren %#U", r)
// 		}
// 		return lexInsideAction
// 	case r <= unicode.MaxASCII && unicode.IsPrint(r):
// 		l.emit(itemChar)
// 		return lexInsideAction
// 	default:
// 		return l.errorf("unrecognized character in action: %#U", r)
// 	}
// 	return lexInsideAction
// }

// 'lexSpace' scans a run of space characters.
// One space has already been seen.
func lexSpace(l *lexer) stateFn {
	for isSpace(l.peek()) {
		l.next()
	}
	if l.start == l.lineStart {
		l.emit(itemIndent)
	} else {
		l.ignore()
	}
	return lexTopLevel
}





// // lexIdentifier scans an alphanumeric.
// func lexIdentifier(l *lexer) stateFn {
// Loop:
// 	for {
// 		switch r := l.next(); {
// 		case isAlphaNumeric(r):
// 			// absorb.
// 		default:
// 			l.backup()
// 			word := l.input[l.start:l.pos]
// 			if !l.atTerminator() {
// 				return l.errorf("bad character %#U", r)
// 			}
// 			switch {
// 			case key[word] > itemKeyword:
// 				l.emit(key[word])
// 			case word[0] == '.':
// 				l.emit(itemField)
// 			case word == "true", word == "false":
// 				l.emit(itemBool)
// 			default:
// 				l.emit(itemIdentifier)
// 			}
// 			break Loop
// 		}
// 	}
// 	return lexInsideAction
// }
//
// // lexField scans a field: .Alphanumeric.
// // The . has been scanned.
// func lexField(l *lexer) stateFn {
// 	return lexFieldOrVariable(l, itemField)
// }
//
// // lexVariable scans a Variable: $Alphanumeric.
// // The $ has been scanned.
// func lexVariable(l *lexer) stateFn {
// 	if l.atTerminator() { // Nothing interesting follows -> "$".
// 		l.emit(itemVariable)
// 		return lexInsideAction
// 	}
// 	return lexFieldOrVariable(l, itemVariable)
// }
//
// // lexVariable scans a field or variable: [.$]Alphanumeric.
// // The . or $ has been scanned.
// func lexFieldOrVariable(l *lexer, typ itemType) stateFn {
// 	if l.atTerminator() { // Nothing interesting follows -> "." or "$".
// 		if typ == itemVariable {
// 			l.emit(itemVariable)
// 		} else {
// 			l.emit(itemDot)
// 		}
// 		return lexInsideAction
// 	}
// 	var r rune
// 	for {
// 		r = l.next()
// 		if !isAlphaNumeric(r) {
// 			l.backup()
// 			break
// 		}
// 	}
// 	if !l.atTerminator() {
// 		return l.errorf("bad character %#U", r)
// 	}
// 	l.emit(typ)
// 	return lexInsideAction
// }
//
// // atTerminator reports whether the input is at valid termination character to
// // appear after an identifier. Breaks .X.Y into two pieces. Also catches cases
// // like "$x+2" not being acceptable without a space, in case we decide one
// // day to implement arithmetic.
// func (l *lexer) atTerminator() bool {
// 	r := l.peek()
// 	if isSpace(r) || isEndOfLine(r) {
// 		return true
// 	}
// 	switch r {
// 	case eof, '.', ',', '|', ':', ')', '(':
// 		return true
// 	}
// 	// Does r start the delimiter? This can be ambiguous (with delim=="//", $x/2 will
// 	// succeed but should fail) but only in extremely rare cases caused by willfully
// 	// bad choice of delimiter.
// 	if rd, _ := utf8.DecodeRuneInString(l.rightDelim); rd == r {
// 		return true
// 	}
// 	return false
// }
//
// // lexChar scans a character constant. The initial quote is already
// // scanned. Syntax checking is done by the parser.
// func lexChar(l *lexer) stateFn {
// Loop:
// 	for {
// 		switch l.next() {
// 		case '\\':
// 			if r := l.next(); r != eof && r != '\n' {
// 				break
// 			}
// 			fallthrough
// 		case eof, '\n':
// 			return l.errorf("unterminated character constant")
// 		case '\'':
// 			break Loop
// 		}
// 	}
// 	l.emit(itemCharConstant)
// 	return lexInsideAction
// }
//
// // lexNumber scans a number: decimal, octal, hex, float, or imaginary. This
// // isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// // and "089" - but when it's wrong the input is invalid and the parser (via
// // strconv) will notice.
// func lexNumber(l *lexer) stateFn {
// 	if !l.scanNumber() {
// 		return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
// 	}
// 	if sign := l.peek(); sign == '+' || sign == '-' {
// 		// Complex: 1+2i. No spaces, must end in 'i'.
// 		if !l.scanNumber() || l.input[l.pos-1] != 'i' {
// 			return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
// 		}
// 		l.emit(itemComplex)
// 	} else {
// 		l.emit(itemNumber)
// 	}
// 	return lexInsideAction
// }
//
// func (l *lexer) scanNumber() bool {
// 	// Optional leading sign.
// 	l.accept("+-")
// 	// Is it hex?
// 	digits := "0123456789"
// 	if l.accept("0") && l.accept("xX") {
// 		digits = "0123456789abcdefABCDEF"
// 	}
// 	l.acceptRun(digits)
// 	if l.accept(".") {
// 		l.acceptRun(digits)
// 	}
// 	if l.accept("eE") {
// 		l.accept("+-")
// 		l.acceptRun("0123456789")
// 	}
// 	// Is it imaginary?
// 	l.accept("i")
// 	// Next thing mustn't be alphanumeric.
// 	if isAlphaNumeric(l.peek()) {
// 		l.next()
// 		return false
// 	}
// 	return true
// }
//
// // lexQuote scans a quoted string.
// func lexQuote(l *lexer) stateFn {
// Loop:
// 	for {
// 		switch l.next() {
// 		case '\\':
// 			if r := l.next(); r != eof && r != '\n' {
// 				break
// 			}
// 			fallthrough
// 		case eof, '\n':
// 			return l.errorf("unterminated quoted string")
// 		case '"':
// 			break Loop
// 		}
// 	}
// 	l.emit(itemString)
// 	return lexInsideAction
// }
//
// // lexRawQuote scans a raw quoted string.
// func lexRawQuote(l *lexer) stateFn {
// Loop:
// 	for {
// 		switch l.next() {
// 		case eof, '\n':
// 			return l.errorf("unterminated raw quoted string")
// 		case '`':
// 			break Loop
// 		}
// 	}
// 	l.emit(itemRawString)
// 	return lexInsideAction
// }

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
