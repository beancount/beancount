package beancount

import (
	"fmt"
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
	case i.Type == EOF:
		return "EOF"
	// case i.Type == EOL:
	// 	return "EOL"
	case i.Type == ERROR:
		return i.val
	default:
		return fmt.Sprintf("<%v %#v>", i.Type, i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// TokenType identifies the type of lex items.
type TokenType int

// Make the types prettyprint.
// FIXME: I think there's a way to auto-generate this map from gen code in grammar.go
var itemName = map[TokenType]string{
	ERROR					: "ERROR",
	INDENT				: "INDENT",
	EOL						: "EOL",
	EOF						: "EOF",
	COMMENT				: "COMMENT",
	PIPE					: "PIPE",
	ATAT					: "ATAT",
	AT						: "AT",
	LCURL					: "LCURL",
	RCURL					: "RCURL",
	EQUAL					: "EQUAL",
	COMMA					: "COMMA",
	TXN						: "TXN",
	TXNFLAG				: "TXNFLAG",
	CHECK					: "CHECK",
	OPEN					: "OPEN",
	CLOSE					: "CLOSE",
	PAD						: "PAD",
	EVENT					: "EVENT",
	PRICE					: "PRICE",
	LOCATION			: "LOCATION",
	NOTE					: "NOTE",
	BEGINTAG			: "BEGINTAG",
	ENDTAG				: "ENDTAG",
	DATE					: "DATE",
	CURRENCY			: "CURRENCY",
	ACCOUNT				: "ACCOUNT",
	STRING				: "STRING",
	NUMBER				: "NUMBER",
}

func (it TokenType) String() string {
	return itemName[it]
}



var keywords = map[string]TokenType{
  "txn"					: TXN,
  "check"				: CHECK,
  "open"				: OPEN,
  "close"				: CLOSE,
  "pad"					: PAD,
  "event"				: EVENT,
  "price"				: PRICE,
  "location"		: LOCATION, // FIXME: remove, make this just an event
  "note"  		  : NOTE,
  "begintag"		: BEGINTAG,
  "endtag"			: ENDTAG,
}

const eof = -1

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*Lexer) stateFn

// 'Lexer' holds the state of the scanner.
type Lexer struct {
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
func (l *Lexer) next() rune {
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
func (l *Lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// 'length' returns the number of runes parsed.
func (l *Lexer) length() Pos {
	return l.pos - l.start
}

// 'backup' steps back one rune. Can only be called once per call of next.
func (l *Lexer) backup() {
	l.pos -= l.width
}

// 'emit' passes an item back to the client.
func (l *Lexer) emit(t TokenType) {
	l.items <- item{t, l.start, l.input[l.start:l.pos]}
	l.start = l.pos
}

// 'ignore' skips over the pending input before this point.
func (l *Lexer) ignore() {
	l.start = l.pos
}

// 'accept' consumes the next rune if it's from the valid set.
func (l *Lexer) accept(valid string) bool {
	if strings.IndexRune(valid, l.next()) >= 0 {
		return true
	}
	l.backup()
	return false
}

// 'acceptRun' consumes a run of runes from the valid set.
func (l *Lexer) acceptRun(valid string) {
	for strings.IndexRune(valid, l.next()) >= 0 {
	}
	l.backup()
}

// 'lineNumber' reports which line we're on, based on the position of
// the previous item returned by NextTok. Doing it this way
// means we don't have to worry about peek double counting.
func (l *Lexer) lineNumber() int {
	return 1 + strings.Count(l.input[:l.lastPos], "\n")
}

// 'errorf' returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.NextTok.
func (l *Lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{ERROR, l.start, fmt.Sprintf(format, args...)}
	return nil
}

// 'NextTok' returns the next item from the input.
func (l *Lexer) NextTok() item {
	item := <-l.items
	l.lastPos = item.pos
	return item
}

// 'lex' creates a new scanner for the input string.
func MakeLexer(name, input string) *Lexer {
	l := &Lexer{
		name:       name,
		input:      input,
		lineNo:     1,
		items:      make(chan item),
	}
	go l.run()
	return l
}

// 'run' runs the state machine for the Lexer.
func (l *Lexer) run() {
	for l.state = lexTopLevel; l.state != nil; {
		l.state = l.state(l)
	}
}

var (
	dateRegexp, _ = regexp.Compile("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
)

// 'lexTopLevel' is the top-level scanner that looks for new things
func lexTopLevel(l *Lexer) stateFn {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		l.emit(EOF)
		return nil
	}

	var ninput = l.input[l.pos:]
	switch r := l.peek(); {

	case r == eof:
		return l.errorf("Unexpected end of file")

	case isEndOfLine(r):
		l.next()
		l.emit(EOL)
		l.lineNo++
		l.lineStart = l.pos

	case isSpace(r):
		return lexSpace

	case r == ';':
		return lexComment

	case unicode.IsDigit(r):
		if len(ninput) >= 10 && dateRegexp.MatchString(ninput) {
			l.pos += 10
			l.emit(DATE)
		} else {
			return lexNumber
		}

	case r == '.':
			return lexNumber

	case l.accept("+-"):
		l.backup()
		return lexNumber

	case l.accept("|"):
		l.emit(PIPE)

	case strings.HasPrefix(ninput, "@@"):
		l.pos += 2
		l.emit(ATAT)

	case l.accept("@"):
		l.emit(AT)

	case l.accept("{"):
		l.emit(LCURL)

	case l.accept("}"):
		l.emit(RCURL)

	case l.accept("="):
		l.emit(EQUAL)

	case l.accept(","):
		l.emit(COMMA)

	case l.accept("\""):
		return lexStringMulti

	case r == '*':
		if l.start == l.lineStart {
			return lexSkipLine
		} else {
			l.pos++
			l.emit(TXNFLAG)
		}

	case l.accept("!&#?%"):
		l.emit(TXNFLAG)

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
func lexFreeWord(l *Lexer) stateFn {
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
		case r == '.':

		default:
			l.backup()
			break Loop;
		}
	}

	if hasSpecial {
		l.emit(ACCOUNT)
	} else if allUpper && l.length() >= 2 {
		l.emit(CURRENCY)
	} else {
		return l.errorf("Invalid string")
	}
	return lexTopLevel
}

// 'lexKeyword' scans a keyword
func lexKeyword(l *Lexer) stateFn {
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
func lexComment(l *Lexer) stateFn {
	for l.peek() == ';' {
		l.next()
	}
	l.ignore()

	for !isEndOfLine(l.peek()) && l.peek() != eof {
		//fmt.Printf("C %v\n", l.next())
		l.next()
	}

	emitComment(l)
	return lexTopLevel
}

// 'emitComment' is where we decide to either emit or ignore COMMENT tokens.
func emitComment(l *Lexer) {
	///l.emit(COMMENT)
	l.ignore()
}

// 'lexSkipLine' skips until the end of the line and emits it as a comment. This
// is used to ignore org-mode lines.
func lexSkipLine(l *Lexer) stateFn {
	for !isEndOfLine(l.peek()) && l.peek() != eof {
		l.next()
	}

	emitComment(l)
	return lexTopLevel
}

// 'lexStringSingle' parses a quoted string literal on a single line.
func lexStringSingle(l *Lexer) stateFn {
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
	l.emit(STRING)
	l.pos += 1
	l.ignore()
	return lexTopLevel
}

// 'lexStringMulti' parses a quoted string literal on possibly multiple lines.
func lexStringMulti(l *Lexer) stateFn {
	l.ignore()
Loop:
	for {
		switch l.next() {
		case eof:
			return l.errorf("unterminated quoted string")
		case '"':
			break Loop
		}
	}
	l.pos -= 1
	l.emit(STRING)
	l.pos += 1
	l.ignore()
	return lexTopLevel
}

// 'lexSpace' scans a run of space characters.
// One space has already been seen.
func lexSpace(l *Lexer) stateFn {
	for isSpace(l.peek()) {
		l.next()
	}
	if l.start == l.lineStart {
		l.emit(INDENT)
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
func lexNumber(l *Lexer) stateFn {
	// Optional leading sign.
	l.accept("+-")
	digits := "0123456789"
	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}
	l.emit(NUMBER)
	return lexTopLevel
}
