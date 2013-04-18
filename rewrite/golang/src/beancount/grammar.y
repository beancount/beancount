%{

package beancount

import (
	// "bufio"
	// "flag"
	"fmt"
	// "math"
	// "runtime"
	// "os"
	// "path/filepath"
	// "strconv"
	// "unicode/utf8"
)

// const (
// 	Ndim = 15  // number of dimensions
// 	Maxe = 695 // log of largest number
// )

// type Node struct {
// 	vval float64
// 	dim  [Ndim]int8
// }

// type Var struct {
// 	name string
// 	node Node
// }

// var fi *bufio.Reader // input
// var fund [Ndim]*Var  // names of fundamental units
// var line string      // current input line
// var lineno int       // current input line number
// var linep int        // index to next rune in unput
// var nerrors int      // error count
// var one Node         // constant one
// var peekrune rune    // backup runt from input
// var retnode1 Node
// var retnode2 Node
// var retnode Node
// var sym string
// var vflag bool

%}

%union {
	val string
}

// %type	<node>	prog expr expr0 expr1 expr2 expr3 expr4

// %token	<vval>	VÄL // dieresis to test UTF-8
// %token	<numb>	_SUP // tests leading underscore in token name

// FIXME: I really ought to put the types in here...
%token ERROR	// error occurred; value is text of error
%token INDENT	// Initial indent IF at the beginning of a line
%token EOL	// End-of-line
%token EOF	// End-of-file
%token COMMENT  // A comment
%token PIPE	// |
%token ATAT	// @@
%token AT	// @
%token LCURL	// {
%token RCURL	// }
%token EQUAL	// =
%token COMMA	// ,
%token TXN	// 'txn' keyword
%token TXNFLAG	// Valid characters for flags
%token CHECK	// 'check' keyword
%token OPEN	// 'open' keyword
%token CLOSE	// 'close' keyword
%token PAD	// 'pad' keyword
%token EVENT	// 'event' keyword
%token PRICE	// 'price' keyword
%token LOCATION	// 'location' keyword
%token NOTE    	// 'note' keyword
%token BEGINTAG	// 'begintag' keyword
%token ENDTAG	// 'endtag' keyword
%token DATE	// A date object
%token CURRENCY	// A currency specification
%token ACCOUNT  // The name of an account
%token STRING	// A quoted string, with any characters inside
%token NUMBER	// A floating-point number

%start entry_list

%%

empty :

txn : TXN
    | TXNFLAG

dates : DATE
      | DATE EQUAL DATE

transaction : dates txn STRING EOL posting_list
            | dates txn STRING PIPE STRING EOL posting_list

optflag : empty
        | TXNFLAG

posting : INDENT optflag ACCOUNT lot EOL
        | INDENT optflag ACCOUNT lot AT amount EOL
        | INDENT optflag ACCOUNT lot ATAT amount EOL
        | INDENT optflag ACCOUNT EOL
        | INDENT EOL

posting_list : empty
             | posting_list posting

currency_list : empty
              | CURRENCY
              | currency_list COMMA CURRENCY

begintag : BEGINTAG STRING

endtag : ENDTAG STRING

open : DATE OPEN ACCOUNT currency_list
     | DATE OPEN ACCOUNT STRING currency_list

close : DATE CLOSE ACCOUNT

pad : DATE PAD ACCOUNT ACCOUNT

check : DATE CHECK ACCOUNT amount

amount : NUMBER CURRENCY

lot : amount
    | amount LCURL amount RCURL

price : DATE PRICE CURRENCY amount

location : DATE LOCATION STRING

event : DATE EVENT STRING STRING

note : DATE NOTE STRING
{
	fmt.Printf("%#v\n", &$3)
}

entry : EOL
      | transaction
      | check
      | begintag
      | endtag
      | open
      | close
      | pad
      | event
      | note
      | price
      | location

entry_list : empty
           | entry_list entry


%%

func (l Lexer) Lex(lval *yySymType) int {
	item := l.NextTok()
	if item.Type == EOF {
		return 0
	}
	lval.val = item.val
	return int(item.Type)
}

func (l Lexer) Error(e string) {
	fmt.Printf("%s:%d: %v\n", l.name, l.lineNo, e)
}

func Parse(yylex yyLexer) int {
	return yyParse(yylex)
}
