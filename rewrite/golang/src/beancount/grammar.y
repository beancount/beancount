// -*- mode: go -*-
// Parser grammar for beancount 2.0 input syntax.
//
// This assumes it feeds off the corresponding lexer in this pacakge. This is
// meant to be used with the stock "go yacc" command.

%{

package beancount

import (
	"fmt"
	"time"
	"unicode/utf8"
	"container/list"
)

const (
	ISO8601 = "2006-01-02"
)



type Parser struct {
	transactions *list.List
}

func MakeParser() *Parser {
	return &Parser{
		transactions: list.New(),
	}
}

type DatePair struct {
	date time.Time
	other_date time.Time
}



// FIXME: These will all go into another file
type Transaction struct {
	date time.Time
	other_date time.Time
	flag rune
	description string
	postings *list.List
}

type Posting struct {
	account *Account
}

type Account struct {
	name string
}




%}

%union {
	str string
	char rune
	date time.Time
	dates *DatePair
	transaction *Transaction
	posting *Posting
	posting_list *list.List
	account *Account
}

%token <str> ERROR			// error occurred; value is text of error
%token <str> INDENT			// Initial indent IF at the beginning of a line
%token <str> EOL				// End-of-line
%token <str> EOF				// End-of-file
%token <str> COMMENT		// A comment
%token <str> PIPE				// |
%token <str> ATAT				// @@
%token <str> AT					// @
%token <str> LCURL			// {
%token <str> RCURL			// }
%token <str> EQUAL			// =
%token <str> COMMA			// ,
%token <str> SLASH			// /
%token <str> TXN				// 'txn' keyword
%token <str> TXNFLAG		// Valid characters for flags
%token <str> CHECK			// 'check' keyword
%token <str> OPEN				// 'open' keyword
%token <str> CLOSE			// 'close' keyword
%token <str> PAD				// 'pad' keyword
%token <str> EVENT			// 'event' keyword
%token <str> PRICE			// 'price' keyword
%token <str> LOCATION		// 'location' keyword
%token <str> NOTE				// 'note' keyword
%token <str> BEGINTAG		// 'begintag' keyword
%token <str> ENDTAG			// 'endtag' keyword
%token <str> DATE				// A date object
%token <str> ACCOUNT		// The name of an account
%token <str> CURRENCY		// A currency specification
%token <str> STRING			// A quoted string, with any characters inside
%token <str> NUMBER			// A floating-point number

%type <char> txn
%type	<date> date
%type	<dates> date_pair
%type	<transaction> transaction
%type	<posting> posting
%type	<posting_list> posting_list
%type	<account> account
%type	<str> begintag
%type	<str> endtag

%start directives

%%
//--------------------------------------------------------------------------------

empty :

txn : TXN
    {
			$$ = rune(-1)
		}
    | TXNFLAG
    {
			r, _ := utf8.DecodeRuneInString($1)
			$$ = r
		}

date : DATE
     {
			 d, _ := time.Parse(ISO8601, $1)
			 $$ = d
		 }

date_pair : date
					{
						$$ = &DatePair{$1, time.Time{}}
					}
					| date EQUAL date
					{
						$$ = &DatePair{$1, $3}
					}

transaction : date_pair txn STRING EOL posting_list
            {
							$$ = &Transaction{$1.date, $1.other_date, $2, $3, $5}
						}
            | date_pair txn STRING PIPE STRING EOL posting_list
            {
							$$ = &Transaction{$1.date, $1.other_date, $2, $5, $7}
						}

optflag : empty
        | TXNFLAG

account : ACCOUNT
        {
					$$ = &Account{$1}
				}

posting : INDENT optflag account amount_lot EOL
        {
					$$ = &Posting{}
				}
        | INDENT optflag account amount_lot AT amount EOL
        {
					$$ = &Posting{}
				}
        | INDENT optflag account amount_lot ATAT amount EOL
        {
					$$ = &Posting{}
				}
        | INDENT optflag account EOL
        {
					$$ = &Posting{}
				}

posting_list : empty
             {
							 $$ = list.New()
						 }
             | posting_list posting
             {
							 $$.PushBack($2)
						 }

currency_list : empty
              | CURRENCY
              | currency_list COMMA CURRENCY

begintag : BEGINTAG STRING
         {
					 $$ = $2
				 }

endtag : ENDTAG STRING
       {
				 $$ = $2
			 }

open : date OPEN account currency_list
     | date OPEN account STRING currency_list

close : date CLOSE account

pad : date PAD account account

check : date CHECK account amount

amount : NUMBER CURRENCY

amount_lot : amount
           | amount lot

lot : LCURL amount RCURL
    | LCURL amount SLASH date RCURL
    {
		 //fmt.Printf("%20v / %v\n", $2.str, $4.str)
	 }


price : date PRICE CURRENCY amount

location : date LOCATION STRING

event : date EVENT STRING STRING

note : date NOTE STRING
/* { */
/* 	fmt.Printf("%#v\n", &$3) */
/* } */

entry : EOL
      | transaction
      | check
      | open
      | close
      | pad
      | event
      | note
      | price
      | location

directive : entry
				  | begintag
          {
						parserState.tags.PushFront($1)
					}
				  | endtag
          {
						// FIXME: We should assert that the tag is present in the list (or
						// at the top of it, if required to be balanced).
						parserState.tags.Remove(parserState.tags.Front())
					}

directives : empty
           | directives directive


//--------------------------------------------------------------------------------
%%

func (l Lexer) Lex(lval *yySymType) int {
	item := l.NextTok()
	if item.Type == EOF {
		return 0
	}
	lval.str = item.val
	return int(item.Type)
}

func (l Lexer) Error(e string) {
	fmt.Printf("%s:%d: %v\n", l.name, l.lineNo, e)
}



// Global state of the parser.
type ParserState struct {
	tags *list.List
}

var parserState *ParserState

func Parse(yylex yyLexer) int {
	parserState = &ParserState{list.New()}
	result := yyParse(yylex)
	parserState = nil
	return result
}
