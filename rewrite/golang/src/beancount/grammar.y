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

%token TokERROR
%token TokINDENT
%token TokEOL
%token TokEOF
%token TokCOMMENT
%token TokPIPE
%token TokATAT
%token TokAT
%token TokLCURL
%token TokRCURL
%token TokEQUAL
%token TokCOMMA
%token TokTXN
%token TokTXNFLAG
%token TokCHECK
%token TokOPEN
%token TokCLOSE
%token TokPAD
%token TokEVENT
%token TokPRICE
%token TokLOCATION
%token TokNOTE
%token TokBEGINTAG
%token TokENDTAG
%token TokDATE
%token TokCURRENCY
%token TokACCOUNT
%token TokSTRING
%token TokNUMBER

%%
entry_list : empty
           | entry_list entry
{
}

entry : TokEOL
      | comment
      | open

open : TokDATE TokOPEN TokACCOUNT currency_list
     | TokDATE TokOPEN TokACCOUNT TokSTRING currency_list
{

}

currency_list : empty
              | TokCURRENCY
              | currency_list TokCOMMA TokCURRENCY

comment : TokCOMMENT

empty :


%%

func (l Lexer) Lex(lval *yySymType) int {
	item := l.NextTok()
	lval.val = item.val
	return int(item.Type)
}

func (l Lexer) Error(e string) {
	fmt.Printf("Error :%d %#v\n", l.lineNo, e)
}



func Parse(yylex yyLexer) int {
	return yyParse(yylex)
}
