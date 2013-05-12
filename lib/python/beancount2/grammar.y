/* -*- mode: c -*- */
/*
 * Parser grammar for beancount 2.0 input syntax.
 *
 * This assumes it feeds off the corresponding lexer in this pacakge. This is
 * meant to be used with the stock "go yacc" command.
 */

/*--------------------------------------------------------------------------------*/
/* Prologue */
%{

#include <stdio.h>
#include "parser.h"
#include "lexer.h"


/* Error-handling function. */
void yyerror(char const *s)
{
    fprintf(stderr, "%s:%d:%d: Parsing error - %s\n", "/home/blais/q/office/accounting/tmp/syntax.beancount", yy_line_begin, yy_column(), s);
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


/* #define DECREF1(x)  Py_DECREF(x); */
#define DECREF1(x1)
#define DECREF2(x1, x2)
#define DECREF3(x1, x2, x3)
#define DECREF4(x1, x2, x3, x4)

%}


/*--------------------------------------------------------------------------------*/
/* Bison Declarations */


/* Options. */
%defines
%error-verbose
%debug
%pure_parser

/* FIXME: Pass this explicitly eventually. */
/* %parse-param { PyObject* builder} */


/* Collection of value types. */
%union {
    char character;
    const char* string;
    PyObject* pyobj;
}

/* Types for terminal symbols */
%token <string> ERROR      /* error occurred; value is text of error */
%token <string> INDENT     /* Initial indent IF at the beginning of a line */
%token <string> EOL        /* End-of-line */
%token <string> COMMENT    /* A comment */
%token <string> SKIPPED    /* A line skipped because not a directive nor a comment */
%token <string> PIPE       /* | */
%token <string> ATAT       /* @@ */
%token <string> AT         /* @ */
%token <string> LCURL      /* { */
%token <string> RCURL      /* } */
%token <string> EQUAL      /* = */
%token <string> COMMA      /* , */
%token <string> SLASH      /* / */
%token <character> TXNFLAG /* Valid characters for flags */
%token TXN                 /* 'txn' keyword */
%token CHECK               /* 'check' keyword */
%token OPEN                /* 'open' keyword */
%token CLOSE               /* 'close' keyword */
%token PAD                 /* 'pad' keyword */
%token EVENT               /* 'event' keyword */
%token PRICE               /* 'price' keyword */
%token NOTE                /* 'note' keyword */
%token BEGINTAG            /* 'begintag' keyword */
%token ENDTAG              /* 'endtag' keyword */
%token <pyobj> DATE        /* A date object */
%token <pyobj> ACCOUNT     /* The name of an account */
%token <pyobj> CURRENCY    /* A currency specification */
%token <pyobj> STRING      /* A quoted string, with any characters inside */
%token <pyobj> NUMBER      /* A floating-point number */
%token <pyobj> TAG         /* A tag that can be associated with a transaction */

%code{

/* A function that will convert a token name to a string, used in debugging. */
const char* getTokenName(int token)
{
    switch ( token ) {
        case ERROR    : return "ERROR";
        case INDENT   : return "INDENT";
        case EOL      : return "EOL";
        case COMMENT  : return "COMMENT";
        case SKIPPED  : return "SKIPPED";
        case PIPE     : return "PIPE";
        case ATAT     : return "ATAT";
        case AT       : return "AT";
        case LCURL    : return "LCURL";
        case RCURL    : return "RCURL";
        case EQUAL    : return "EQUAL";
        case COMMA    : return "COMMA";
        case SLASH    : return "SLASH";
        case TXNFLAG  : return "TXNFLAG";
        case TXN      : return "TXN";
        case CHECK    : return "CHECK";
        case OPEN     : return "OPEN";
        case CLOSE    : return "CLOSE";
        case PAD      : return "PAD";
        case EVENT    : return "EVENT";
        case PRICE    : return "PRICE";
        case NOTE     : return "NOTE";
        case BEGINTAG : return "BEGINTAG";
        case ENDTAG   : return "ENDTAG";
        case DATE     : return "DATE";
        case ACCOUNT  : return "ACCOUNT";
        case CURRENCY : return "CURRENCY";
        case STRING   : return "STRING";
        case NUMBER   : return "NUMBER";
        case TAG      : return "TAG";
    }
    return 0;
}

}


/* Types for non-terminal symbols. */
%type <character> txn
%type <character> optflag
%type <pyobj> transaction
%type <pyobj> posting
%type <pyobj> posting_list
%type <pyobj> currency_list
%type <pyobj> open
%type <pyobj> close
%type <pyobj> check
%type <pyobj> pad
%type <pyobj> amount
%type <pyobj> amount_lot
%type <pyobj> lot
%type <pyobj> price
%type <pyobj> event
%type <pyobj> note
%type <pyobj> entry
%type <pyobj> declarations
%type <pyobj> tag


/* Start symbol. */
%start file


/*--------------------------------------------------------------------------------*/
/* Grammar Rules */
%%

empty :

/* A transaction declaration can be either 'txn' or one of the special character flags. */
txn : TXN
    {
        $$ = '*';
    }
    | TXNFLAG
    {
        $$ = $1;
    }

eol : EOL
    | COMMENT EOL

transaction : DATE txn STRING eol posting_list
            {
                $$ = BUILD("transaction", "ObOOO", $1, $2, Py_None, $3, $5);
                DECREF3($1, $3, $5);
            }
            | DATE txn STRING PIPE STRING eol posting_list
            {
                $$ = BUILD("transaction", "ObOOO", $1, $2, $3, $5, $7);
                DECREF4($1, $3, $5, $7);
            }

optflag : empty
        {
            $$ = '\0';
        }
        | TXNFLAG

posting : INDENT optflag ACCOUNT amount_lot eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, Py_None, Py_False, $2);
            DECREF2($3, $4);
        }
        | INDENT optflag ACCOUNT amount_lot AT amount eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, $6, Py_False, $2);
            DECREF3($3, $4, $6);
        }
        | INDENT optflag ACCOUNT amount_lot ATAT amount eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, $6, Py_True, $2);
            DECREF3($3, $4, $6);
        }
        | INDENT optflag ACCOUNT eol
        {
            $$ = BUILD("posting", "OOOOb", $3, Py_None, Py_None, Py_False, $2);
            DECREF1($3);
        }

posting_list : empty
             {
                 Py_INCREF(Py_None);
                 $$ = Py_None;
             }
             | posting_list posting
             {
                 $$ = BUILD("handle_list", "OO", $1, $2);
                 DECREF2($1, $2);
             }

currency_list : empty
              {
                  Py_INCREF(Py_None);
                  $$ = Py_None;
              }
              | CURRENCY
              {
                  $$ = BUILD("handle_list", "OO", Py_None, $1);
                  DECREF1($1);
              }
              | currency_list COMMA CURRENCY
              {
                  $$ = BUILD("handle_list", "OO", $1, $3);
                  DECREF2($1, $3);
              }

tag : STRING

begintag : BEGINTAG tag
         {
             BUILD("begintag", "O", $2);
             DECREF1($2);
         }

endtag : ENDTAG tag
       {
           BUILD("endtag", "O", $2);
           DECREF1($2);
       }

open : DATE OPEN ACCOUNT currency_list
     {
         $$ = BUILD("open", "OOOO", $1, $3, Py_None, $4);
         DECREF3($1, $3, $4);
     }
     | DATE OPEN ACCOUNT STRING currency_list
     {
         $$ = BUILD("open", "OOOO", $1, $3, $4, $5);
         DECREF4($1, $3, $4, $5);
     }

close : DATE CLOSE ACCOUNT
      {
          $$ = BUILD("close", "OO", $1, $3);
          DECREF2($1, $3);
      }

pad : DATE PAD ACCOUNT ACCOUNT
    {
        $$ = BUILD("pad", "OOO", $1, $3, $4);
        DECREF3($1, $3, $4);
    }

check : DATE CHECK ACCOUNT amount
      {
          $$ = BUILD("check", "OOO", $1, $3, $4);
          DECREF3($1, $3, $4);
      }

amount : NUMBER CURRENCY
       {
         PyObject* o = BUILD("amount", "OO", $1, $2);
         $$ = o;
         DECREF2($1, $2);
       }

amount_lot : amount
           {
               $$ = BUILD("amount_lot", "OO", $1, Py_None);
               DECREF1($1);
           }
           | amount lot
           {
               $$ = BUILD("amount_lot", "OO", $1, $2);
               DECREF2($1, $2);
           }


lot : LCURL amount RCURL
    {
        $$ = BUILD("lot", "OO", $2, Py_None);
        DECREF1($2);
    }
    | LCURL amount SLASH DATE RCURL
    {
        $$ = BUILD("lot", "OO", $2, $4);
        DECREF2($2, $4);
    }


price : DATE PRICE CURRENCY amount
      {
          $$ = BUILD("price", "OOO", $1, $3, $4);
          DECREF3($1, $3, $4);
      }

event : DATE EVENT STRING STRING
      {
          $$ = BUILD("event", "OOO", $1, $3, $4);
          DECREF3($1, $3, $4);
      }

note : DATE NOTE STRING
      {
          $$ = BUILD("note", "OO", $1, $3);
          DECREF2($1, $3);
      }

entry : transaction
      | check
      | open
      | close
      | pad
      | event
      | note
      | price
      {
          $$ = $1;
      }

directive : SKIPPED
          | eol
          | begintag
          | endtag

declarations : declarations entry
             {
                 $$ = BUILD("handle_list", "OO", $1, $2);
                 DECREF2($1, $2);
             }
             | declarations directive
             {
                 $$ = $1;
             }
             | empty
             {
                  Py_INCREF(Py_None);
                  $$ = Py_None;
             }

file : declarations
     {
         BUILD("store_result", "O", $1);
     }


/*--------------------------------------------------------------------------------*/
/* Epilogue */
%%
