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
    fprintf(stderr, "Parsing error: %s\n", s);
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


%}


/*--------------------------------------------------------------------------------*/
/* Bison Declarations */


/* Options. */
%defines
%error-verbose
%debug
%pure_parser


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
%token LOCATION            /* 'location' keyword */
%token NOTE                /* 'note' keyword */
%token BEGINTAG            /* 'begintag' keyword */
%token ENDTAG              /* 'endtag' keyword */
%token <pyobj> DATE       /* A date object */
%token <pyobj> ACCOUNT    /* The name of an account */
%token <pyobj> CURRENCY   /* A currency specification */
%token <pyobj> STRING     /* A quoted string, with any characters inside */
%token <pyobj> NUMBER     /* A floating-point number */

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
        case LOCATION : return "LOCATION";
        case NOTE     : return "NOTE";
        case BEGINTAG : return "BEGINTAG";
        case ENDTAG   : return "ENDTAG";
        case DATE     : return "DATE";
        case ACCOUNT  : return "ACCOUNT";
        case CURRENCY : return "CURRENCY";
        case STRING   : return "STRING";
        case NUMBER   : return "NUMBER";
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
%type <pyobj> pad
%type <pyobj> amount
%type <pyobj> amount_lot


/* Start symbol. */
%start directives


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
            }
            | DATE txn STRING PIPE STRING eol posting_list
            {
                $$ = BUILD("transaction", "ObOOO", $1, $2, $3, $5, $7);
            }

optflag : empty
        {
            $$ = '\0';
        }
        | TXNFLAG

posting : INDENT optflag ACCOUNT amount_lot eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, Py_None, Py_False, $2);
        }
        | INDENT optflag ACCOUNT amount_lot AT amount eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, $6, Py_False, $2);
        }
        | INDENT optflag ACCOUNT amount_lot ATAT amount eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, $6, Py_True, $2);
        }
        | INDENT optflag ACCOUNT eol
        {
            $$ = BUILD("posting", "OOOOb", $3, Py_None, Py_None, Py_False, $2);
        }

posting_list : empty
             {
                 $$ = Py_None; /* FIXME: Do I need to incref here? */
             }
             | posting_list posting
             {
                 $$ = BUILD("handle_list", "OO", $1, $2);
             }

currency_list : empty
              {
                  $$ = Py_None; /* FIXME: Do I need to incref here? */
              }
              | CURRENCY
              {
                  $$ = BUILD("handle_list", "OO", Py_None, $1);
              }
              | currency_list COMMA CURRENCY
              {
                  $$ = BUILD("handle_list", "OO", $1, $3);
              }

begintag : BEGINTAG STRING
         {
             BUILD("begintag", "O", $2);
         }

endtag : ENDTAG STRING
       {
           BUILD("endtag", "O", $2);
       }

open : DATE OPEN ACCOUNT currency_list
     {
         $$ = BUILD("open", "OOOO", $1, Py_None, $3, $4);
     }
     | DATE OPEN ACCOUNT STRING currency_list
     {
         $$ = BUILD("open", "OOOO", $1, $3, $4, $5);
     }

close : DATE CLOSE ACCOUNT
      {
          $$ = BUILD("close", "OO", $1, $3);
      }

pad : DATE PAD ACCOUNT ACCOUNT
    {
        /* $$ = BUILD("pad", "OOO", $1, $3, $4); */
    }

check : DATE CHECK ACCOUNT amount
      {
          $$ = BUILD("check", "OOO", $1, $3, $4);
      }

amount : NUMBER CURRENCY
       {
         $$ = BUILD("amount", "OO", $1, $2);
       }

amount_lot : amount
           | amount lot
           {
               /* FIXME: here */
           }


lot : LCURL amount RCURL
    | LCURL amount SLASH DATE RCURL
    {
        /* FIXME: here */
   }


price : DATE PRICE CURRENCY amount

location : DATE LOCATION STRING

event : DATE EVENT STRING STRING

note : DATE NOTE STRING
/* { */
/*  fmt.Printf("%#v\n", &$3) */
/* } */

entry : transaction
      | check
      | open
      | close
      | pad
      | event
      | note
      | price
      | location

directive : entry
          | SKIPPED
          | eol
          | begintag
          {
            /* parserState.tags.PushFront($1) */
          }
          | endtag
          {
            // FIXME: We should assert that the tag is present in the list (or
            // at the top of it, if required to be balanced).
            /* parserState.tags.Remove(parserState.tags.Front()) */
          }

directives : empty
           | directives directive


/*--------------------------------------------------------------------------------*/
/* Epilogue */
%%
