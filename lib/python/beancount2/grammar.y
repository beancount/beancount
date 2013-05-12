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
    char* string;
    PyObject* pyobj;
    /* transaction *Transaction */
    /* posting *Posting */
    /* posting_list *list.List */
    /* account *Account */
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
%type <pyobj> transaction
%type <pyobj> posting
%type <pyobj> posting_list
%type <pyobj> currency_list
%type <pyobj> open
%type <string> begintag
%type <string> endtag
%type <pyobj> amount


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
                $$ = PyObject_CallMethod(builder, "buildTransaction", "ObOO", $1, $2, Py_None, $3);
            }
            | DATE txn STRING PIPE STRING eol posting_list
            {
                $$ = PyObject_CallMethod(builder, "buildTransaction", "ObOO", $1, $2, $3, $5);
            }

optflag : empty
        | TXNFLAG

posting : INDENT optflag ACCOUNT amount_lot eol
        {
            /* $$ = PyObject_CallMethod(builder, "buildPosting", "ObOO", $2, $3, $4); */
        }
        | INDENT optflag ACCOUNT amount_lot AT amount eol
        {
        }
        | INDENT optflag ACCOUNT amount_lot ATAT amount eol
        {
        }
        | INDENT optflag ACCOUNT eol
        {
        }

posting_list : empty
             {
               /* $$ = list.New() */
             }
             | posting_list posting
             {
               /* $$.PushBack($2) */
             }

currency_list : empty
              {
                  $$ = Py_None; /* FIXME: Do I need to incref here? */
              }
              | CURRENCY
              {
                  $$ = PyObject_CallMethod(builder, "buildList", "OO", Py_None, $1);
              }
              | currency_list COMMA CURRENCY
              {
                  $$ = PyObject_CallMethod(builder, "buildList", "OO", $1, $3);
              }

begintag : BEGINTAG STRING
         {
           /* $$ = $2 */
         }

endtag : ENDTAG STRING
       {
         /* $$ = $2 */
       }

open : DATE OPEN ACCOUNT currency_list
     {
         $$ = PyObject_CallMethod(builder, "buildOpen", "OOOO", $1, Py_None, $3, $4);
     }
     | DATE OPEN ACCOUNT STRING currency_list
     {
         $$ = PyObject_CallMethod(builder, "buildOpen", "OOOO", $1, $3, $4, $5);
     }

close : DATE CLOSE ACCOUNT

pad : DATE PAD ACCOUNT ACCOUNT

check : DATE CHECK ACCOUNT amount

amount : NUMBER CURRENCY
       {
         $$ = PyObject_CallMethod(builder, "buildAmount", "OO", $1, $2);
       }

amount_lot : amount
           | amount lot

lot : LCURL amount RCURL
    | LCURL amount SLASH DATE RCURL
    {
     /* fmt.Printf("%20v / %v\n", $2.str, $4.str) */
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
