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
#include "builder.h"
#include "beancount_lexer.h"


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
%token <string> DATE       /* A date object */
%token <string> ACCOUNT    /* The name of an account */
%token <string> CURRENCY   /* A currency specification */
%token <string> STRING     /* A quoted string, with any characters inside */
%token <string> NUMBER     /* A floating-point number */

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
%type <pyobj> date
%type <transaction> transaction
%type <posting> posting
%type <posting_list> posting_list
%type <account> account
%type <string> begintag
%type <string> endtag


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

date : DATE
     {
         /* free($1); */
         /* $$ = buildDate($1); */
         /* d, _ := time.Parse(ISO8601, $1) */
         /* $$ = d */
     }

eol : EOL
    | COMMENT EOL

transaction : date txn STRING eol posting_list
            {
              /* $$ = &Transaction{$1.date, $1.other_date, $2, $3, $5} */
            }
            | date txn STRING PIPE STRING eol posting_list
            {
              /* $$ = &Transaction{$1.date, $1.other_date, $2, $5, $7} */
            }

optflag : empty
        | TXNFLAG

account : ACCOUNT
        {
          /* $$ = &Account{$1} */
        }

posting : INDENT optflag account amount_lot eol
        {
          /* $$ = &Posting{} */
        }
        | INDENT optflag account amount_lot AT amount eol
        {
          /* $$ = &Posting{} */
        }
        | INDENT optflag account amount_lot ATAT amount eol
        {
          /* $$ = &Posting{} */
        }
        | INDENT optflag account eol
        {
          /* $$ = &Posting{} */
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
              | CURRENCY
              | currency_list COMMA CURRENCY

begintag : BEGINTAG STRING
         {
           /* $$ = $2 */
         }

endtag : ENDTAG STRING
       {
         /* $$ = $2 */
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
     /* fmt.Printf("%20v / %v\n", $2.str, $4.str) */
   }


price : date PRICE CURRENCY amount

location : date LOCATION STRING

event : date EVENT STRING STRING

note : date NOTE STRING
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
