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

/* FIXME: set real filename here. */

/* Error-handling function. */
void yyerror(char const* message)
{
    /* fprintf(stderr, "LOCTEST %d %d: ", yylloc.first_line, s); */
    fprintf(stderr, "%s: %s\n", yy_filename, message);
}

void report_error(char const* message, YYLTYPE* yylloc)
{
    fprintf(stderr, "%s:%d:: %s\n", yy_filename, yylloc->first_line, message);
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


/* #define DECREF1(x)  Py_DECREF(x); */
#define DECREF1(x1)
#define DECREF2(x1, x2)
#define DECREF3(x1, x2, x3)
#define DECREF4(x1, x2, x3, x4)
#define DECREF5(x1, x2, x3, x4, x5)


#define FILE_LINE_ARGS  yy_filename, yylloc.first_line


%}


/*--------------------------------------------------------------------------------*/
/* Bison Declarations */


/* Options. */
%defines
%error-verbose
%debug
%pure_parser
%locations

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
%token <character> FLAG    /* Valid characters for flags */
%token TXN                 /* 'txn' keyword */
%token CHECK               /* 'check' keyword */
%token OPEN                /* 'open' keyword */
%token CLOSE               /* 'close' keyword */
%token PAD                 /* 'pad' keyword */
%token EVENT               /* 'event' keyword */
%token PRICE               /* 'price' keyword */
%token NOTE                /* 'note' keyword */
%token PUSHTAG            /* 'pushtag' keyword */
%token POPTAG              /* 'poptag' keyword */
%token <pyobj> DATE        /* A date object */
%token <pyobj> ACCOUNT     /* The name of an account */
%token <pyobj> CURRENCY    /* A currency specification */
%token <pyobj> STRING      /* A quoted string, with any characters inside */
%token <pyobj> NUMBER      /* A floating-point number */
%token <pyobj> TAG         /* A tag that can be associated with a transaction */

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
%type <pyobj> position
%type <pyobj> lot_cost_date
%type <pyobj> price
%type <pyobj> event
%type <pyobj> note
%type <pyobj> entry
%type <pyobj> declarations
%type <pyobj> tags_list


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
    | FLAG
    {
        $$ = $1;
    }

eol : EOL
    | INDENT EOL
    | COMMENT EOL
    | INDENT
    | COMMENT

tags_list : empty
          {
              Py_INCREF(Py_None);
              $$ = Py_None;
          }
          | tags_list TAG
          {
              $$ = BUILD("handle_list", "OO", $1, $2);
              DECREF2($1, $2);
          }

transaction : DATE txn STRING tags_list eol posting_list
            {
                $$ = BUILD("transaction", "siObOOOO", FILE_LINE_ARGS, $1, $2, Py_None, $3, $4, $6);
                DECREF4($1, $3, $4, $6);
            }
            | DATE txn STRING PIPE STRING tags_list eol posting_list
            {
                $$ = BUILD("transaction", "siObOOOO", FILE_LINE_ARGS, $1, $2, $3, $5, $6, $8);
                DECREF5($1, $3, $5, $6, $8);
            }

optflag : empty
        {
            $$ = '\0';
        }
        | FLAG

posting : INDENT optflag ACCOUNT position eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, Py_None, Py_False, $2);
            DECREF2($3, $4);
        }
        | INDENT optflag ACCOUNT position AT amount eol
        {
            $$ = BUILD("posting", "OOOOb", $3, $4, $6, Py_False, $2);
            DECREF3($3, $4, $6);
        }
        | INDENT optflag ACCOUNT position ATAT amount eol
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

pushtag : PUSHTAG TAG
         {
             BUILD("pushtag", "O", $2);
             DECREF1($2);
         }

poptag : POPTAG TAG
       {
           BUILD("poptag", "O", $2);
           DECREF1($2);
       }

open : DATE OPEN ACCOUNT currency_list
     {
         $$ = BUILD("open", "siOOOO", FILE_LINE_ARGS, $1, $3, Py_None, $4);
         DECREF3($1, $3, $4);
     }
     | DATE OPEN ACCOUNT STRING currency_list
     {
         $$ = BUILD("open", "siOOOO", FILE_LINE_ARGS, $1, $3, $4, $5);
         DECREF4($1, $3, $4, $5);
     }

close : DATE CLOSE ACCOUNT
      {
          $$ = BUILD("close", "siOO", FILE_LINE_ARGS, $1, $3);
          DECREF2($1, $3);
      }

pad : DATE PAD ACCOUNT ACCOUNT
    {
        $$ = BUILD("pad", "siOOO", FILE_LINE_ARGS, $1, $3, $4);
        DECREF3($1, $3, $4);
    }

check : DATE CHECK ACCOUNT position
      {
          $$ = BUILD("check", "siOOO", FILE_LINE_ARGS, $1, $3, $4);
          DECREF3($1, $3, $4);
      }

amount : NUMBER CURRENCY
       {
         PyObject* o = BUILD("amount", "OO", $1, $2);
         $$ = o;
         DECREF2($1, $2);
       }

position : amount
           {
               $$ = BUILD("position", "OO", $1, Py_None);
               DECREF1($1);
           }
           | amount lot_cost_date
           {
               $$ = BUILD("position", "OO", $1, $2);
               DECREF2($1, $2);
           }

lot_cost_date : LCURL amount RCURL
         {
             $$ = BUILD("lot_cost_date", "OO", $2, Py_None);
             DECREF1($2);
         }
         | LCURL amount SLASH DATE RCURL
         {
             $$ = BUILD("lot_cost_date", "OO", $2, $4);
             DECREF2($2, $4);
         }


price : DATE PRICE CURRENCY amount
      {
          $$ = BUILD("price", "siOOO", FILE_LINE_ARGS, $1, $3, $4);
          DECREF3($1, $3, $4);
      }

event : DATE EVENT STRING STRING
      {
          $$ = BUILD("event", "siOOO", FILE_LINE_ARGS, $1, $3, $4);
          DECREF3($1, $3, $4);
      }

note : DATE NOTE ACCOUNT STRING
      {
          $$ = BUILD("note", "siOOO", FILE_LINE_ARGS, $1, $3, $4);
          DECREF3($1, $3, $4);
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
          | pushtag
          | poptag

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
        case FLAG     : return "FLAG";
        case TXN      : return "TXN";
        case CHECK    : return "CHECK";
        case OPEN     : return "OPEN";
        case CLOSE    : return "CLOSE";
        case PAD      : return "PAD";
        case EVENT    : return "EVENT";
        case PRICE    : return "PRICE";
        case NOTE     : return "NOTE";
        case PUSHTAG : return "PUSHTAG";
        case POPTAG   : return "POPTAG";
        case DATE     : return "DATE";
        case ACCOUNT  : return "ACCOUNT";
        case CURRENCY : return "CURRENCY";
        case STRING   : return "STRING";
        case NUMBER   : return "NUMBER";
        case TAG      : return "TAG";
    }
    return 0;
}
