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
#include <assert.h>
#include "parser.h"
#include "lexer.h"


/* FIXME: remove this */
PyObject* checkNull(PyObject* o)
{
    if ( o == NULL ) {
        PyErr_Print();
        /* FIXME: We should remove this and handle all errors. */
        abort();
    }
    return o;
}



#define BUILD(method_name, format, ...)                                                 \
    checkNull( PyObject_CallMethod(builder, method_name, format, __VA_ARGS__) );

#define BUILD_NOARGS(method_name)                                       \
    checkNull( PyObject_CallMethod(builder, method_name, NULL) );


#define BUILD_X(method_name, format, ...)                                                        \
    PyObject* build_value = PyObject_CallMethod(builder, method_name, format, __VA_ARGS__);     \
    if (build_value == NULL) {                                                                  \
        YYABORT;                                                                                \
    }

#define BUILD_NOARGS_X(method_name)                                             \
    PyObject* build_value = PyObject_CallMethod(builder, method_name, NULL);    \
    if (build_value == NULL) {                                                  \
        YYABORT;                                                                \
    }

/* FIXME: if there is an error, we end up leaking the $1, $2, ... find a
   solution for this. Maybe a more sensible solution here would be to inc-ref
   None when setting that and to have the cleanup/decref code be done all within
   the macro, to simplify all the code below. */

/* FIXME: Complete testing aborting from here instead of calling abort() as above. */
/* #define BUILD(method_name, format, ...)                                                         \ */
/*     {                                                                                           \ */
/*         checkNull( PyObject_CallMethod(builder, method_name, format, __VA_ARGS__) );            \ */
/*         PyObject* result = PyObject_CallMethod(builder, method_name, format, __VA_ARGS__)       \ */
/*         if (result == NULL) {                                                                   \ */
/*             YYABORT;                                                                            \ */
/*         }, result                                                                               \ */
/*     } */






/* First line of reported file/line string. This is used as #line. */
int yy_firstline;

#define FILE_LINE_ARGS  yy_filename, ((yyloc).first_line + yy_firstline)

/* Error-handling function. {ca6aab8b9748} */
void yyerror(char const* message)
{
    /* Skip lex errors: they have already been registered the lexer itself. */
    if (strstr(message, "LEX_ERROR") != NULL) {
        return;
    }
    else {
        /* Register a syntax error with the builder. */
        BUILD("build_grammar_error", "ssi", message, yy_filename, yylineno + yy_firstline);
        TRACE_ERROR("yyerror: '%s'\nyytext='%s'\n", message, yytext);
    }
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


/* #define DECREF1(x)  Py_DECREF(x); */
#define DECREF1(x1)
#define DECREF2(x1, x2)
#define DECREF3(x1, x2, x3)
#define DECREF4(x1, x2, x3, x4)
#define DECREF5(x1, x2, x3, x4, x5)
#define DECREF6(x1, x2, x3, x4, x5, x6)

%}


/*--------------------------------------------------------------------------------*/
/* Bison Declarations */


/* Options. */
%defines
%error-verbose
%debug
%pure-parser
%locations
/* %glr-parser */


/* Collection of value types. */
%union {
    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;
}

/* Types for terminal symbols */
%token <string> LEX_ERROR  /* Error occurred in the lexer; value is text of error */
%token <string> INDENT     /* Initial indent IF at the beginning of a line */
%token <string> EOL        /* End-of-line */
%token <string> COMMENT    /* A comment */
%token <string> SKIPPED    /* A line skipped because not a directive nor a comment */
%token <string> PIPE       /* | */
%token <string> ATAT       /* @@ */
%token <string> AT         /* @ */
%token <string> LCURLCURL  /* {{ */
%token <string> RCURLCURL  /* }} */
%token <string> LCURL      /* { */
%token <string> RCURL      /* } */
%token <string> EQUAL      /* = */
%token <string> COMMA      /* , */
%token <string> TILDE      /* ~ */
%token <string> SLASH      /* / */
%token <character> FLAG    /* Valid characters for flags */
%token TXN                 /* 'txn' keyword */
%token BALANCE             /* 'balance' keyword */
%token OPEN                /* 'open' keyword */
%token CLOSE               /* 'close' keyword */
%token COMMODITY           /* 'commodity' keyword */
%token PAD                 /* 'pad' keyword */
%token EVENT               /* 'event' keyword */
%token PRICE               /* 'price' keyword */
%token NOTE                /* 'note' keyword */
%token DOCUMENT            /* 'document' keyword */
%token PUSHTAG             /* 'pushtag' keyword */
%token POPTAG              /* 'poptag' keyword */
%token OPTION              /* 'option' keyword */
%token INCLUDE             /* 'include' keyword */
%token PLUGIN              /* 'plugin' keyword */
%token <pyobj> BOOL        /* A boolean, true or false */
%token <pyobj> DATE        /* A date object */
%token <pyobj> ACCOUNT     /* The name of an account */
%token <pyobj> CURRENCY    /* A currency specification */
%token <pyobj> STRING      /* A quoted string, with any characters inside */
%token <pyobj> NUMBER      /* A floating-point number */
%token <pyobj> TAG         /* A tag that can be associated with a transaction */
%token <pyobj> LINK        /* A link that can be associated with a transaction */
%token <pyobj> KEY         /* A key in a key-value pair */

/* Types for non-terminal symbols. */
%type <character> txn
%type <character> optflag
%type <pyobj> transaction
%type <pyobj> posting
%type <pyobj> key_value
%type <pyobj> key_value_list
%type <pyobj> key_value_value
%type <pyobj> posting_or_kv_list
%type <pyobj> currency_list
%type <pyobj> open
%type <pyobj> close
%type <pyobj> commodity
%type <pyobj> balance
%type <pyobj> pad
%type <pyobj> amount
%type <pairobj> amount_tolerance
%type <pyobj> position
%type <pyobj> lot_cost_date
%type <pyobj> price
%type <pyobj> event
%type <pyobj> note
%type <pyobj> document
%type <pyobj> entry
%type <pyobj> declarations
%type <pyobj> txn_fields
%type <pyobj> filename
%type <pyobj> opt_booking
%type <pyobj> number_expr


/* Start symbol. */
%start file

/* We have some number of expected shift/reduce conflicts at 'eol'. */
%expect 12


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
    | COMMENT EOL

/* Note: Technically we could have the lexer yield EOF and handle INDENT EOF and
   COMMENT EOF. However this is not necessary. */
empty_line : EOL
           | COMMENT EOL
           | INDENT EOL
           | INDENT
           | COMMENT

/* FIXME: This needs be made more general, dealing with precedence.
   I just need this right now, so I'm putting it in, in a way that will.
   be backwards compatible, so this is just a bit of a temporary hack
   (blais, 2015-04-18). */
number_expr : NUMBER
            {
                $$ = $1;
            }
            | number_expr SLASH NUMBER
            {
                $$ = PyNumber_TrueDivide($1, $3);
            }

txn_fields : empty
           {
               BUILD_NOARGS_X("txn_field_new");
               $$ = build_value;
           }
           | txn_fields STRING
           {
               BUILD_X("txn_field_STRING", "OO", $1, $2);
               DECREF2($1, $2);
               $$ = build_value;
           }
           | txn_fields LINK
           {
               BUILD_X("txn_field_LINK", "OO", $1, $2);
               DECREF2($1, $2);
               $$ = build_value;
           }
           | txn_fields TAG
           {
               BUILD_X("txn_field_TAG", "OO", $1, $2);
               DECREF2($1, $2);
               $$ = build_value;
           }
           | txn_fields PIPE
           {
               /* Mark PIPE as present for backwards compatibility and raise an error */
               BUILD_X("txn_field_PIPE", "OO", $1, Py_None);
               DECREF1($1);
               $$ = build_value;
           }

transaction : DATE txn txn_fields eol posting_or_kv_list
            {
                BUILD_X("transaction", "siObOO", FILE_LINE_ARGS, $1, $2, $3, $5);
                DECREF4($1, $2, $3, $5);
                $$ = build_value;
            }

optflag : empty
        {
            $$ = '\0';
        }
        | FLAG

posting : INDENT optflag ACCOUNT position eol
        {
            BUILD_X("posting", "siOOOOb", FILE_LINE_ARGS, $3, $4, Py_None, Py_False, $2);
            DECREF2($3, $4);
            $$ = build_value;
        }
        | INDENT optflag ACCOUNT position AT amount eol
        {
            BUILD_X("posting", "siOOOOb", FILE_LINE_ARGS, $3, $4, $6, Py_False, $2);
            DECREF3($3, $4, $6);
            $$ = build_value;
        }
        | INDENT optflag ACCOUNT position ATAT amount eol
        {
            BUILD_X("posting", "siOOOOb", FILE_LINE_ARGS, $3, $4, $6, Py_True, $2);
            DECREF3($3, $4, $6);
            $$ = build_value;
        }
        | INDENT optflag ACCOUNT eol
        {
            BUILD_X("posting", "siOOOOb", FILE_LINE_ARGS, $3, Py_None, Py_None, Py_False, $2);
            DECREF1($3);
            $$ = build_value;
        }

key_value : INDENT KEY key_value_value eol
          {
              BUILD_X("key_value", "OO", $2, $3);
              DECREF2($2, $3);
              $$ = build_value;
          }

key_value_value : STRING
                | ACCOUNT
                | DATE
                | CURRENCY
                | TAG
                | NUMBER
                | BOOL
                | amount
                {
                    $$ = $1;
                }
                | empty
                {
                    Py_INCREF(Py_None);
                    $$ = Py_None;
                }

posting_or_kv_list : empty
                   {
                       Py_INCREF(Py_None);
                       $$ = Py_None;
                   }
                   | posting_or_kv_list key_value
                   {
                       BUILD_X("handle_list", "OO", $1, $2);
                       DECREF2($1, $2);
                       $$ = build_value;
                   }
                   | posting_or_kv_list posting
                   {
                       BUILD_X("handle_list", "OO", $1, $2);
                       DECREF2($1, $2);
                       $$ = build_value;
                   }

key_value_list : empty
               {
                   Py_INCREF(Py_None);
                   $$ = Py_None;
               }
               | key_value_list key_value
               {
                   BUILD_X("handle_list", "OO", $1, $2);
                   DECREF2($1, $2);
                   $$ = build_value;
               }

currency_list : empty
              {
                  Py_INCREF(Py_None);
                  $$ = Py_None;
              }
              | CURRENCY
              {
                  BUILD_X("handle_list", "OO", Py_None, $1);
                  DECREF1($1);
                  $$ = build_value;
              }
              | currency_list COMMA CURRENCY
              {
                  BUILD_X("handle_list", "OO", $1, $3);
                  DECREF2($1, $3);
                  $$ = build_value;
              }

pushtag : PUSHTAG TAG eol
         {
             BUILD_X("pushtag", "O", $2);
             DECREF1($2);
             DECREF1(build_value);
         }

poptag : POPTAG TAG eol
       {
           BUILD_X("poptag", "O", $2);
           DECREF1($2);
           DECREF1(build_value);
       }

open : DATE OPEN ACCOUNT currency_list opt_booking eol key_value_list
     {
         BUILD_X("open", "siOOOOO", FILE_LINE_ARGS, $1, $3, $4, $5, $7);
         DECREF5($1, $3, $4, $5, $7);
         $$ = build_value;
     }

opt_booking : STRING
            {
                $$ = $1;
            }
            | empty
            {
                Py_INCREF(Py_None);
                $$ = Py_None;
            }

close : DATE CLOSE ACCOUNT eol key_value_list
      {
          BUILD_X("close", "siOOO", FILE_LINE_ARGS, $1, $3, $5);
          DECREF3($1, $3, $5);
          $$ = build_value;
      }

commodity : DATE COMMODITY CURRENCY eol key_value_list
          {
              BUILD_X("commodity", "siOOO", FILE_LINE_ARGS, $1, $3, $5);
              DECREF3($1, $3, $5);
              $$ = build_value;
          }

pad : DATE PAD ACCOUNT ACCOUNT eol key_value_list
    {
        BUILD_X("pad", "siOOOO", FILE_LINE_ARGS, $1, $3, $4, $6);
        DECREF4($1, $3, $4, $6);
        $$ = build_value;
    }

balance : DATE BALANCE ACCOUNT amount_tolerance eol key_value_list
        {
            BUILD_X("balance", "siOOOOO", FILE_LINE_ARGS, $1, $3, $4.pyobj1, $4.pyobj2, $6);
            DECREF3($1, $3, $6);
            DECREF2($4.pyobj1, $4.pyobj2);
            $$ = build_value;
        }

amount : number_expr CURRENCY
       {
           BUILD_X("amount", "OO", $1, $2);
           DECREF2($1, $2);
           $$ = build_value;
       }

amount_tolerance : number_expr CURRENCY
                 {
                     BUILD_X("amount", "OO", $1, $2);
                     $$.pyobj1 = build_value;
                     $$.pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     DECREF2($1, $2);
                 }
                 | number_expr TILDE number_expr CURRENCY
                 {
                     BUILD_X("amount", "OO", $1, $4);
                     $$.pyobj1 = build_value;
                     $$.pyobj2 = $3;
                     DECREF3($1, $3, $4);
                 }

position : amount
         {
             BUILD_X("position", "siOO", FILE_LINE_ARGS, $1, Py_None);
             DECREF1($1);
             $$ = build_value;
         }
         | amount lot_cost_date
         {
             BUILD_X("position", "siOO", FILE_LINE_ARGS, $1, $2);
             DECREF2($1, $2);
             $$ = build_value;
         }

lot_cost_date : LCURL amount RCURL
         {
             BUILD_X("lot_cost_date", "OOO", $2, Py_None, Py_False);
             DECREF1($2);
             $$ = build_value;
         }
         | LCURL amount SLASH DATE RCURL
         {
             BUILD_X("lot_cost_date", "OOO", $2, $4, Py_False);
             DECREF2($2, $4);
             $$ = build_value;
         }
         | LCURLCURL amount RCURLCURL
         {
             BUILD_X("lot_cost_date", "OOO", $2, Py_None, Py_True);
             DECREF1($2);
             $$ = build_value;
         }
         | LCURLCURL amount SLASH DATE RCURLCURL
         {
             BUILD_X("lot_cost_date", "OOO", $2, $4, Py_True);
             DECREF2($2, $4);
             $$ = build_value;
         }


price : DATE PRICE CURRENCY amount eol key_value_list
      {
          BUILD_X("price", "siOOOO", FILE_LINE_ARGS, $1, $3, $4, $6);
          DECREF4($1, $3, $4, $6);
          $$ = build_value;
      }

event : DATE EVENT STRING STRING eol key_value_list
      {
          BUILD_X("event", "siOOOO", FILE_LINE_ARGS, $1, $3, $4, $6);
          DECREF4($1, $3, $4, $6);
          $$ = build_value;
      }

note : DATE NOTE ACCOUNT STRING eol key_value_list
      {
          BUILD_X("note", "siOOOO", FILE_LINE_ARGS, $1, $3, $4, $6);
          DECREF4($1, $3, $4, $6);
          $$ = build_value;
      }

filename : STRING

document : DATE DOCUMENT ACCOUNT filename eol key_value_list
         {
             BUILD_X("document", "siOOOO", FILE_LINE_ARGS, $1, $3, $4, $6);
             DECREF4($1, $3, $4, $6);
             $$ = build_value;
         }

entry : transaction
      | balance
      | open
      | close
      | pad
      | event
      | note
      | document
      | price
      | commodity
      {
          $$ = $1;
      }

option : OPTION STRING STRING eol
       {
          BUILD_X("option", "siOO", FILE_LINE_ARGS, $2, $3);
          DECREF2($2, $3);
          DECREF1(build_value);
       }

include : INCLUDE STRING eol
       {
          BUILD_X("include", "siO", FILE_LINE_ARGS, $2);
          DECREF1($2);
          DECREF1(build_value);
       }

plugin : PLUGIN STRING eol
       {
          BUILD_X("plugin", "siOO", FILE_LINE_ARGS, $2, Py_None);
          DECREF1($2);
          DECREF1(build_value);
       }
       | PLUGIN STRING STRING eol
       {
          BUILD_X("plugin", "siOO", FILE_LINE_ARGS, $2, $3);
          DECREF2($2, $3);
          DECREF1(build_value);
       }

directive : SKIPPED
          | empty_line
          | pushtag
          | poptag
          | option
          | include
          | plugin


declarations : declarations directive
             {
                 $$ = $1;
             }
             | declarations entry
             {
                 BUILD_X("handle_list", "OO", $1, $2);
                 DECREF2($1, $2);
                 $$ = build_value;
             }
             | declarations error
             {
                 /* {3d95e55b654e} */
                 /* TRACE_ERROR("processing 'error': yytext='%s'.\n", yytext); */

                 /* Ignore the error and continue reducing. */
                 $$ = $1; /* YYABORT; */
             }
             | empty
             {
                  Py_INCREF(Py_None);
                  $$ = Py_None;
             }

file : declarations
     {
         BUILD_X("store_result", "O", $1);
         DECREF1(build_value);
     }


/*--------------------------------------------------------------------------------*/
/* Epilogue */
%%

/* A function that will convert a token name to a string, used in debugging. */
const char* getTokenName(int token)
{
    switch ( token ) {
        case LEX_ERROR : return "LEX_ERROR";
        case INDENT    : return "INDENT";
        case EOL       : return "EOL";
        case COMMENT   : return "COMMENT";
        case SKIPPED   : return "SKIPPED";
        case PIPE      : return "PIPE";
        case ATAT      : return "ATAT";
        case AT        : return "AT";
        case LCURL     : return "LCURL";
        case RCURL     : return "RCURL";
        case EQUAL     : return "EQUAL";
        case COMMA     : return "COMMA";
        case SLASH     : return "SLASH";
        case FLAG      : return "FLAG";
        case TXN       : return "TXN";
        case BALANCE   : return "BALANCE";
        case OPEN      : return "OPEN";
        case CLOSE     : return "CLOSE";
        case PAD       : return "PAD";
        case EVENT     : return "EVENT";
        case PRICE     : return "PRICE";
        case NOTE      : return "NOTE";
        case DOCUMENT  : return "DOCUMENT";
        case PUSHTAG   : return "PUSHTAG";
        case POPTAG    : return "POPTAG";
        case OPTION    : return "OPTION";
        case DATE      : return "DATE";
        case ACCOUNT   : return "ACCOUNT";
        case CURRENCY  : return "CURRENCY";
        case STRING    : return "STRING";
        case NUMBER    : return "NUMBER";
        case TAG       : return "TAG";
        case LINK      : return "LINK";
        case KEY       : return "KEY";
    }
    return 0;
}
