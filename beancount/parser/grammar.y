/* -*- mode: c -*- */
/*
 * Parser grammar for beancount 2.0 input syntax.
 *
 * This assumes it feeds off the corresponding lexer in this package. This is
 * meant to be used with the stock "go yacc" command.
 */

/*--------------------------------------------------------------------------------*/
/* Prologue */

%code requires {

#include <stdio.h>
#include <assert.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

/* Extend default location type with file name information. */
typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
  PyObject* file_name;
} YYLTYPE;

#define YYLTYPE_IS_DECLARED 1

/* Extend defult location action to copy file name over. */
#define YYLLOC_DEFAULT(current, rhs, N)                                 \
  do {                                                                \
      if (N) {                                                        \
          (current).first_line   = YYRHSLOC(rhs, 1).first_line;       \
          (current).first_column = YYRHSLOC(rhs, 1).first_column;     \
          (current).last_line    = YYRHSLOC(rhs, N).last_line;        \
          (current).last_column  = YYRHSLOC(rhs, N).last_column;      \
          (current).file_name    = YYRHSLOC(rhs, N).file_name;        \
      } else {                                                        \
          (current).first_line   = (current).last_line =              \
              YYRHSLOC(rhs, 0).last_line;                             \
          (current).first_column = (current).last_column =            \
              YYRHSLOC(rhs, 0).last_column;                           \
          (current).file_name    = YYRHSLOC(rhs, 0).file_name;        \
      }                                                               \
  } while (0)

/* Get a printable version of a token name. */
const char* token_to_string(int token);

}

%{

#include "beancount/parser/macros.h"
#include "beancount/parser/grammar.h"
#include "beancount/parser/lexer.h"

extern YY_DECL;

/*
 * Call a builder method and detect and handle a Python exception being raised
 * in the handler. Always run the code to clean the references provided by the
 * reduced rule. {05bb0fb60e86}
 */
#define BUILDY(clean, target, method_name, format, ...)                         \
  target = PyObject_CallMethod(builder, method_name, "Oi" format,             \
                               FILENAME, LINENO, ## __VA_ARGS__);             \
  clean;                                                                      \
  if (target == NULL) {                                                       \
      build_grammar_error_from_exception(&yyloc, builder);                    \
      YYERROR;                                                                \
  }

#define MISSING_OBJ (yyget_extra(scanner)->missing_obj)

#define FILENAME (yyloc).file_name
#define LINENO (yyloc).first_line

/* Build a grammar error from the exception context. */
void build_grammar_error_from_exception(YYLTYPE* loc, PyObject* builder)
{
  /* Get the exception context. */
  PyObject* ptype;
  PyObject* pvalue;
  PyObject* ptraceback;
  PyErr_Fetch(&ptype, &pvalue, &ptraceback);
  PyErr_NormalizeException(&ptype, &pvalue, &ptraceback);

  /* Clear the exception. */
  PyErr_Clear();

  if (pvalue != NULL) {
      /* Build and accumulate a new error object. {27d1d459c5cd} */
      PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "OiOOO",
                                         loc->file_name, loc->first_line,
                                         pvalue, ptype, ptraceback);
      if (rv == NULL) {
          /* Note: Leave the internal error trickling up its detail. */
          /* PyErr_SetString(PyExc_RuntimeError, */
          /*                 "Internal error: While building exception"); */
      }
  }
  else {
      PyErr_SetString(PyExc_RuntimeError,
                      "Internal error: No exception");
  }

  Py_XDECREF(ptype);
  Py_XDECREF(pvalue);
  Py_XDECREF(ptraceback);
}

/* Error-handling function. {ca6aab8b9748} */
void yyerror(YYLTYPE* loc, yyscan_t scanner, PyObject* builder, char const* message)
{
  /* Register a syntax error with the builder. */
  PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "Ois",
                                     loc->file_name, loc->first_line,
                                     message);
  if (rv == NULL) {
      PyErr_SetString(PyExc_RuntimeError,
                      "Internal error: Building exception from yyerror()");
  }
  Py_XDECREF(rv);
}

#define DECREF(...) _CC_FUNC(Py_DECREF, __VA_ARGS__)

%}


/*--------------------------------------------------------------------------------*/
/* Bison Declarations */

/* Options. */
%require "3.6"

%defines
%debug
%locations
%define parse.error verbose
%define api.pure full
%param {yyscan_t scanner}
%param {PyObject* builder}

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
%token <string> INDENT     /* Initial indent IF at the beginning of a line */
%token <string> EOL        /* End-of-line */
%token <string> PIPE       /* | */
%token <string> ATAT       /* @@ */
%token <string> AT         /* @ */
%token <string> LCURLCURL  /* {{ */
%token <string> RCURLCURL  /* }} */
%token <string> LCURL      /* { */
%token <string> RCURL      /* } */
%token <string> COMMA      /* , */
%token <string> TILDE      /* ~ */
%token <string> HASH       /* # */
%token <string> ASTERISK   /* * */
%token <string> SLASH      /* / */
%token <string> COLON      /* : */
%token <string> PLUS       /* + */
%token <string> MINUS      /* - */
%token <string> LPAREN     /* ( */
%token <string> RPAREN     /* ) */
%token <character> FLAG    /* Valid characters for flags */
%token <character> CAPITAL /* Valid characters for flags */
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
%token QUERY               /* 'query' keyword */
%token CUSTOM              /* 'custom' keyword */
%token PUSHTAG             /* 'pushtag' keyword */
%token POPTAG              /* 'poptag' keyword */
%token PUSHMETA            /* 'pushmeta' keyword */
%token POPMETA             /* 'popmeta' keyword */
%token OPTION              /* 'option' keyword */
%token INCLUDE             /* 'include' keyword */
%token PLUGIN              /* 'plugin' keyword */
%token <pyobj> NONE        /* A None value (parsed as NULL) */
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
%type <pyobj> currency
%type <pyobj> account
%type <pyobj> transaction
%type <pyobj> posting
%type <pyobj> key_value
%type <pyobj> key_value_line
%type <pyobj> key_value_list
%type <pyobj> key_value_value
%type <pyobj> posting_or_kv_list
%type <pyobj> currency_list
%type <pyobj> open
%type <pyobj> close
%type <pyobj> commodity
%type <pyobj> balance
%type <pyobj> pad
%type <pairobj> amount_tolerance
%type <pyobj> amount
%type <pyobj> incomplete_amount
%type <pyobj> compound_amount
%type <pyobj> maybe_number
%type <pyobj> maybe_currency
%type <pyobj> price_annotation
%type <pyobj> cost_comp
%type <pyobj> cost_comp_list
%type <pyobj> cost_spec
%type <pyobj> price
%type <pyobj> event
%type <pyobj> query
%type <pyobj> note
%type <pyobj> document
%type <pyobj> entry
%type <pyobj> declarations
%type <pyobj> txn_strings
%type <pyobj> tags_links
%type <pyobj> filename
%type <pyobj> opt_booking
%type <pyobj> number_expr
%type <pyobj> option
%type <pyobj> pushtag
%type <pyobj> poptag
%type <pyobj> pushmeta
%type <pyobj> popmeta
%type <pyobj> include
%type <pyobj> plugin
%type <pyobj> file
%type <pyobj> custom
%type <pyobj> custom_value
%type <pyobj> custom_value_list

/* Operator precedence.
 * This is pulled straight out of the textbook example:
 * https://www.gnu.org/software/bison/manual/html_node/Infix-Calc.html#Infix-Calc
 */
%left MINUS PLUS
%left ASTERISK SLASH
%precedence NEGATIVE /* negation--unary minus */

/* Start symbol. */
%start file

/* We have some number of expected shift/reduce conflicts at 'eol'. */
%expect 8


/*--------------------------------------------------------------------------------*/
/* Grammar Rules */
%%

/* A transaction declaration can be either 'txn' or one of the special character flags. */
txn:
  TXN
    {
      $$ = '*';
    }
  | FLAG
    {
        $$ = $1;
    }
  | ASTERISK
    {
        $$ = '*';
    }
  | HASH
    {
        $$ = '#';
    }
  | CAPITAL
    {
        $$ = $1;
    }

eol: EOL | YYEOF

/* FIXME: This needs be made more general, dealing with precedence.
   I just need this right now, so I'm putting it in, in a way that will.
   be backwards compatible, so this is just a bit of a temporary hack
   (blais, 2015-04-18). */
number_expr:
  NUMBER
  | number_expr PLUS number_expr
    {
      $$ = PyNumber_Add($1, $3);
      DECREF($1, $3);
    }
  | number_expr MINUS number_expr
    {
        $$ = PyNumber_Subtract($1, $3);
        DECREF($1, $3);
    }
  | number_expr ASTERISK number_expr
    {
        $$ = PyNumber_Multiply($1, $3);
        DECREF($1, $3);
    }
  | number_expr SLASH number_expr
    {
        $$ = PyNumber_TrueDivide($1, $3);
        DECREF($1, $3);
    }
  | MINUS number_expr %prec NEGATIVE
    {
        $$ = PyNumber_Negative($2);
        DECREF($2);
    }
  | PLUS number_expr %prec NEGATIVE
    {
        $$ = $2;
    }
  | LPAREN number_expr RPAREN
    {
        $$ = $2;
    }

currency:
  CURRENCY
    {
        $$ = $1;
    }
  | CAPITAL
    {
        $$ = PyUnicode_FromStringAndSize(&$1, 1);
    }

txn_strings:
  %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }
  | txn_strings STRING
    {
        BUILDY(DECREF($1, $2),
               $$, "handle_list", "OO", $1, $2);
    }
  | txn_strings PIPE
    {
        BUILDY(,
               $$, "pipe_deprecated_error", "");
        $$ = $1;
    }

tags_links:
  %empty
    {
        BUILDY(,
               $$, "tag_link_new", "");
    }
  | tags_links LINK
    {
        BUILDY(DECREF($1, $2),
               $$, "tag_link_LINK", "OO", $1, $2);
    }
  | tags_links TAG
    {
        BUILDY(DECREF($1, $2),
               $$, "tag_link_TAG", "OO", $1, $2);
    }

transaction:
  DATE txn txn_strings tags_links eol posting_or_kv_list
    {
        BUILDY(DECREF($1, $3, $4, $6),
               $$, "transaction", "ObOOO", $1, $2, $3, $4, $6);
    }

optflag:
  %empty
    {
        $$ = '\0';
    }
  | ASTERISK
    {
        $$ = '*';
    }
  | HASH
    {
        $$ = '#';
    }
  | FLAG
  | CAPITAL

price_annotation:
  incomplete_amount

account:
  ACCOUNT
    {
        BUILDY(DECREF($1),
               $$, "account", "O", $1);
    }

posting:
  INDENT optflag account incomplete_amount cost_spec eol
    {
        BUILDY(DECREF($3, $4, $5),
               $$, "posting", "OOOOOb", $3, $4, $5, Py_None, Py_False, $2);
    }
  | INDENT optflag account incomplete_amount cost_spec AT price_annotation eol
    {
        BUILDY(DECREF($3, $4, $5, $7),
               $$, "posting", "OOOOOb", $3, $4, $5, $7, Py_False, $2);
    }
  | INDENT optflag account incomplete_amount cost_spec ATAT price_annotation eol
    {
        BUILDY(DECREF($3, $4, $5, $7),
               $$, "posting", "OOOOOb", $3, $4, $5, $7, Py_True, $2);
    }
  | INDENT optflag account eol
    {
        BUILDY(DECREF($3),
               $$, "posting", "OOOOOb", $3, MISSING_OBJ, Py_None, Py_None, Py_False, $2);
    }

key_value:
  KEY COLON key_value_value
    {
        BUILDY(DECREF($1, $3),
               $$, "key_value", "OO", $1, $3);
    }

key_value_line:
  INDENT key_value eol
    {
        $$ = $2;
    }

key_value_value:
  STRING
  | account
  | DATE
  | currency
  | TAG
  | BOOL
  | NONE
  | number_expr
  | amount
  | %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }

posting_or_kv_list:
  %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }
  | posting_or_kv_list INDENT eol
    {
        $$ = $1;
    }
  | posting_or_kv_list INDENT tags_links eol
    {
        BUILDY(DECREF($1, $3),
               $$, "handle_list", "OO", $1, $3);
    }
  | posting_or_kv_list key_value_line
    {
        BUILDY(DECREF($1, $2),
               $$, "handle_list", "OO", $1, $2);
    }
  | posting_or_kv_list posting
    {
        BUILDY(DECREF($1, $2),
               $$, "handle_list", "OO", $1, $2);
    }

key_value_list:
  %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }
  | key_value_list INDENT eol
    {
        $$ = $1;
    }
  | key_value_list key_value_line
    {
        BUILDY(DECREF($1, $2),
               $$, "handle_list", "OO", $1, $2);
    }

currency_list:
  %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }
  | currency
    {
        BUILDY(DECREF($1),
               $$, "handle_list", "OO", Py_None, $1);
    }
  | currency_list COMMA currency
    {
        BUILDY(DECREF($1, $3),
               $$, "handle_list", "OO", $1, $3);
    }

pushtag:
  PUSHTAG TAG eol
    {
        BUILDY(DECREF($2),
               $$, "pushtag", "O", $2);
    }

poptag:
  POPTAG TAG eol
    {
        BUILDY(DECREF($2),
               $$, "poptag", "O", $2);
    }

pushmeta:
  PUSHMETA key_value eol
    {
        /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
         * within a tuple, so expand in the method (it receives two
         * objects). See https://docs.python.org/3.4/c-api/arg.html. */
        BUILDY(DECREF($2),
               $$, "pushmeta", "O", $2);
    }

popmeta:
  POPMETA KEY COLON eol
    {
        BUILDY(DECREF($2),
               $$, "popmeta", "O", $2);
    }

open:
  DATE OPEN account currency_list opt_booking eol key_value_list
    {
        BUILDY(DECREF($1, $3, $4, $5, $7),
               $$, "open", "OOOOO", $1, $3, $4, $5, $7);
    }

opt_booking:
  STRING
  | %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }

close:
  DATE CLOSE account eol key_value_list
    {
        BUILDY(DECREF($1, $3, $5),
               $$, "close", "OOO", $1, $3, $5);
    }

commodity:
  DATE COMMODITY currency eol key_value_list
    {
        BUILDY(DECREF($1, $3, $5),
               $$, "commodity", "OOO", $1, $3, $5);
    }

pad:
  DATE PAD account account eol key_value_list
    {
        BUILDY(DECREF($1, $3, $4, $6),
               $$, "pad", "OOOO", $1, $3, $4, $6);
    }

balance:
  DATE BALANCE account amount_tolerance eol key_value_list
    {
        BUILDY(DECREF($1, $3, $6, $4.pyobj1, $4.pyobj2),
               $$, "balance", "OOOOO", $1, $3, $4.pyobj1, $4.pyobj2, $6);
    }

amount:
  number_expr currency
    {
        BUILDY(DECREF($1, $2),
               $$, "amount", "OO", $1, $2);
    }

amount_tolerance:
  number_expr currency
    {
        BUILDY(DECREF($1, $2),
               $$.pyobj1, "amount", "OO", $1, $2);
        $$.pyobj2 = Py_None;
        Py_INCREF(Py_None);
    }
  | number_expr TILDE number_expr currency
    {
        BUILDY(DECREF($1, $4),
               $$.pyobj1, "amount", "OO", $1, $4);
        $$.pyobj2 = $3;
    }

maybe_number:
  number_expr
  | %empty
    {
        Py_INCREF(MISSING_OBJ);
        $$ = MISSING_OBJ;
    }

maybe_currency:
  currency
  | %empty
    {
        Py_INCREF(MISSING_OBJ);
        $$ = MISSING_OBJ;
    }

compound_amount:
  maybe_number currency
    {
        BUILDY(DECREF($1, $2),
               $$, "compound_amount", "OOO", $1, Py_None, $2);
    }
  | number_expr maybe_currency
    {
        BUILDY(DECREF($1, $2),
               $$, "compound_amount", "OOO", $1, Py_None, $2);
    }
  | maybe_number HASH maybe_number currency
    {
        BUILDY(DECREF($1, $3, $4),
               $$, "compound_amount", "OOO", $1, $3, $4);
        ;
    }

incomplete_amount:
  maybe_number maybe_currency
    {
        BUILDY(DECREF($1, $2),
               $$, "amount", "OO", $1, $2);
    }

cost_spec:
  LCURL cost_comp_list RCURL
    {
        BUILDY(DECREF($2),
               $$, "cost_spec", "OO", $2, Py_False);
    }
  | LCURLCURL cost_comp_list RCURLCURL
    {
        BUILDY(DECREF($2),
               $$, "cost_spec", "OO", $2, Py_True);
    }
  | %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }

cost_comp_list:
  %empty
    {
        /* We indicate that there was a cost if there */
        $$ = PyList_New(0);
    }
  | cost_comp
    {
        BUILDY(DECREF($1),
               $$, "handle_list", "OO", Py_None, $1);
    }
  | cost_comp_list COMMA cost_comp
    {
        BUILDY(DECREF($1, $3),
               $$, "handle_list", "OO", $1, $3);
    }

cost_comp:
  compound_amount
  | DATE
  | STRING
  | ASTERISK
    {
        BUILDY(,
               $$, "cost_merge", "O", Py_None);
    }

price:
  DATE PRICE currency amount eol key_value_list
    {
        BUILDY(DECREF($1, $3, $4, $6),
               $$, "price", "OOOO", $1, $3, $4, $6);
    }

event:
  DATE EVENT STRING STRING eol key_value_list
    {
        BUILDY(DECREF($1, $3, $4, $6),
               $$, "event", "OOOO", $1, $3, $4, $6);
    }

query:
  DATE QUERY STRING STRING eol key_value_list
       {
           BUILDY(DECREF($1, $3, $4, $6),
                  $$, "query", "OOOO", $1, $3, $4, $6);
       }

note:
  DATE NOTE account STRING tags_links eol key_value_list
    {
      BUILDY(DECREF($1, $3, $4, $5, $7),
               $$, "note", "OOOOO", $1, $3, $4, $5, $7);
    }

filename: STRING

document:
  DATE DOCUMENT account filename tags_links eol key_value_list
    {
        BUILDY(DECREF($1, $3, $4, $5, $7),
               $$, "document", "OOOOO", $1, $3, $4, $5, $7);
    }

custom_value:
  STRING
    {
        BUILDY(DECREF($1),
               $$, "custom_value", "OO", $1, Py_None);
    }
  | DATE
    {
        BUILDY(DECREF($1),
               $$, "custom_value", "OO", $1, Py_None);
    }
  | BOOL
    {
        BUILDY(DECREF($1),
               $$, "custom_value", "OO", $1, Py_None);
    }
  | amount
    {
        BUILDY(DECREF($1),
               $$, "custom_value", "OO", $1, Py_None);
    }
  | number_expr
    {
        BUILDY(DECREF($1),
               $$, "custom_value", "OO", $1, Py_None);
    }
  | account
    {
        /* Obtain beancount.core.account.TYPE */
        PyObject* module = PyImport_ImportModule("beancount.core.account");
        PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
        Py_DECREF(module);
        BUILDY(DECREF($1, dtype),
               $$, "custom_value", "OO", $1, dtype);
    }

custom_value_list:
  %empty
    {
        Py_INCREF(Py_None);
        $$ = Py_None;
    }
  | custom_value_list custom_value
    {
        BUILDY(DECREF($1, $2),
               $$, "handle_list", "OO", $1, $2);
    }

custom:
  DATE CUSTOM STRING custom_value_list eol key_value_list
   {
       BUILDY(DECREF($1, $3, $4, $6),
              $$, "custom", "OOOO", $1, $3, $4, $6);
   }

entry:
  transaction
  | balance
  | open
  | close
  | pad
  | document
  | note
  | event
  | price
  | commodity
  | query
  | custom

option:
  OPTION STRING STRING eol
    {
        BUILDY(DECREF($2, $3),
               $$, "option", "OO", $2, $3);
    }

include:
  INCLUDE STRING eol
    {
        BUILDY(DECREF($2),
               $$, "include", "O", $2);
    }

plugin:
  PLUGIN STRING eol
    {
        BUILDY(DECREF($2),
               $$, "plugin", "OO", $2, Py_None);
    }
  | PLUGIN STRING STRING eol
    {
        BUILDY(DECREF($2, $3),
               $$, "plugin", "OO", $2, $3);
    }

directive:
  pushtag
  | poptag
  | pushmeta
  | popmeta
  | option
  | include
  | plugin

declarations:
  declarations EOL
  | declarations directive
  | declarations entry
    {
        BUILDY(DECREF($1, $2),
               $$, "handle_list", "OO", $1, $2);
    }
  | declarations error
    {
        /*
         * Ignore the error and continue reducing ({3d95e55b654e}).
         * Note that with the matching rule above, "error" will
         * successfully reduce on each line that cannot reduce.
         * Non-erroneous postings after an error occurs will reduce but
         * not be included because a transaction's list of postings
         * does not include an "error" rule.
         *
         * Note: Adding EOL after the "error" rule above works to
         * reduce the number of calls to this rule resulting from the
         * appearance of an error but makes the parser errors
         * skip the next valid directive, so we just have to make sure
         * repeated runs of this rule's handling code are idempotent.
         */
        $$ = $1;
    }
  | %empty
    {
         Py_INCREF(Py_None);
         $$ = Py_None;
    }


file:
  declarations YYEOF
    {
        BUILDY(DECREF($1),
               $$, "store_result", "O", $1);
    }


/*--------------------------------------------------------------------------------*/
/* Epilogue */
%%

/* Get a printable version of a token name. */
const char* token_to_string(int token)
{
  return yytname[YYTRANSLATE(token)];
}
