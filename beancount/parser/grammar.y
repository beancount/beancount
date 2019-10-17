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
#include "parser.h"

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

}

%{

#include "grammar.h"
#include "lexer.h"

extern YY_DECL;

/*
 * Call a builder method and detect and handle a Python exception being raised
 * in the handler. Always run the code to clean the references provided by the
 * reduced rule. {05bb0fb60e86}
 */
#define BUILD(clean, target, method_name, format, ...)                  \
    target = PyObject_CallMethod(builder, method_name, format, __VA_ARGS__);    \
    clean;                                                              \
    if (target == NULL) {                                               \
        build_grammar_error_from_exception(builder, yyloc);             \
        YYERROR;                                                        \
    }

#define FILENAME (yyloc).file_name
#define LINENO (yyloc).first_line

/* Build a grammar error from the exception context. */
void build_grammar_error_from_exception(PyObject* builder, YYLTYPE yyloc)
{
    PyObject* traceback = NULL;
    PyObject* value = NULL;
    PyObject* type = NULL;
    PyObject* rv = NULL;

    /* Get the exception context. */
    PyErr_Fetch(&type, &value, &traceback);
    PyErr_NormalizeException(&type, &value, &traceback);

    if (value) {
        /* traceback can be NULL. This is the case for exceptions
         * raised in C code for example. Replace NULL with Py_None to
         * keep PyObject_CallMethod() happy. */
        traceback = traceback ? traceback : Py_None;

        /* Build and accumulate a new error object. {27d1d459c5cd} */
        rv = PyObject_CallMethod(builder, "build_grammar_error", "OiOOO",
                                 yyloc.file_name, yyloc.first_line,
                                 value, type, traceback);
    } else {
        PyErr_SetString(PyExc_RuntimeError, "No exception");
    }

    Py_XDECREF(rv);
    Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(traceback);
}

/* Error-handling function. {ca6aab8b9748} */
void yyerror(YYLTYPE *locp, yyscan_t scanner, PyObject* builder, char const* message)
{
    PyObject* rv = NULL;

    /* Skip lex errors: they have already been registered the lexer itself. */
    if (strstr(message, "LEX_ERROR"))
        return;

    /* Register a syntax error with the builder. */
    rv = PyObject_CallMethod(builder, "build_grammar_error", "Ois",
                             locp[0].file_name, locp[0].first_line,
                             message);

    Py_XDECREF(rv);
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


/* Macros to clean up memory for temporaries in rule reductions. */
#define DECREF1(x1)                        Py_DECREF(x1);
#define DECREF2(x1, x2)                    DECREF1(x1); Py_DECREF(x2);
#define DECREF3(x1, x2, x3)                DECREF2(x1, x2); Py_DECREF(x3);
#define DECREF4(x1, x2, x3, x4)            DECREF3(x1, x2, x3); Py_DECREF(x4);
#define DECREF5(x1, x2, x3, x4, x5)        DECREF4(x1, x2, x3, x4); Py_DECREF(x5);
#define DECREF6(x1, x2, x3, x4, x5, x6)    DECREF5(x1, x2, x3, x4, x5); Py_DECREF(x6);

%}


/*--------------------------------------------------------------------------------*/
/* Bison Declarations */

/* Options. */
%defines
%error-verbose
%debug
%locations
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
%token <string> HASH       /* # */
%token <string> ASTERISK   /* * */
%token <string> SLASH      /* / */
%token <string> COLON      /* : */
%token <string> PLUS       /* + */
%token <string> MINUS      /* - */
%token <string> LPAREN     /* ( */
%token <string> RPAREN     /* ) */
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
%expect 17


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
    | ASTERISK
    {
        $$ = '*';
    }
    | HASH
    {
        $$ = '#';
    }

eol : EOL
    | COMMENT EOL

/* Note: Technically we could have the lexer yield EOF and handle INDENT EOF and
   COMMENT EOF. However this is not necessary. */
empty_line : EOL
           | COMMENT
           | INDENT
           | SKIPPED

/* FIXME: This needs be made more general, dealing with precedence.
   I just need this right now, so I'm putting it in, in a way that will.
   be backwards compatible, so this is just a bit of a temporary hack
   (blais, 2015-04-18). */
number_expr : NUMBER
            {
                $$ = $1;
            }
            | number_expr PLUS number_expr
            {
                $$ = PyNumber_Add($1, $3);
                DECREF2($1, $3);
            }
            | number_expr MINUS number_expr
            {
                $$ = PyNumber_Subtract($1, $3);
                DECREF2($1, $3);
            }
            | number_expr ASTERISK number_expr
            {
                $$ = PyNumber_Multiply($1, $3);
                DECREF2($1, $3);
            }
            | number_expr SLASH number_expr
            {
                $$ = PyNumber_TrueDivide($1, $3);
                DECREF2($1, $3);
            }
            | MINUS number_expr %prec NEGATIVE
            {
                $$ = PyNumber_Negative($2);
                DECREF1($2);
            }
            | PLUS number_expr %prec NEGATIVE
            {
                $$ = $2;
            }
            | LPAREN number_expr RPAREN
            {
                $$ = $2;
            }

txn_strings : empty
            {
                Py_INCREF(Py_None);
                $$ = Py_None;
            }
            | txn_strings STRING
            {
                BUILD(DECREF2($1, $2),
                       $$, "handle_list", "OO", $1, $2);
            }
            | txn_strings PIPE
            {
                BUILD(,
                       $$, "pipe_deprecated_error", "Oi", FILENAME, LINENO);
                $$ = $1;
            }

tags_links : empty
           {
               /* Note: We're passing a bogus value here in order to avoid
                * having to declare a second macro just for this one special
                * case. */
               BUILD(,
                      $$, "tag_link_new", "O", Py_None);
           }
           | tags_links LINK
           {
               BUILD(DECREF2($1, $2),
                      $$, "tag_link_LINK", "OO", $1, $2);
           }
           | tags_links TAG
           {
               BUILD(DECREF2($1, $2),
                      $$, "tag_link_TAG", "OO", $1, $2);
           }

transaction : DATE txn txn_strings tags_links eol posting_or_kv_list
            {
                BUILD(DECREF4($1, $3, $4, $6),
                       $$, "transaction", "OiObOOO", FILENAME, LINENO, $1, $2, $3, $4, $6);
            }

optflag : empty
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

price_annotation : incomplete_amount
                 {
                     $$ = $1;
                 }

posting : INDENT optflag ACCOUNT incomplete_amount cost_spec eol
        {
            BUILD(DECREF3($3, $4, $5),
                   $$, "posting", "OiOOOOOb", FILENAME, LINENO, $3, $4, $5, Py_None, Py_False, $2);
        }
        | INDENT optflag ACCOUNT incomplete_amount cost_spec AT price_annotation eol
        {
            BUILD(DECREF4($3, $4, $5, $7),
                   $$, "posting", "OiOOOOOb", FILENAME, LINENO, $3, $4, $5, $7, Py_False, $2);
        }
        | INDENT optflag ACCOUNT incomplete_amount cost_spec ATAT price_annotation eol
        {
            BUILD(DECREF4($3, $4, $5, $7),
                   $$, "posting", "OiOOOOOb", FILENAME, LINENO, $3, $4, $5, $7, Py_True, $2);
        }
        | INDENT optflag ACCOUNT eol
        {
            BUILD(DECREF1($3),
                   $$, "posting", "OiOOOOOb", FILENAME, LINENO, $3, missing, Py_None, Py_None, Py_False, $2);
        }

key_value : KEY COLON key_value_value
          {
              BUILD(DECREF2($2, $3),
                     $$, "key_value", "OO", $2, $3);
          }

key_value_line : INDENT key_value eol
               {
                   $$ = $2;
               }

key_value_value : STRING
                | ACCOUNT
                | DATE
                | CURRENCY
                | TAG
                | BOOL
                | NONE
                | number_expr
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
                   | posting_or_kv_list INDENT COMMENT EOL
                   {
                       $$ = $1;
                   }
                   | posting_or_kv_list INDENT tags_links EOL
                   {
                       BUILD(DECREF2($1, $3),
                              $$, "handle_list", "OO", $1, $3);
                   }
                   | posting_or_kv_list key_value_line
                   {
                       BUILD(DECREF2($1, $2),
                              $$, "handle_list", "OO", $1, $2);
                   }
                   | posting_or_kv_list posting
                   {
                       BUILD(DECREF2($1, $2),
                              $$, "handle_list", "OO", $1, $2);
                   }

key_value_list : empty
               {
                   Py_INCREF(Py_None);
                   $$ = Py_None;
               }
               | key_value_list key_value_line
               {
                   BUILD(DECREF2($1, $2),
                          $$, "handle_list", "OO", $1, $2);
               }

currency_list : empty
              {
                  Py_INCREF(Py_None);
                  $$ = Py_None;
              }
              | CURRENCY
              {
                  BUILD(DECREF1($1),
                         $$, "handle_list", "OO", Py_None, $1);
              }
              | currency_list COMMA CURRENCY
              {
                  BUILD(DECREF2($1, $3),
                         $$, "handle_list", "OO", $1, $3);
              }

pushtag : PUSHTAG TAG eol
         {
             BUILD(DECREF1($2),
                    $$, "pushtag", "O", $2);
         }

poptag : POPTAG TAG eol
       {
           BUILD(DECREF1($2),
                  $$, "poptag", "O", $2);
       }

pushmeta : PUSHMETA key_value eol
         {
             /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
              * within a tuple, so expand in the method (it receives two
              * objects). See https://docs.python.org/3.4/c-api/arg.html. */
             BUILD(DECREF1($2),
                    $$, "pushmeta", "O", $2);
         }

popmeta : POPMETA KEY COLON eol
        {
            BUILD(DECREF1($2),
                   $$, "popmeta", "O", $2);
        }

open : DATE OPEN ACCOUNT currency_list opt_booking eol key_value_list
     {
         BUILD(DECREF5($1, $3, $4, $5, $7),
                $$, "open", "OiOOOOO", FILENAME, LINENO, $1, $3, $4, $5, $7);
         ;
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
          BUILD(DECREF3($1, $3, $5),
                 $$, "close", "OiOOO", FILENAME, LINENO, $1, $3, $5);
      }

commodity : DATE COMMODITY CURRENCY eol key_value_list
          {
              BUILD(DECREF3($1, $3, $5),
                     $$, "commodity", "OiOOO", FILENAME, LINENO, $1, $3, $5);
          }

pad : DATE PAD ACCOUNT ACCOUNT eol key_value_list
    {
        BUILD(DECREF4($1, $3, $4, $6),
               $$, "pad", "OiOOOO", FILENAME, LINENO, $1, $3, $4, $6);
    }

balance : DATE BALANCE ACCOUNT amount_tolerance eol key_value_list
        {
            BUILD(DECREF5($1, $3, $6, $4.pyobj1, $4.pyobj2),
                   $$, "balance", "OiOOOOO", FILENAME, LINENO, $1, $3, $4.pyobj1, $4.pyobj2, $6);
        }

amount : number_expr CURRENCY
       {
           BUILD(DECREF2($1, $2),
                  $$, "amount", "OO", $1, $2);
       }

amount_tolerance : number_expr CURRENCY
                 {
                     BUILD(DECREF2($1, $2),
                            $$.pyobj1, "amount", "OO", $1, $2);
                     $$.pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
                 | number_expr TILDE number_expr CURRENCY
                 {
                     BUILD(DECREF2($1, $4),
                            $$.pyobj1, "amount", "OO", $1, $4);
                     $$.pyobj2 = $3;
                 }

maybe_number : empty
             {
                 Py_INCREF(missing);
                 $$ = missing;
             }
             | number_expr
             {
                 $$ = $1;
             }

maybe_currency : empty
             {
                 Py_INCREF(missing);
                 $$ = missing;
             }
             | CURRENCY
             {
                 $$ = $1;
             }

compound_amount : maybe_number CURRENCY
                {
                    BUILD(DECREF2($1, $2),
                           $$, "compound_amount", "OOO", $1, Py_None, $2);
                }
                | number_expr maybe_currency
                {
                    BUILD(DECREF2($1, $2),
                           $$, "compound_amount", "OOO", $1, Py_None, $2);
                }
                | maybe_number HASH maybe_number CURRENCY
                {
                    BUILD(DECREF3($1, $3, $4),
                           $$, "compound_amount", "OOO", $1, $3, $4);
                    ;
                }

incomplete_amount : maybe_number maybe_currency
                  {
                      BUILD(DECREF2($1, $2),
                             $$, "amount", "OO", $1, $2);
                 }

cost_spec : LCURL cost_comp_list RCURL
          {
              BUILD(DECREF1($2),
                     $$, "cost_spec", "OO", $2, Py_False);
          }
          | LCURLCURL cost_comp_list RCURLCURL
          {
              BUILD(DECREF1($2),
                     $$, "cost_spec", "OO", $2, Py_True);
          }
          | empty
          {
              Py_INCREF(Py_None);
              $$ = Py_None;
          }

cost_comp_list : empty
               {
                   /* We indicate that there was a cost if there */
                   $$ = PyList_New(0);
               }
               | cost_comp
               {
                   BUILD(DECREF1($1),
                          $$, "handle_list", "OO", Py_None, $1);
               }
               | cost_comp_list COMMA cost_comp
               {
                   BUILD(DECREF2($1, $3),
                          $$, "handle_list", "OO", $1, $3);
               }

cost_comp : compound_amount
          {
              $$ = $1;
          }
          | DATE
          {
              $$ = $1;
          }
          | STRING
          {
              $$ = $1;
          }
          | ASTERISK
          {
              BUILD(,
                     $$, "cost_merge", "O", Py_None);
          }


price : DATE PRICE CURRENCY amount eol key_value_list
      {
          BUILD(DECREF4($1, $3, $4, $6),
                 $$, "price", "OiOOOO", FILENAME, LINENO, $1, $3, $4, $6);
      }

event : DATE EVENT STRING STRING eol key_value_list
      {
          BUILD(DECREF4($1, $3, $4, $6),
                 $$, "event", "OiOOOO", FILENAME, LINENO, $1, $3, $4, $6);
      }

query : DATE QUERY STRING STRING eol key_value_list
         {
             BUILD(DECREF4($1, $3, $4, $6),
                    $$, "query", "OiOOOO", FILENAME, LINENO, $1, $3, $4, $6);
         }

note : DATE NOTE ACCOUNT STRING eol key_value_list
      {
          BUILD(DECREF4($1, $3, $4, $6),
                 $$, "note", "OiOOOO", FILENAME, LINENO, $1, $3, $4, $6);
      }

filename : STRING

document : DATE DOCUMENT ACCOUNT filename tags_links eol key_value_list
         {
             BUILD(DECREF5($1, $3, $4, $5, $7),
                    $$, "document", "OiOOOOO", FILENAME, LINENO, $1, $3, $4, $5, $7);
         }


custom_value : STRING
             {
                 BUILD(DECREF1($1),
                        $$, "custom_value", "OO", $1, Py_None);
             }
             | DATE
             {
                 BUILD(DECREF1($1),
                        $$, "custom_value", "OO", $1, Py_None);
             }
             | BOOL
             {
                 BUILD(DECREF1($1),
                        $$, "custom_value", "OO", $1, Py_None);
             }
             | amount
             {
                 BUILD(DECREF1($1),
                        $$, "custom_value", "OO", $1, Py_None);
             }
             | number_expr
             {
                 BUILD(DECREF1($1),
                        $$, "custom_value", "OO", $1, Py_None);
             }
             | ACCOUNT
             {
                 /* Obtain beancount.core.account.TYPE */
                 PyObject* module = PyImport_ImportModule("beancount.core.account");
                 PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
                 Py_DECREF(module);
                 BUILD(DECREF2($1, dtype),
                        $$, "custom_value", "OO", $1, dtype);
             }

custom_value_list : empty
                  {
                      Py_INCREF(Py_None);
                      $$ = Py_None;
                  }
                  | custom_value_list custom_value
                  {
                      BUILD(DECREF2($1, $2),
                             $$, "handle_list", "OO", $1, $2);
                  }

custom : DATE CUSTOM STRING custom_value_list eol key_value_list
       {
           BUILD(DECREF4($1, $3, $4, $6),
                  $$, "custom", "OiOOOO", FILENAME, LINENO, $1, $3, $4, $6);
       }


entry : transaction
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
      {
          $$ = $1;
      }

option : OPTION STRING STRING eol
       {
           BUILD(DECREF2($2, $3),
                  $$, "option", "OiOO", FILENAME, LINENO, $2, $3);
       }

include : INCLUDE STRING eol
       {
           BUILD(DECREF1($2),
                  $$, "include", "OiO", FILENAME, LINENO, $2);
       }

plugin : PLUGIN STRING eol
       {
           BUILD(DECREF1($2),
                  $$, "plugin", "OiOO", FILENAME, LINENO, $2, Py_None);
       }
       | PLUGIN STRING STRING eol
       {
           BUILD(DECREF2($2, $3),
                  $$, "plugin", "OiOO", FILENAME, LINENO, $2, $3);
       }

directive : empty_line
          | pushtag
          | poptag
          | pushmeta
          | popmeta
          | option
          | include
          | plugin


declarations : declarations directive
             {
                 $$ = $1;
             }
             | declarations entry
             {
                 BUILD(DECREF2($1, $2),
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
                  * appearance of a LEX_ERROR token but makes the parser errors
                  * skip the next valid directive, so we just have to make sure
                  * repeated runs of this rule's handling code are idempotent.
                  */
                 $$ = $1;
             }
             | empty
             {
                  Py_INCREF(Py_None);
                  $$ = Py_None;
             }


file : declarations
     {
         /* If a Python exception has been raised and not handled, abort. In
          * case of unrecoverable error, the lexer raises a Python exception and
          * the yylex() function returns -1, whcih is translated by Bison into
          * an EOF token, handled here. */
         if (PyErr_Occurred()) {
             YYABORT;
         }
         BUILD(DECREF1($1),
                $$, "store_result", "O", $1);
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
        case TILDE     : return "TILDE";
        case HASH      : return "HASH";
        case PLUS      : return "PLUS";
        case MINUS     : return "MINUS";
        case ASTERISK  : return "ASTERISK";
        case SLASH     : return "SLASH";
        case COLON     : return "COLON";
        case LPAREN    : return "LPAREN";
        case RPAREN    : return "RPAREN";
        case FLAG      : return "FLAG";
        case TXN       : return "TXN";
        case BALANCE   : return "BALANCE";
        case OPEN      : return "OPEN";
        case CLOSE     : return "CLOSE";
        case PAD       : return "PAD";
        case EVENT     : return "EVENT";
        case QUERY     : return "QUERY";
        case CUSTOM    : return "CUSTOM";
        case PRICE     : return "PRICE";
        case NOTE      : return "NOTE";
        case DOCUMENT  : return "DOCUMENT";
        case PUSHTAG   : return "PUSHTAG";
        case POPTAG    : return "POPTAG";
        case PUSHMETA  : return "PUSHMETA";
        case POPMETA   : return "POPMETA";
        case OPTION    : return "OPTION";
        case PLUGIN    : return "PLUGIN";
        case DATE      : return "DATE";
        case ACCOUNT   : return "ACCOUNT";
        case CURRENCY  : return "CURRENCY";
        case STRING    : return "STRING";
        case NUMBER    : return "NUMBER";
        case TAG       : return "TAG";
        case LINK      : return "LINK";
        case KEY       : return "KEY";
        case BOOL      : return "BOOL";
        case NONE      : return "NULL";
    }
    return "<NO_STRING_TRANSLATION>";
}
