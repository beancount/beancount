/* -*- mode: c -*- */
/*
 * Parser grammar for beancount 2.0 input syntax.
 *
 * This assumes it feeds off the corresponding lexer in this pacakge. This is
 * meant to be used with the stock "go yacc" command.
 */

%defines
%error-verbose
%debug

%{

#include <stdio.h>

/* type Parser struct { */
/*  transactions *list.List */
/* } */

/* func MakeParser() *Parser { */
/*  return &Parser{ */
/*    transactions: list.New(), */
/*  } */
/* } */

/* type DatePair struct { */
/*  date time.Time */
/*  other_date time.Time */
/* } */



/* // FIXME: These will all go into another file */
/* type Transaction struct { */
/*  date time.Time */
/*  other_date time.Time */
/*  flag rune */
/*  description string */
/*  postings *list.List */
/* } */

/* type Posting struct { */
/*  account *Account */
/* } */

/* type Account struct { */
/*  name string */
/* } */

void yyerror(char const *s)
{
    fprintf(stderr, "%s\n", s);
}



%}

%union {
    const char* str;
    /* char rune */
    /* date time.Time */
    /* dates *DatePair */
    /* transaction *Transaction */
    /* posting *Posting */
    /* posting_list *list.List */
    /* account *Account */
}

%token <str> ERROR    /* error occurred; value is text of error */
%token <str> INDENT   /* Initial indent IF at the beginning of a line */
%token <str> EOL      /* End-of-line */
%token <str> COMMENT  /* A comment */
%token <str> SKIPPED  /* A line skipped because not a directive nor a comment */
%token <str> PIPE     /* | */
%token <str> ATAT     /* @@ */
%token <str> AT       /* @ */
%token <str> LCURL    /* { */
%token <str> RCURL    /* } */
%token <str> EQUAL    /* = */
%token <str> COMMA    /* , */
%token <str> SLASH    /* / */
%token <str> TXN      /* 'txn' keyword */
%token <str> TXNFLAG  /* Valid characters for flags */
%token <str> CHECK    /* 'check' keyword */
%token <str> OPEN     /* 'open' keyword */
%token <str> CLOSE    /* 'close' keyword */
%token <str> PAD      /* 'pad' keyword */
%token <str> EVENT    /* 'event' keyword */
%token <str> PRICE    /* 'price' keyword */
%token <str> LOCATION /* 'location' keyword */
%token <str> NOTE     /* 'note' keyword */
%token <str> BEGINTAG /* 'begintag' keyword */
%token <str> ENDTAG   /* 'endtag' keyword */
%token <str> DATE     /* A date object */
%token <str> ACCOUNT  /* The name of an account */
%token <str> CURRENCY /* A currency specification */
%token <str> STRING   /* A quoted string, with any characters inside */
%token <str> NUMBER   // A floating-point number

%code{

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


%type <char> txn
%type <sate> date
%type <sates> date_pair
%type <sransaction> transaction
%type <sosting> posting
%type <sosting_list> posting_list
%type <sccount> account
%type <str> begintag
%type <str> endtag

%start directives

%%
//--------------------------------------------------------------------------------

empty :

txn : TXN
    {
      /* $$ = rune(-1) */
    }
    | TXNFLAG
    {
      /* r, _ := utf8.DecodeRuneInString($1) */
      /* $$ = r */
    }

date : DATE
     {
         printf("%s", $1);
       /* d, _ := time.Parse(ISO8601, $1) */
       /* $$ = d */
     }

date_pair : date
          {
            /* $$ = &DatePair{$1, time.Time{}} */
          }
          | date EQUAL date
          {
            /* $$ = &DatePair{$1, $3} */
          }

eol : EOL
    | COMMENT EOL

transaction : date_pair txn STRING eol posting_list
            {
              /* $$ = &Transaction{$1.date, $1.other_date, $2, $3, $5} */
            }
            | date_pair txn STRING PIPE STRING eol posting_list
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


//--------------------------------------------------------------------------------
%%

/* func (l Lexer) Lex(lval *yySymType) int { */
/*   item := l.NextTok() */
/*   if item.Type == EOF { */
/*     return 0 */
/*   } */
/*   lval.str = item.val */
/*   return int(item.Type) */
/* } */

/* func (l Lexer) Error(e string) { */
/*   fmt.Printf("%s:%d: %v\n", l.name, l.lineNo, e) */
/* } */



/* // Global state of the parser. */
/* type ParserState struct { */
/*   tags *list.List */
/* } */

/* var parserState *ParserState */

/* func Parse(yylex yyLexer) int { */
/*   parserState = &ParserState{list.New()} */
/*   result := yyParse(yylex) */
/*   parserState = nil */
/*   return result */
/* } */
