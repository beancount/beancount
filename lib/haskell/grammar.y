/* Canonical BNF grammar for Ledger data files.

   Extensions are permitted if: they are not required, and they are
   backwards-compatible with this grammar.

   Version 2.6
 */

/* There are three special terminals in this grammar, which violate its
   context free nature:

   TEXT       -- consumes all characters until the next terminal
                 or EOL (end of line)
   WHITESPACE -- any amount of whitespace, not including EOL
   STRING     -- characters up to the next WHITESPACE or EOL

   BIGINT     -- a number of any width, matching [0-9]+
   INT4       -- a four digit wide number
   INT2       -- a two digit wide number
   INT1       -- a one digit wide number

   Except for 1) the 'spacer' production (see below), 2) EOL, and 3) the
   WHITESPACE required to begin a transaction, whitespace is otherwise
   ignored.

   Yes, this grammar is confusing and not so happy for machine readers, but it
   was designed for the human author and reader.  Once parsed, the contents
   must be unambiguous, which means they can be output to more rigorous
   formats for other programs to consume.
 */

/* JOURNALS ------------------------------------------------------------

   A journal is a file which primarily contains entries, among other elements.
 */

journal: journal_elements ;

journal_elements:
    EOL |
    ';' TEXT EOL |		/* these next four are all ignored */
    '*' TEXT EOL |
    'h' TEXT EOL |
    'b' TEXT EOL |
    directive_or_entry journal_elements |
    /* epsilon */               /* a journal may be empty */
    ;

directive_or_entry:
    entry |
    '@' word_directive EOL |
    '!' word_directive EOL |
    char_directive EOL
    ;

word_directive:
    "include" TEXT |
    "account" TEXT |
    "end" |
    "alias" STRING '=' TEXT |
    "def" TEXT
    ;

char_directive:
    'i' date time TEXT |	/* a timeclock.el "check in" */
    'I' date time TEXT |
    'o' date time TEXT |	/* a timeclock.el "check out" */
    'O' date time TEXT |
    'D' amount |		/* sets display parameters for a commodity */
    'A' TEXT |			/* sets the "default balancing account" */
    'C' commodity '=' amount |	/* specifies a commodity conversion */
    'P' date time commodity amount | /* a pricing history entry */
    'N' commodity |		/* commodity's price is never downloaded */
    'Y' INT4 |			/* sets the default year for date parsing */
    '-' '-' STRING TEXT |	/* specify command-line options in the file */
    ;

date: INT4 date_sep INT2 date_sep INT2 ;
date_opt: '=' date | /* epsilon */ ;
date_sep: '/' | '-' | '.' ;

time: INT2 ':' INT2 ':' INT2 ;

commodity: STRING ;

/* ENTRIES ------------------------------------------------------------
   
   Entries are the atomic units of accounting, which are composed of multiple
   transactions between accounts, so long as it all balances in the end.
 */

entry: plain_entry |
       periodic_entry |
       automated_entry ;

plain_entry:
    date date_opt status_opt code_opt FULLSTRING note_opt EOL
    transaction transactions ;

status_opt: status | /* epsilon */ ;
status: '*' | '!' | /* epsilon */ ;

code_opt: code | /* epsilon */ ;
code: '(' TEXT ')' ;

spacer: ' ' ' ' | '\t' | ' ' '\t' ;

note_opt: spacer note | /* epsilon */ ;
note: ';' TEXT ;

/* ---------------------------------------------------------------------- */

periodic_entry:
    '~' period_expr note_opt EOL
    transaction transactions ;

/* A period expression has its own sub-grammar, which I don't quite have the
   time to exhaustively describe now.  See datetime.cc.  It allows for lots
   and lots of things, and is probably horribly ambiguous. */

period_expr: FULLSTRING ;

/* ---------------------------------------------------------------------- */

automated_entry:
    '=' value_expr note_opt EOL
    transaction transactions ;

/* Value expressions are a algebraic math expressions very similar to XPath
   (minus the path traversal elements).  This grammar needs fleshing out also,
   since it's allowed in many places. */

value_expr: FULLSTRING ;

/* There is a serious ambiguity here which the parser resolves as follows: if
   an amount_expr can be parsed as an amount, it's an amount; otherwise, it's
   a value expression. */

quantity: neg_opt BIGINT decimal_opt ;

neg_opt: '-' | /* epsilon */ ;
decimal_opt: '.' BIGINT | /* epsilon */ ;

annotation: lot_price_opt lot_date_opt lot_note_opt ;

lot_date_opt: date | /* epsilon */ ;
lot_date: '[' date ']' ;

lot_price_opt: price | /* epsilon */ ;
lot_price: '{' amount '}' ;

lot_note_opt: note | /* epsilon */ ;
lot_note: '(' string ')' ;

amount:
    neg_opt commodity quantity annotation |
    quantity commodity annotation ;

amount_expr: amount | value_expr ;

/* TRANSACTIONS ------------------------------------------------------------

   Transactions are the fundamental unit of accounting, and represent the
   movement of commodities to or from an account.  Thus, paying off your
   credit card consists of two balancing transactions: one that withdraws
   money from your checking account, and another which pays money to your
   credit institution.
 */

transactions:
    transaction transactions |
    /* epsilon */
    ;

transaction:
    WHITESPACE status_opt account values_opt note_opt EOL;

account_name: FULLSTRING ;

values_opt:
    spacer amount_expr price_opt |
    /* epsilon */
    ;

price_opt: price | /* epsilon */ ;
price:
    '@' amount_expr |
    '@@' amount_expr		/* in this case, it's the whole price */
    ;

account:
    account_name |
    '(' account_name ')' |
    '[' account_name ']' ;

/* grammar ends here */
