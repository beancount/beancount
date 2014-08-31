/* -*- mode: c -*- */
/* A flex lexer for Beancount. */

/*--------------------------------------------------------------------------------------*/
/* Definitions */

/* Options */
%option noyywrap
/* %option nodefault */
%option yylineno
%option never-interactive
%option warn
%option bison-bridge
%option bison-locations
/* %option reentrant */
/* %option debug */
/* %option stack */
/* %option 8bit */


/* Top Code. */
%top{

/* Includes. */
#include <math.h>
#include <stdlib.h>

#include "parser.h"
#include "grammar.h"


/* The filename being parsed. */
extern const char* yy_filename;
extern int yycolumn;



/* Handle detecting the beginning of line. */
extern int yy_line_tokens; /* Number of tokens since the bol */

#define YY_USER_ACTION  {                               \
    yy_line_tokens++;                                   \
    yylloc->first_line = yylloc->last_line = yylineno;  \
    yylloc->first_column = yycolumn;                    \
    yylloc->last_column = yycolumn+yyleng-1;            \
    yycolumn += yyleng;                                 \
  }


/* Skip the rest of the input line. */
int yy_skip_line(void);


/* Utility functions. */
int strtonl(const char* buf, size_t nchars);

}

/* A start condition for chomping an invalid token. */
%x INVALID


/*--------------------------------------------------------------------------------------*/
/* Rules */
%%


 /* Newlines are output as explicit tokens, because lines matter in the syntax. */
\n		{
    yy_line_tokens = 0;
    yycolumn = 1;
    return EOL;
}

 /* Ignore whitespace, except when found at the beginning of a line and followed
    by a non-comment regular character. This is how we group indented
    postings. */
[ \t\r]+	{
    if ( yy_line_tokens == 1 ) {
        /* If the next character completes the line, skip it. */
        if ( yy_hold_char == '\n' ||
             yy_hold_char == '\r' ||
             yy_hold_char == '\0' ||
             yy_hold_char == ';' ) {
            return SKIPPED;
        }
        else {
            return INDENT;
        }
    }
}

 /* Characters with special meanings have their own tokens. */
\|		{ return PIPE; }
@@		{ return ATAT; }
@		{ return AT; }
\{\{		{ return LCURLCURL; }
\}\}		{ return RCURLCURL; }
\{		{ return LCURL; }
\}		{ return RCURL; }
,		{ return COMMA; }
\/		{ return SLASH; }

 /* Skip commented output (but not the accompanying newline). */
;+.*		{
    /* yy_skip_line(); */
    return COMMENT;
}

 /* Special characters which may be used in-lieu of a 'txn' keyword in a
    transaction declaration output as a token. Other lines are just skipped.
    This allows us to use org-mode or other separators in the input syntax.

    Note: You need to make sure to include all the values from
    beancount.core.flags, in order for round-trips between text and entries to
    be possible. {5307d8fa1e7b}
    */
[*!&#?%PSTCU]	{
    if ( yy_line_tokens != 1 ) {
        yylval->character = yytext[0];
        return FLAG;
    }
    else {
        yy_skip_line();
        return SKIPPED;
    }
}

 /* Keywords. */
txn		{ return TXN; }
balance		{ return BALANCE; }
open		{ return OPEN; }
close		{ return CLOSE; }
pad		{ return PAD; }
event		{ return EVENT; }
price		{ return PRICE; }
note		{ return NOTE; }
document	{ return DOCUMENT; }
pushtag	        { return PUSHTAG; }
poptag		{ return POPTAG; }
option		{ return OPTION; }

 /* Dates. */
[0-9]{4,}[\-/][0-9]+[\-/][0-9]+	{
    const char* year_str;
    const char* month_str;
    const char* day_str;
    int year, month, day;

    /* Parse the numbers. */
    year_str = yytext;
    month_str = strpbrk(year_str, "-/");
    year = strtonl(year_str, month_str - year_str);
    month_str++;
    day_str = strpbrk(month_str, "-/");
    month = strtonl(month_str, day_str - month_str);
    day_str++;
    day = strtonl(day_str, yytext + yyleng - day_str);

    /* Attempt to create the date. */
    yylval->pyobj = BUILD("DATE", "iii", year, month, day);

    /* Return error if we could not. An error has already been logged. */
    if (yylval->pyobj == Py_None) {
      return ERROR;
    }
    return DATE;
}

 /* Account names. */
([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]+)+		{
    yylval->pyobj = BUILD("ACCOUNT", "s", yytext);
    return ACCOUNT;
}

 /* Currencies. These are defined as uppercase only in order to disambiguate the
  * syntax. */
[A-Z][A-Z0-9\'\.\_\-]{0,10}[A-Z0-9]	{
    yylval->pyobj = BUILD("CURRENCY", "s", yytext);
    return CURRENCY;
}

 /* String literals. */
\"[^\"]*\" 			{
    yylval->pyobj = BUILD("STRING", "s#", yytext+1, yyleng-2);
    return STRING;
}

 /* Numbers */
[-+]?([0-9\.])+ 		{
    yylval->pyobj = BUILD("NUMBER", "s", yytext);
    return NUMBER;
}

 /* Tags */
#[A-Za-z0-9\-_/.]+ 		{
    yylval->pyobj = BUILD("TAG", "s", &(yytext[1]));
    return TAG;
}

 /* Links */
\^[A-Za-z0-9\-_/.]+ 		{
    yylval->pyobj = BUILD("LINK", "s", &(yytext[1]));
    return LINK;
}

 /* Default rule. */
.			{
  unput(*yytext);
  BEGIN(INVALID);
  return ERROR;
}

 /* Note: we use a subparser because if we set a default rule to
    chomp this pattern, it would take precedence over valid rules
    if the matched text is longer and thus break the parser. */
<INVALID>[^ \t\n\r]+     {
    yylval->pyobj = BUILD("ERROR", "s#", yytext, yyleng);
    BEGIN(INITIAL);
}


%%
/*--------------------------------------------------------------------------------------*/
/* User Code */


const char* yy_filename = 0;
int yy_line_tokens = 0;
int yycolumn = 1;

#define LEXEOF 0

int yy_skip_line()
{
    int num_chars = 0;
    for ( ;; ) {
        int c = input();
        num_chars++;
        if ( c == LEXEOF || c == -1 ) {
            break;
        }
        if ( c == '\n' ) {
            unput(c);
            num_chars--;
            break;
        }
    }
    return num_chars;
}

/* Convert an integer string to a number. */
int strtonl(const char* buf, size_t nchars)
{
    int result = 0;
    int i;
    for ( i = 0; i < nchars; ++i ) {
        result *= 10;
        result += (buf[i] - '0');
    }
    return result;
}
