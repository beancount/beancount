/* -*- mode: c -*- */
/* A flex lexer for Beancount. */

/*------------------------------------------------------------------------------------------------------------------------*/
/* Definitions */

/* Options */
%option noyywrap
%option nodefault
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


/* Handle detecting the beginning of line. */
extern int yy_line_tokens; /* Number of tokens since the bol */

#define YY_USER_ACTION  {                               \
    yy_line_tokens++;                                   \
    yylloc->first_line = yylloc->last_line = yylineno;  \
  }


/* Skip the rest of the input line. */
void yy_skip_line(void);


/* Error handlers. */
extern void yyerror(char const *s);
extern void report_error(char const *s, YYLTYPE* yylloc);


/* Utility functions. */
int strtonl(const char* buf, size_t nchars);

}

/*------------------------------------------------------------------------------------------------------------------------*/
/* Rules */
%%


 /* Newlines are output as explicit tokens, because lines matter in the syntax. */
\n		{
    yy_line_tokens = 0;
    return EOL;
}

 /* Ignore whitespace, except when found at the beginning of a line. This is how we group indented postings. */
[ \t\r]+	{
    if ( yy_line_tokens == 1 ) {
        return INDENT;
    }
}

 /* Characters with special meanings have their own tokens. */
\|		{ return PIPE; }
@@		{ return ATAT; }
@		{ return AT; }
\{		{ return LCURL; }
\}		{ return RCURL; }
=		{ return EQUAL; }
,		{ return COMMA; }
\/		{ return SLASH; }

 /* Skip commented output (but not the accompanying newline). */
;+		{
    yy_skip_line();
    return COMMENT;
}

 /* Special characters which may be used in-lieu of a 'txn' keyword in a transaction declaration output as a token.
    Other lines are just skipped. This allows us to use org-mode or other separators in the input syntax. */
[*!&#?%]	{
    if ( yy_line_tokens != 1 ) {
        yylval->character = yytext[0];
        return TXNFLAG;
    }
    else {
        yy_skip_line();
        return SKIPPED;
    }
}

 /* Keywords. */
txn		{ return TXN; }
check		{ return CHECK; }
open		{ return OPEN; }
close		{ return CLOSE; }
pad		{ return PAD; }
event		{ return EVENT; }
price		{ return PRICE; }
note		{ return NOTE; }
begintag	{ return BEGINTAG; }
endtag		{ return ENDTAG; }

 /* Dates. */
[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]	{
    int year = strtonl(yytext, 4);
    int month = strtonl(yytext+5, 2);
    int day = strtonl(yytext+8, 2);
    yylval->pyobj = BUILD("DATE", "iii", year, month, day);
    return DATE;
}

 /* Account names. */
(Assets|Liabilities|Equity|Income|Expenses)(:[A-Z][A-Za-z0-9\-]+)+		{
    yylval->pyobj = BUILD("ACCOUNT", "s", yytext);
    return ACCOUNT;
}

 /* Currencies. These are defined as uppercase only in order to disambiguate the syntax. */
[A-Z][A-Z0-9\'\.]{1,10}		{
    yylval->pyobj = BUILD("CURRENCY", "s", yytext);
    return CURRENCY;
}

 /* String literals. */
\"[^\"]*\" 			{
    yytext[strlen(yytext)-1] = '\0';
    yylval->pyobj = BUILD("STRING", "s", &(yytext[1]));
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

 /* Default rule. This should never be reached. */
.	{ report_error("Syntax error.", yylloc); }



%%
/*------------------------------------------------------------------------------------------------------------------------*/
/* User Code */

int yy_line_tokens = 0;

void yy_skip_line()
{
    int c;
    while ( (c = input()) != 0 ) {
        if ( c == '\n' ) {
            break;
        }
    }
    unput(c);
}

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
