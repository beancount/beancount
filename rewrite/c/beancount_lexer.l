/* -*- mode: c -*- */
/* A flex lexer for Beancount. */

%option noyywrap
%option nodefault
%option yylineno
%option never-interactive
%option warn
/* %option reentrant */
/* %option debug */
/* %option stack */
/* %option bison-bridge */
/* %option 8bit */
%option outfile="beancount_lexer.c"
%option header-file="beancount_lexer.h"

%{
#include <math.h>
#include "beancount_parser.h"

/* int last_newline = 0; */
/* int pos = 0; */
/* int column = 0; */

/* Handle the current column. */
int yy_line_begin = 0;
int yy_pos = 0;

int yy_column()
{
    return yy_pos - yy_line_begin;
}

#define YY_USER_ACTION        yy_pos++;


/* Skip the rest of the input line. */
void skip_rest_of_line();

%}

/*------------------------------------------------------------------------------------------------------------------------*/
%%

\n		{
    yy_line_begin = yy_pos + 1;
    return EOL;
}

[ \t\r]+	{

    /* Whitespace counts as an 'INDENT' token, but only if it is found at the beginning of a line. */
    if ( yy_column() == 0 ) {
        return INDENT;
    }
}


\|		{ return PIPE; }
@@		{ return ATAT; }
@		{ return AT; }
\{		{ return LCURL; }
\}		{ return RCURL; }
=		{ return EQUAL; }
,		{ return COMMA; }
\/		{ return SLASH; }


;+		{
    /* This is a comment; skip the rest of the line. */
    skip_rest_of_line();
    return COMMENT;
}


[*!&#?%]	{
    if ( yy_column() != 0 ) {
        return TXNFLAG;
    }
    else {
        /* If not at the beginning of a line, skip the rest of the line (to support org-mode). */
        skip_rest_of_line();
        return SKIPPED;
    }
}

txn		{ return TXN; }
check		{ return CHECK; }
open		{ return OPEN; }
close		{ return CLOSE; }
pad		{ return PAD; }
event		{ return EVENT; }
price		{ return PRICE; }
location	{ return LOCATION; }
note		{ return NOTE; }
begintag	{ return BEGINTAG; }
endtag		{ return ENDTAG; }

[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]	{ return DATE; }

(([A-Z][A-Za-z0-9\-]+):)+([A-Z][A-Za-z0-9\-]+)		{ return ACCOUNT; }

[A-Z][A-Z0-9\'\.]{1,10} 				{ return CURRENCY; }

\"[^\"]*\"							{ return STRING; }

[-+]?([0-9\.])+						{ return NUMBER; }

 /* .		{ printf("location = %d:%d\n", yylineno, yy_column()); return 1000; } */

%%
/*------------------------------------------------------------------------------------------------------------------------*/

void skip_rest_of_line()
{
    int c;
    while ( (c = input()) != 0 ) {
        if ( c == '\n' ) {
            break;
        }
    }
    unput(c);
}
