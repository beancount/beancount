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
%option outfile="beancount_lexer.c"
%option header-file="beancount_lexer.h"
/* %option reentrant */
/* %option debug */
/* %option stack */
/* %option 8bit */


/* Top Code. */
%top{

/* Includes. */
#include <math.h>
#include <stdlib.h>

#include "builder.h"
#include "beancount_parser.h"


/* Handle the current column. */
extern int yy_line_begin;
extern int yy_pos;

#define YY_USER_ACTION        { yy_pos++; }

int yy_column(void);


/* Skip the rest of the input line. */
void yy_skip_line(void);


/* Error handler. */
extern void yyerror(char const *s);


}

/*------------------------------------------------------------------------------------------------------------------------*/
/* Rules */
%%


 /* Newlines are output as explicit tokens, because lines matter in the syntax. */
\n		{
    yy_line_begin = yy_pos + 1;
    return EOL;
}

 /* Ignore whitespace, except when found at the beginning of a line. This is how we group indented postings. */
[ \t\r]+	{
    if ( yy_column() == 0 ) {
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
    if ( yy_column() != 0 ) {
        /* printf("yylval '%d'\n", (void*)yylval); */
        /* yylval->character = yytext[0]; */
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
location	{ return LOCATION; }
note		{ return NOTE; }
begintag	{ return BEGINTAG; }
endtag		{ return ENDTAG; }

 /* Dates. */
[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]	{
    /* yylval->pyobj = parseDate(yytext); */
    yylval->pyobj = build(builder, "date", yytext);
    return DATE;
}

 /* Account names. */
(([A-Z][A-Za-z0-9\-]+):)+([A-Z][A-Za-z0-9\-]+)		{
    /* yylval->pyobj = parseString(yytext); */
    return ACCOUNT;
}

 /* Currencies. These are defined as uppercase only in order to disambiguate the syntax. */
[A-Z][A-Z0-9\'\.]{1,10}		{
    /* yylval->pyobj = parseString(yytext); */
    return CURRENCY;
}

 /* String literals. */
\"[^\"]*\" 			{
    /* yylval->pyobj = parseString(&yytext[1], strlen(yytext)-2); */
    return STRING;
}

 /* Numbers */
[-+]?([0-9\.])+ 		{
    /* yylval->pyobj = parseDecimal(yytest); */
    return NUMBER;
}

 /* Default rule. This should never be reached. */
.	{ yyerror("Syntax error in tokenizer."); }



%%
/*------------------------------------------------------------------------------------------------------------------------*/
/* User Code */

int yy_line_begin = 0;
int yy_pos = 0;

int yy_column()
{
    return yy_pos - yy_line_begin;
}

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
