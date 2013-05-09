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
/* %option outfile="scanner.c" header-file="scanner.h" */

%{
#include <math.h>

enum Tokens {
    TOKEN_ERROR = 1,
    TOKEN_INDENT,
    TOKEN_EOL,
    TOKEN_EOF,
    TOKEN_COMMENT,
    TOKEN_SKIPPED,
    TOKEN_PIPE,
    TOKEN_ATAT,
    TOKEN_AT,
    TOKEN_LCURL,
    TOKEN_RCURL,
    TOKEN_EQUAL,
    TOKEN_COMMA,
    TOKEN_SLASH,
    TOKEN_TXNFLAG,
    TOKEN_TXN,
    TOKEN_CHECK,
    TOKEN_OPEN,
    TOKEN_CLOSE,
    TOKEN_PAD,
    TOKEN_EVENT,
    TOKEN_PRICE,
    TOKEN_LOCATION,
    TOKEN_NOTE,
    TOKEN_BEGINTAG,
    TOKEN_ENDTAG,
    TOKEN_DATE,
    TOKEN_ACCOUNT,
    TOKEN_CURRENCY,
    TOKEN_STRING,
    TOKEN_NUMBER
};

const char* getTokenName(int token)
{
    switch ( token ) {
        case TOKEN_ERROR    : return "ERROR";
        case TOKEN_INDENT   : return "INDENT";
        case TOKEN_EOL      : return "EOL";
        case TOKEN_EOF      : return "EOF";
        case TOKEN_COMMENT  : return "COMMENT";
        case TOKEN_SKIPPED  : return "SKIPPED";
        case TOKEN_PIPE     : return "PIPE";
        case TOKEN_ATAT     : return "ATAT";
        case TOKEN_AT       : return "AT";
        case TOKEN_LCURL    : return "LCURL";
        case TOKEN_RCURL    : return "RCURL";
        case TOKEN_EQUAL    : return "EQUAL";
        case TOKEN_COMMA    : return "COMMA";
        case TOKEN_SLASH    : return "SLASH";
        case TOKEN_TXNFLAG  : return "TXNFLAG";
        case TOKEN_TXN      : return "TXN";
        case TOKEN_CHECK    : return "CHECK";
        case TOKEN_OPEN     : return "OPEN";
        case TOKEN_CLOSE    : return "CLOSE";
        case TOKEN_PAD      : return "PAD";
        case TOKEN_EVENT    : return "EVENT";
        case TOKEN_PRICE    : return "PRICE";
        case TOKEN_LOCATION : return "LOCATION";
        case TOKEN_NOTE     : return "NOTE";
        case TOKEN_BEGINTAG : return "BEGINTAG";
        case TOKEN_ENDTAG   : return "ENDTAG";
        case TOKEN_DATE     : return "DATE";
        case TOKEN_ACCOUNT  : return "ACCOUNT";
        case TOKEN_CURRENCY : return "CURRENCY";
        case TOKEN_STRING   : return "STRING";
        case TOKEN_NUMBER   : return "NUMBER";
    }
    return 0;
}



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


/*
DIGIT    [0-9]
ID       [a-z][a-z0-9]*
*/

%%

\n		{
    yy_line_begin = yy_pos + 1;
    return TOKEN_EOL;
}

[ \t\r]+	{

    /* Whitespace counts as an 'INDENT' token, but only if it is found at the beginning of a line. */
    if ( yy_column() == 0 ) {
        return TOKEN_INDENT;
    }
}


\|		{ return TOKEN_PIPE; }
@@		{ return TOKEN_ATAT; }
@		{ return TOKEN_AT; }
\{		{ return TOKEN_LCURL; }
\}		{ return TOKEN_RCURL; }
=		{ return TOKEN_EQUAL; }
,		{ return TOKEN_COMMA; }
\/		{ return TOKEN_SLASH; }


;+		{
    /* This is a comment; skip the rest of the line. */
    skip_rest_of_line();
    return TOKEN_COMMENT;
}


[*!&#?%]	{
    if ( yy_column() != 0 ) {
        return TOKEN_TXNFLAG;
    }
    else {
        /* If not at the beginning of a line, skip the rest of the line (to support org-mode). */
        skip_rest_of_line();
        return TOKEN_SKIPPED;
    }
}

txn		{ return TOKEN_TXN; }
check		{ return TOKEN_CHECK; }
open		{ return TOKEN_OPEN; }
close		{ return TOKEN_CLOSE; }
pad		{ return TOKEN_PAD; }
event		{ return TOKEN_EVENT; }
price		{ return TOKEN_PRICE; }
location	{ return TOKEN_LOCATION; }
note		{ return TOKEN_NOTE; }
begintag	{ return TOKEN_BEGINTAG; }
endtag		{ return TOKEN_ENDTAG; }

[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]	{ return TOKEN_DATE; }

(([A-Z][A-Za-z0-9\-]+):)+([A-Z][A-Za-z0-9\-]+)		{ return TOKEN_ACCOUNT; }

[A-Z][A-Z0-9\'\.]{1,10} 				{ return TOKEN_CURRENCY; }

\"[^\"]*\"							{ return TOKEN_STRING; }

[-+]?([0-9\.])+						{ return TOKEN_NUMBER; }

 /* .		{ printf("location = %d:%d\n", yylineno, yy_column()); return 1000; } */

%%

void skip_rest_of_line()
{
    int c;
    while ( (c = yyinput()) != 0 ) {
        if ( c == '\n' ) {
            break;
        }
    }
    unput(c);
}

int main(int argc, char** argv)
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
        yyin = fopen(argv[0], "r");
    else
        yyin = stdin;

    while ( 1 )  {
        int token = yylex();
        printf("%s = %d\n", getTokenName(token), token);
        if ( token == YY_NULL ) {
            break;
        }
    }
}
