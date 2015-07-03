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


/* Build and accumulate an error on the builder object. */
void build_lexer_error(YYSTYPE* yylval, const char* string, size_t length);

/* Build and accumulate an error on the builder object using the current
 * exception state. */
void build_lexer_error_from_exception(YYSTYPE* yylval);



/* Callback call site with error handling. */
#define BUILD_LEX(method_name, format, ...)                             \
    yylval->pyobj = PyObject_CallMethod(builder, method_name,           \
                                        format, __VA_ARGS__);           \
    /* Process exception state {3cfb2739349a} */                        \
    if (yylval->pyobj == NULL) {                                        \
       TRACE_ERROR("BUILD_LEX(%s) raised exception", method_name);      \
       build_lexer_error_from_exception(yylval);                        \
       return LEX_ERROR;                                                \
    }

/* FIXME: These methods shoudl never return None... check for this and raise an error if they ever do. */
    /* else if (yylval->pyobj == Py_None) {                             \ */
    /*    TRACE_ERROR("BUILD_LEX(%s) returned None", method_name); \ */
    /*    create_error_from_exception(); */
    /*    return LEX_ERROR;                                        \ */
    /* } */


/* Global declarations; defined below. */
extern int yy_eof_times;
extern const char* yy_filename;
extern int yycolumn;



/* Handle detecting the beginning of line. */
extern int yy_line_tokens; /* Number of tokens since the bol. */

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


/* Longest possible string, including multiline strings. */
#define MAX_STR_BUF  16384

#define SAFE_COPY(dst_ptr, value)               \
	*dst_ptr++ = value;                     \
	if (dst_ptr >= strbuf_end) {            \
            BEGIN(INVALID);                     \
	}
/* FIXME: You have to make this code gobble the rest of the string and not fail. */

}

/* A start condition for chomping an invalid token. */
%x INVALID

/* Exclusive start condition for parsing escape sequences in string literals. */
%x STRLIT


/*--------------------------------------------------------------------------------------*/
/* Rules */
%%

	/* String literal buffer. */
	char strbuf[MAX_STR_BUF];
	char* strbuf_end = strbuf + MAX_STR_BUF;
	char* strbuf_ptr = NULL;


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
\~		{ return TILDE; }
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
[*!&#?%PSTCUR]	{
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
commodity	{ return COMMODITY; }
pad		{ return PAD; }
event		{ return EVENT; }
price		{ return PRICE; }
note		{ return NOTE; }
document	{ return DOCUMENT; }
pushtag	        { return PUSHTAG; }
poptag		{ return POPTAG; }
option		{ return OPTION; }
plugin		{ return PLUGIN; }
include		{ return INCLUDE; }

 /* Account names. */
TRUE		{
    yylval->pyobj = Py_True;
    Py_INCREF(Py_True);
    return BOOL;
}

FALSE		{
    yylval->pyobj = Py_False;
    Py_INCREF(Py_False);
    return BOOL;
}

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
    BUILD_LEX("DATE", "iii", year, month, day);

    return DATE;
}

 /* Account names. */
([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]+)+		{
    BUILD_LEX("ACCOUNT", "s", yytext);
    return ACCOUNT;
}

 /* Currencies. These are defined as uppercase only in order to disambiguate the
  * syntax. This is kept in sync with beancount.core.amount.CURRENCY_RE. */
[A-Z][A-Z0-9\'\.\_\-]{0,22}[A-Z0-9]	{
    BUILD_LEX("CURRENCY", "s", yytext);
    return CURRENCY;
}

 /* String literals. */
 /* Note that we use an exclusive start condition.
    See section "Start Conditions" in the GNU Flex manual. */
\"	{
    strbuf_ptr = strbuf;
    BEGIN(STRLIT);
}

<STRLIT>{

    /* Saw closing quote - all done. */
    \"        {
        BEGIN(INITIAL);
        *strbuf_ptr = '\0';
        BUILD_LEX("STRING", "s#", strbuf, strbuf_ptr - strbuf);
        return STRING;
    }

    /* Escape sequences. */
    \\n       SAFE_COPY(strbuf_ptr, '\n');
    \\t       SAFE_COPY(strbuf_ptr, '\t');
    \\r       SAFE_COPY(strbuf_ptr, '\r');
    \\b       SAFE_COPY(strbuf_ptr, '\b');
    \\f       SAFE_COPY(strbuf_ptr, '\f');
    \\(.|\n)  SAFE_COPY(strbuf_ptr, yytext[1]);

    /* All other characters. */
    [^\\\"]+        {
        char *yptr = yytext;
        while ( *yptr ) {
            SAFE_COPY(strbuf_ptr, *yptr++);
        }
    }
}

 /* Numbers */
[-+]?([0-9]+|[0-9][0-9,]+[0-9])(\.[0-9]*)? 		{
    BUILD_LEX("NUMBER", "s", yytext);
    return NUMBER;
}

 /* Tags */
#[A-Za-z0-9\-_/.]+ 		{
    BUILD_LEX("TAG", "s", &(yytext[1]));
    return TAG;
}

 /* Links */
\^[A-Za-z0-9\-_/.]+ 		{
    BUILD_LEX("LINK", "s", &(yytext[1]));
    return LINK;
}

 /* Key */
[a-z][a-z0-9\-_]+: 		{
    BUILD_LEX("KEY", "s#", yytext, yyleng-1);
    return KEY;
}

 /* Default rule. {bf253a29a820} */
.			{
    unput(*yytext);
    TRACE_ERROR("Default rule '%s'.\n", yytext);
    BEGIN(INVALID);
}

 /* Fake an EOL at the end of file, to ensure that files without a final newline
  * will process postings right. */
<<EOF>>     		{
  if ( yy_eof_times == 0 ) {
    yy_eof_times = 1;
    yylloc->first_line = yylineno;
    return EOL;
  }
  return 0;
 }

 /* Note: We use a subparser here because if we set a default rule to chomp this
    pattern, it would take precedence over valid rules if the matched text is
    longer and thus would break the lexer. Writing our own lexer would fix
    this and more. {bba169a1d35a} */
<INVALID>[^ \t\n\r]+     {
    /* TRACE_ERROR("Processing INVALID sublexer at '%s'\n", yytext); */

    char buffer[256];
    size_t length = snprintf(buffer, 256, "Invalid token: '%s'", yytext);
    build_lexer_error(yylval, buffer, length);

    BEGIN(INITIAL);

    return LEX_ERROR;
 }


%%
/*--------------------------------------------------------------------------------------*/
/* User Code */


/* Note: All these globals should be moved to an yylex_extra */

/* The number of times EOF has been hit. This is used to synthesize an EOL at
 * the end of the file. */
int yy_eof_times = 0;

/* The filename being tokenized. */
const char* yy_filename = 0;

/* Number of tokens since the beginning of the line. */
int yy_line_tokens = 0;

/* The current column we're tokenizing at. */
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

/* Build and accumulate an error on the builder object. */
void build_lexer_error(YYSTYPE* yylval, const char* string, size_t length)
{
    /* Build and accumulate a new error object. {27d1d459c5cd} */
    PyObject* rv = PyObject_CallMethod(builder, "build_lexer_error",
                                       "s#", string, length);
    if (rv == NULL) {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: Building exception from default rule");
    }
}

void build_lexer_error_from_exception(YYSTYPE* yylval)
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
        PyObject* rv = PyObject_CallMethod(builder, "build_lexer_error",
                                           "OO", pvalue, ptype);
        Py_DECREF(ptype);
        Py_DECREF(pvalue);
        Py_DECREF(ptraceback);

        if (rv == NULL) {
            PyErr_SetString(PyExc_RuntimeError,
                            "Internal error: Building exception");
        }
    }
    else {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: No exception");
    }
}
