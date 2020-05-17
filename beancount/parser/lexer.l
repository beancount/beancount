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
void build_lexer_error(const char* string, size_t length);

/* Build and accumulate an error on the builder object using the current
 * exception state. */
void build_lexer_error_from_exception(void);



/* Callback call site with error handling. */
#define BUILD_LEX(method_name, format, ...)                                             \
    yylval->pyobj = PyObject_CallMethod(builder, method_name, format, __VA_ARGS__);     \
    /* Handle a Python exception raised by the handler {3cfb2739349a} */                \
    if (yylval->pyobj == NULL) {                                                        \
       build_lexer_error_from_exception();                                              \
       return LEX_ERROR;                                                                \
    }                                                                                   \
    /* Lexer builder methods should never return None, check for it. */                 \
    else if (yylval->pyobj == Py_None) {                                                \
        Py_DECREF(Py_None);                                                             \
        build_lexer_error("Unexpected None result from lexer", 34);                     \
        return LEX_ERROR;                                                               \
    }


/* Initialization/finalization methods. These are separate from the yylex_init()
 * and yylex_destroy() and they call them. */
void yylex_initialize(const char* filename, const char* encoding);
void yylex_finalize(void);


/* Global declarations; defined below. */
extern int yy_eof_times;
extern const char* yy_filename;
extern int yycolumn;
extern const char* yy_encoding;

/* String buffer statics. */
extern size_t strbuf_size; /* Current buffer size (not including final nul). */
extern char* strbuf;       /* Current buffer head. */
extern char* strbuf_end;   /* Current buffer sentinel (points to the final nul). */
extern char* strbuf_ptr;   /* Current insertion point in buffer. */
void strbuf_realloc(size_t num_new_chars);



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


/* Append characters to the static string buffer and verify. */
#define SAFE_COPY_CHAR(value)                    \
	if (strbuf_ptr >= strbuf_end) {         \
            strbuf_realloc(1);                  \
	}                                       \
        *strbuf_ptr++ = value;

}

/* A start condition for chomping an invalid token. */
%x INVALID

/* Exclusive start condition for parsing escape sequences in string literals. */
%x STRLIT


ASCII           [\x00-\x7f]
UTF-8-1         [\x80-\xbf]
UTF-8-2         [\xc2-\xdf]{UTF-8-1}
UTF-8-3         \xe0[\xa0-\xbf]{UTF-8-1}|[\xe1-\xec]{UTF-8-1}{UTF-8-1}|\xed[\x80-\x9f]{UTF-8-1}|[\xee-\xef]{UTF-8-1}{UTF-8-1}
UTF-8-4         \xf0[\x90-\xbf]{UTF-8-1}{UTF-8-1}|[\xf1-\xf3]{UTF-8-1}{UTF-8-1}{UTF-8-1}|\xf4[\x80-\x8f]{UTF-8-1}{UTF-8-1}
UTF-8-ONLY      {UTF-8-2}|{UTF-8-3}|{UTF-8-4}
UTF-8           {ASCII}|{UTF-8-ONLY}

ACCOUNTTYPE     ([A-Z]|{UTF-8-ONLY})([A-Za-z0-9\-]|{UTF-8-ONLY})*
ACCOUNTNAME     ([A-Z0-9]|{UTF-8-ONLY})([A-Za-z0-9\-]|{UTF-8-ONLY})*


/*--------------------------------------------------------------------------------------*/
/* Rules */
%%


 /* Newlines are output as explicit tokens, because lines matter in the syntax. */
\n		{
    yy_line_tokens = 0;
    yycolumn = 1;
    return EOL;
}

 /* Ignore whitespace, except when found at the beginning of a line
    and followed by a regular character. This is how we detect an
    initial indent and thus group postings and comments together in
    the grammar. */
[ \t\r]+	{
    if ( yy_line_tokens == 1 ) {
        /* If the next character completes the line, skip it. */
        if ( yy_hold_char == '\n' ||
             yy_hold_char == '\r' ||
             yy_hold_char == '\0' ) {
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
\+		{ return PLUS; }
\-		{ return MINUS; }
\/		{ return SLASH; }
\(		{ return LPAREN; }
\)		{ return RPAREN; }

 /* Special handling for characters beginning a line to be ignored.
  * I'd like to improve how this is handled. Needs own lexer, really. */
\#		{
    if ( yy_line_tokens != 1 ) {
        return HASH;
    }
    else {
        /* Allow org-mode titles. */
        yy_skip_line();
        return SKIPPED;
    }
}

\*		{
    if ( yy_line_tokens != 1 ) {
        return ASTERISK;
    }
    else {
        /* Allow org-mode titles. */
        yy_skip_line();
        return SKIPPED;
    }
}

\:		{
  if (yy_line_tokens != 1) {
    return COLON;
  }
  else {
    /* Allow org-mode drawers. */
    yy_skip_line();
    return SKIPPED;
  }
}

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
[!&#?%PSTCURM]	{
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
query		{ return QUERY; }
custom		{ return CUSTOM; }
price		{ return PRICE; }
note		{ return NOTE; }
document	{ return DOCUMENT; }
pushtag	        { return PUSHTAG; }
poptag		{ return POPTAG; }
pushmeta	{ return PUSHMETA; }
popmeta		{ return POPMETA; }
option		{ return OPTION; }
plugin		{ return PLUGIN; }
include		{ return INCLUDE; }

 /* Boolean values. */
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

NULL		{
    yylval->pyobj = Py_None;
    Py_INCREF(Py_None);
    return NONE;
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
{ACCOUNTTYPE}(:{ACCOUNTNAME})+		{
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
        PyObject* unicode_str = PyUnicode_Decode(strbuf, strbuf_ptr - strbuf,
                                                 yy_encoding, "ignore");
        if ( unicode_str == NULL ) {
            build_lexer_error_from_exception();
            yylval->pyobj = Py_None;
            Py_INCREF(Py_None);
            return LEX_ERROR;
        }
        BUILD_LEX("STRING", "O", unicode_str);
        Py_DECREF(unicode_str);
        strbuf_ptr = NULL;
        return STRING;
    }

    /* Escape sequences. */
    \\n       SAFE_COPY_CHAR('\n');
    \\t       SAFE_COPY_CHAR('\t');
    \\r       SAFE_COPY_CHAR('\r');
    \\b       SAFE_COPY_CHAR('\b');
    \\f       SAFE_COPY_CHAR('\f');
    \\(.|\n)  SAFE_COPY_CHAR(yytext[1]);

    /* All other characters. */
    [^\\\"]+        {
        if ( yyleng > (strbuf_end - strbuf_ptr) ) {
            strbuf_realloc(yyleng);
        }
        ssize_t i;
        for (i = 0; i < yyleng; ++i) {
            *strbuf_ptr++ = yytext[i];
        }
    }
}

 /* Numbers */
([0-9]+|[0-9][0-9,]+[0-9])(\.[0-9]*)? 		{
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
[a-z][a-zA-Z0-9\-_]+: 		{
    BUILD_LEX("KEY", "s#", yytext, (Py_ssize_t)(yyleng-1));
    unput(':');
    return KEY;
}

 /* Default rule. {bf253a29a820} */
.			{
    unput(*yytext);
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
    char buffer[256];
    size_t length = snprintf(buffer, 256, "Invalid token: '%s'", yytext);
    build_lexer_error(buffer, length);
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

/* The encoding to use for converting strings. */
const char* yy_encoding = NULL;

/* A buffer for parsing string literals. It is reused and its size is dynamically allocated. */
size_t strbuf_size = 0;
char* strbuf = NULL;
char* strbuf_end;
char* strbuf_ptr;

/* Initialize the globals before running the lexer. */
void yylex_initialize(const char* filename, const char* encoding)
{
    assert(filename != NULL);
    yy_eof_times = 0;
    yy_filename = filename;
    yy_line_tokens = 0;
    yycolumn = 1;
    if ( encoding == 0 ) {
        yy_encoding = "utf8";
    }
    else {
        yy_encoding = encoding;
    }

    /* Start with a decent small buffer. */
    strbuf_size = 1024;
    strbuf = realloc(strbuf, strbuf_size + 1);
    strbuf_end = strbuf + strbuf_size - 1;
    strbuf_ptr = NULL;

    /* Note: If we used a reentrant parser, this routine should eventually call
     * yylex_init(). */
}

/* Finalize the globals before running the lexer. */
void yylex_finalize(void)
{
    yy_filename = NULL;

    /* Finalize our reading buffer. */
    if ( strbuf != NULL ) {
        free(strbuf);
        strbuf = NULL;
    }

    /* Call the yylex finalization routine. */
    yylex_destroy();
}


/* Reallocate the buffer to accommodate some new characters. */
void strbuf_realloc(size_t num_new_chars)
{
    assert(strbuf_ptr != NULL);
    size_t cur_size = strbuf_ptr - strbuf;
    size_t new_size = cur_size + num_new_chars;
    while ( strbuf_size < new_size ) {
        strbuf_size <<= 1;
    }
    strbuf = realloc(strbuf, strbuf_size + 1);
    strbuf_ptr = strbuf + cur_size;
    strbuf_end = strbuf + strbuf_size - 1;
}




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
    size_t i;
    for ( i = 0; i < nchars; ++i ) {
        result *= 10;
        result += (buf[i] - '0');
    }
    return result;
}

/* Build and accumulate an error on the builder object. */
void build_lexer_error(const char* string, size_t length)
{
    TRACE_ERROR("Invalid Token");

    /* Build and accumulate a new error object. {27d1d459c5cd} */
    PyObject* rv = PyObject_CallMethod(builder, "build_lexer_error",
                                       "s#", string, (Py_ssize_t)length);
    if (rv == NULL) {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: Building exception from default rule");
    }
    Py_XDECREF(rv);
}

void build_lexer_error_from_exception()
{
    TRACE_ERROR("Lexer Builder Exception");

    /* Get the exception context. */
    PyObject* ptype = NULL;
    PyObject* pvalue = NULL;
    PyObject* ptraceback = NULL;
    PyErr_Fetch(&ptype, &pvalue, &ptraceback);
    PyErr_NormalizeException(&ptype, &pvalue, &ptraceback);

    /* Clear the exception. */
    PyErr_Clear();

    if (pvalue != NULL) {
        /* Build and accumulate a new error object. {27d1d459c5cd} */
        PyObject* rv = PyObject_CallMethod(builder, "build_lexer_error",
                                           "OO", pvalue, ptype);
        Py_XDECREF(ptype);
        Py_XDECREF(pvalue);
        Py_XDECREF(ptraceback);

        if (rv == NULL) {
            PyErr_SetString(PyExc_RuntimeError,
                            "Internal error: While building exception");
        }
    }
    else {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: No exception");
    }
}
