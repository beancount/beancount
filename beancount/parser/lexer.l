/* -*- mode: c -*- */
/* A flex lexer for Beancount. */

/* Options */
%option noyywrap
%option yylineno
%option never-interactive
%option warn
%option bison-bridge
%option bison-locations
%option reentrant
%option extra-type="yyextra_t*"
/* %option nodefault */
/* %option debug */
/* %option stack */
/* %option 8bit */

/* Top Code. This is included in the FLex generated header file. */
%top{

#include "parser.h"

typedef struct _yyextra_t yyextra_t;

/**
 * Allocate a new scanner object including private data.
 *
 * This encapsulates the native yylex_init_extra() API.
 */
yyscan_t yylex_new(void);

/**
 * Free scanner object including private data.
 *
 * This encapsulated the native yylex_destroy() API. Python objects
 * references stored in the @scanner are decremented.
 */
yyscan_t yylex_free(yyscan_t scanner);

/**
 * Allocate and initialize scanner private data.
 *
 * Setup @scanner to read from the Python file-like object @file. Set
 * the reported file name to @filename, if not NULL and not None.
 * Otherwise try to obtain the file name from the @name attribute of
 * the @file object. If this fails, use the empty string. @encoding is
 * used to decode strings read from the input file, if not NULL,
 * otherwise the default UTF-8 encoding is used. Python objects
 * references are incremented. It is safe to call this multiple times.
 */
void yylex_initialize(PyObject* file, PyObject* filename, int lineno, const char* encoding, yyscan_t scanner);

}

/* Definitions. */
%{

#include <math.h>
#include <stdlib.h>

#include "grammar.h"
#include "tokens.h"

struct _yyextra_t {
    /* The number of times EOF has been hit. This is used to
     * synthesize an EOL at the end of the file. */
    int n_eof;

    /* Number of tokens since the beginning of the line. */
    int n_line_tokens;

    /* The filename being tokenized. */
    PyObject* filename;

    /* The encoding to use for converting strings. */
    const char* encoding;
};

#define yy_eof_times yyget_extra(yyscanner)->n_eof
#define yy_line_tokens yyget_extra(yyscanner)->n_line_tokens
#define yy_filename yyget_extra(yyscanner)->filename
#define yy_encoding yyget_extra(yyscanner)->encoding

/* Build and accumulate an error on the builder object. */
void build_lexer_error(YYLTYPE* loc, PyObject* builder, const char* string, size_t length);

/* Build and accumulate an error on the builder object using the current
 * exception state. */
void build_lexer_error_from_exception(YYLTYPE* loc, PyObject* builder);

int pyfile_read_into(PyObject *file, char *buf, size_t max_size);

#define YY_INPUT(buf, result, max_size)                         \
    result = pyfile_read_into((PyObject *)yyin, buf, max_size);

#define YY_USER_ACTION                                                  \
    {                                                                   \
        yy_line_tokens++;                                               \
        yylloc->first_line = yylineno;                                  \
        yylloc->last_line = yylloc->first_line;                         \
        yylloc->first_column = yycolumn;                                \
        yylloc->last_column = yycolumn + yyleng - 1;                    \
        yylloc->file_name = yy_filename;                                \
        yycolumn += yyleng;                                             \
    }

/* Skip the rest of the input line.  This needs to be implemented as a
 * macro because input() and unput() are themselves macros tha use
 * variable definitions internal to the yylex() function. */
#define yy_skip_line()                          \
    do {                                        \
        for (;;) {                              \
            int c = input(yyscanner);           \
            if (c == 0 || c == -1) {		\
                break;                          \
            }                                   \
            if (c == '\n') {                    \
                unput(c);                       \
                break;                          \
            }                                   \
        }                                       \
    } while (0)

%}

/* A start condition for chomping an invalid token. */
%x INVALID


ASCII           [\x00-\x7f]
UTF-8-1         [\x80-\xbf]
UTF-8-2         [\xc2-\xdf]{UTF-8-1}
UTF-8-3         \xe0[\xa0-\xbf]{UTF-8-1}|[\xe1-\xec]{UTF-8-1}{UTF-8-1}|\xed[\x80-\x9f]{UTF-8-1}|[\xee-\xef]{UTF-8-1}{UTF-8-1}
UTF-8-4         \xf0[\x90-\xbf]{UTF-8-1}{UTF-8-1}|[\xf1-\xf3]{UTF-8-1}{UTF-8-1}{UTF-8-1}|\xf4[\x80-\x8f]{UTF-8-1}{UTF-8-1}
UTF-8-ONLY      {UTF-8-2}|{UTF-8-3}|{UTF-8-4}
UTF-8           {ASCII}|{UTF-8-ONLY}

ACCOUNTTYPE     ([A-Z]|{UTF-8-ONLY})([A-Za-z0-9\-]|{UTF-8-ONLY})*
ACCOUNTNAME     ([A-Z0-9]|{UTF-8-ONLY})([A-Za-z0-9\-]|{UTF-8-ONLY})*


%% /* Rules */

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
    if (yy_line_tokens == 1) {
        /* If the next character completes the line, skip it. */
        if (yyg->yy_hold_char == '\n' ||
	    yyg->yy_hold_char == '\r' ||
	    yyg->yy_hold_char == '\0') {
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
    if (yy_line_tokens != 1) {
        return HASH;
    }
    else {
        /* Allow org-mode titles. */
        yy_skip_line();
        return SKIPPED;
    }
}

\*		{
    if (yy_line_tokens != 1) {
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
    if (yy_line_tokens != 1) {
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
    token(BOOL, true);
    return BOOL;
}

FALSE		{
    token(BOOL, false);
    return BOOL;
}

NULL		{
    token(NONE);
    return NONE;
}

 /* Dates. */
[0-9]{4,}[\-/][0-9]+[\-/][0-9]+	{
    token(DATE, yytext);
    return DATE;
}

 /* Account names. */
{ACCOUNTTYPE}(:{ACCOUNTNAME})+		{
    token(ACCOUNT, yytext);
    return ACCOUNT;
}

 /* Currencies. These are defined as uppercase only in order to disambiguate the
  * syntax. This is kept in sync with beancount.core.amount.CURRENCY_RE. */
[A-Z][A-Z0-9\'\.\_\-]{0,22}[A-Z0-9]	{
    token(CURRENCY, yytext, yyleng);
    return CURRENCY;
}

 /* String literals. */
\"([^\\\"]|\\.)*\"		{
    token(STRING, yytext + 1, yyleng - 2, yy_encoding);
    return STRING;
}

 /* Numbers */
([0-9]+|[0-9][0-9,]+[0-9])(\.[0-9]*)?	{
    token(NUMBER, yytext);
    return NUMBER;
}

 /* Tags */
#[A-Za-z0-9\-_/.]+ 		{
    token(TAG, yytext + 1, yyleng - 1);
    return TAG;
}

 /* Links */
\^[A-Za-z0-9\-_/.]+ 		{
    token(LINK, yytext + 1, yyleng - 1);
    return LINK;
}

 /* Key */
[a-z][a-zA-Z0-9\-_]+/: 		{
    token(KEY, yytext, yyleng);
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
    if (yy_eof_times == 0) {
	yy_eof_times = 1;
	/* Ensure location data is populated. */
	YY_USER_ACTION;
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
    build_lexer_error(yylloc, builder, buffer, length);
    BEGIN(INITIAL);
    return YYerror;
}


%% /* User Code */

yyscan_t yylex_new(void)
{
    yyscan_t scanner;
    yyextra_t* extra;

    extra = malloc(sizeof(*extra));
    if (!extra)
        return NULL;

    extra->filename = NULL;

    yylex_init_extra(extra, &scanner);
    if (!scanner) {
        free(extra);
        return NULL;
    }

    PyDateTime_IMPORT;

    return scanner;
}

yyscan_t yylex_free(yyscan_t scanner)
{
    yyextra_t* extra = yyget_extra(scanner);

    Py_XDECREF(extra->filename);
    free(extra);

    Py_XDECREF(yyget_in(scanner));
    yylex_destroy(scanner);

    return NULL;
}

/* yyrestart() does not reset the scanner back to INITIAL state and
 * Flex does not provide a way of doing so outside a scanner
 * rule. This function does just that accessing Flex internals. */
static void yybegin(yyscan_t scanner)
{
    struct yyguts_t* yyg = (struct yyguts_t*)scanner;
    BEGIN(INITIAL);
}

void yylex_initialize(PyObject* file, PyObject* filename, int lineno, const char* encoding, yyscan_t scanner)
{
    yyextra_t* extra = yyget_extra(scanner);

    if (!filename || filename == Py_None) {
        /* If a filename has not been specified, get it from the 'name'
	 * attribute of the input file object. */
        filename = PyObject_GetAttrString(file, "name");
        if (!filename) {
	    /* No 'name' attribute. */
	    PyErr_Clear();
            /* Use the empty string. */
            filename = PyUnicode_FromString("");
        }
    } else {
        Py_INCREF(filename);
    }

    Py_XDECREF(extra->filename);
    extra->filename = filename;

    extra->n_eof = 0;
    extra->n_line_tokens = 0;
    extra->encoding = encoding ? encoding : "utf-8";

    Py_XDECREF(yyget_in(scanner));
    Py_INCREF(file);
    yyrestart((void *)file, scanner);
    yybegin(scanner);

    yyset_lineno(lineno, scanner);
}

/* Build and accumulate an error on the builder object. */
void build_lexer_error(YYLTYPE* loc, PyObject* builder, const char* string, size_t length)
{
    TRACE_ERROR("Invalid Token");

    /* Build and accumulate a new error object. {27d1d459c5cd} */
    PyObject* rv = PyObject_CallMethod(builder, "build_lexer_error", "Ois#",
				       loc->file_name, loc->first_line,
				       string, (Py_ssize_t)length);
    /* Note: If there was an exception in the callback, let it bubble up. */
    Py_XDECREF(rv);
}

void build_lexer_error_from_exception(YYLTYPE* loc, PyObject* builder)
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
        PyObject* rv = PyObject_CallMethod(builder, "build_lexer_error", "OiOO",
					   loc->file_name, loc->first_line,
					   pvalue, ptype);
        Py_XDECREF(ptype);
        Py_XDECREF(pvalue);
        Py_XDECREF(ptraceback);

        /* If there was an exception during the handling of the lexer error, add
         * a prefix to the message to make it clear this wasn't expected. */
        if (rv == NULL) {
            PyObject* message = PyUnicode_FromFormat(
                "Internal error while building exception for: %S", pvalue);
            PyErr_SetObject(PyExc_RuntimeError, message);
            Py_DECREF(message);
        }
    }
    else {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: No exception");
    }
}

int pyfile_read_into(PyObject *file, char *buf, size_t max_size)
{
    PyObject* dest = NULL;
    PyObject* read = NULL;
    int ret = 0;

    // Note: Eventually we ought to allocate this once in the parser state and
    // avoid reallocating this on every block read.
    dest = PyMemoryView_FromMemory(buf, max_size, PyBUF_WRITE);
    if (!dest) {
	goto error;
    }

    read = PyObject_CallMethod(file, "readinto", "O", dest);
    if (!read) {
	goto error;
    }

    ret = PyLong_AsSize_t(read);
    if (PyErr_Occurred()) {
	ret = 0;
    }

error:
    Py_XDECREF(dest);
    Py_XDECREF(read);
    return ret;
}
