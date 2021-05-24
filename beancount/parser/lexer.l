/* -*- mode: c -*- */
/* A flex lexer for Beancount. */

/* Options */
%option noinput
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

/* Top code. This is included in the generated header file. */
%top{

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#define MAX_INCLUDE_DEPTH 128

typedef struct _yyextra_t yyextra_t;

struct _yyextra_t {
    /* Include stack current depth. */
    int depth;

    /* The filename being tokenized. */
    PyObject* names[MAX_INCLUDE_DEPTH];

    /* The encoding to use for converting strings. */
    const char* encoding;

    /* A reference to the beancount.core.number.MISSING object */
    PyObject* missing_obj;
};

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

/* Lexer interface required by Bison. */
#define YY_DECL int yylex(YYSTYPE* yylval_param, YYLTYPE* yylloc_param, \
                          yyscan_t yyscanner, PyObject* builder)

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
 * Initialize scanner private data.
 *
 * Setup @scanner to read from the Python file-like object @file. Set
 * the reported file name to @filename, if not NULL and not None.
 * Otherwise try to obtain the file name from the @name attribute of
 * the @file object. If this fails, use the empty string. @encoding is
 * used to decode strings read from the input file, if not NULL,
 * otherwise the default UTF-8 encoding is used. Python objects
 * references are incremented. It is safe to call this multiple times.
 */
void yylex_initialize(PyObject* file, PyObject* filename, int lineno,
                      const char* encoding, PyObject* missing_obj, yyscan_t scanner);

}

/* Definitions. */
%{

#include <math.h>
#include <stdlib.h>
#include <stdarg.h>

#include "beancount/parser/grammar.h"
#include "beancount/parser/tokens.h"

/**
 * Build and accumulate an error on the builder object.
 */
void build_lexer_error(YYLTYPE* loc, PyObject* builder, const char* format, ...);

/**
 * Build and accumulate an error using the current exception state.
 */
void build_lexer_error_from_exception(YYLTYPE* loc, PyObject* builder);

/**
 * Read from a io.BaseIO Python object into a buffer.
 */
int pyfile_read_into(PyObject *file, char *buf, size_t max_size);

#define YY_INPUT(buf, result, max_size)                         \
    result = pyfile_read_into((PyObject *)yyin, buf, max_size);

#define YY_USER_ACTION                                                  \
    {                                                                   \
        yylloc->first_line = yylineno;                                  \
        yylloc->last_line = yylloc->first_line;                         \
        yylloc->first_column = yycolumn;                                \
        yylloc->last_column = yycolumn + yyleng - 1;                    \
        yylloc->file_name = yyget_extra(yyscanner)->names[yyget_extra(yyscanner)->depth]; \
        yycolumn += yyleng;                                             \
    }

%}

%x INVALID
%x IGNORE

ASCII           [\x00-\x7f]
UTF-8-1         [\x80-\xbf]
UTF-8-2         [\xc2-\xdf]{UTF-8-1}
UTF-8-3         \xe0[\xa0-\xbf]{UTF-8-1}|[\xe1-\xec]{UTF-8-1}{UTF-8-1}|\xed[\x80-\x9f]{UTF-8-1}|[\xee-\xef]{UTF-8-1}{UTF-8-1}
UTF-8-4         \xf0[\x90-\xbf]{UTF-8-1}{UTF-8-1}|[\xf1-\xf3]{UTF-8-1}{UTF-8-1}{UTF-8-1}|\xf4[\x80-\x8f]{UTF-8-1}{UTF-8-1}
UTF-8-ONLY      {UTF-8-2}|{UTF-8-3}|{UTF-8-4}
UTF-8           {ASCII}|{UTF-8-ONLY}

ACCOUNTTYPE     ([A-Z]|{UTF-8-ONLY})([A-Za-z0-9\-]|{UTF-8-ONLY})*
ACCOUNTNAME     ([A-Z0-9]|{UTF-8-ONLY})([A-Za-z0-9\-]|{UTF-8-ONLY})*

 /* Characters that may be used as flags. Single-character letters may also be
  * used as flags, but must be prefixed by a quote, as in "'P' for "P". */
FLAGS           [!&#?%]

%% /* Rules. */

 /* Newlines matter. */
\n {
    yycolumn = 1;
    return EOL;
}

 /* Whitespace: ignored, except when found at the beginning of a line
  * and followed by a regular character. This is how we detect an
  * initial indent and thus group syntax elements in the grammar. */
^[ \t]+/[^ \t\r\n] { return INDENT; }
[ \t\r]+ { }

 /* Comments. */
;.* { }

 /* Boolean values. Must be before currencies. */
TRUE {
    return TOKEN(BOOL, true);
}

FALSE {
    return TOKEN(BOOL, false);
}

NULL {
    return TOKEN(NONE);
}

 /* Currencies.
  *
  * A currency (or sometimes called "commodities") is a string which
  * - Begins with either a letter or a slash (/) character.
  * - Contains at least one letter.
  * - Ends with either a letter or a number.
  * - May contain the following special characters within: period (.),
  *   underscore (_), dash (-), or quote (').
  * The pattern has to be kept in sync with beancount.core.amount.CURRENCY_RE.
  *
  * In Beancount v3 (C++ version), a single character (e.g. "V" for Visa) is a
  * valid currency. v2 uses the GNU flex scanner which does not have the
  * lookahead capability required to make this possible but the v3 C++ scanner
  * uses Genivia's RE-Flex which supports a lookahead and it will.
  *
  * Example currencies:
  *   "AAPL" (stock)
  *   "V" (single-character stock)
  *   "NT.TO" (stock on another market)
  *   "TLT_040921C144" (equity option)
  *   "/6J" (currency futures)
  *   "/NQH21" (commodity futures)
  *   "/NQH21_QNEG21C13100" (futures option)
  *
  * Counter-examples:
  * - "/6.3": If a currency begins with a slash, the following pattern has to
  *   include at least one letter. "/6J" is a valid currency, but "/6.3" is not.
  * - "CAC_": A currency cannot end with a special character; it has to end with
  *   either an uppercase letter or a number ("C345" is a valid currency, but
  *   "C_" is not).
  *
  * Note: In this scanner we locate the rule above that matching SLASH on
  * purpose so that a name starting with '/' for a futures contract will match
  * with higher priority than that matching a divide '/' character.
  *
  * Keep in sync with {edefe3fbe907}.
  */
[A-Z][A-Z0-9\'\.\_\-]*[A-Z0-9] {
    return TOKEN(CURRENCY, yytext, yyleng);
}
\/[A-Z0-9\'\.\_\-]*[A-Z]([A-Z0-9\'\.\_\-]*[A-Z0-9])? {
    return TOKEN(CURRENCY, yytext, yyleng);
}

 /* Literal includes.
  *
  * This is an addtion to the syntax in v3.0. Lines starting with the hash
  * character # have been ignored so far thus this addition is backward
  * compatible. The syntax has been choosen for this reason and because it
  * resambles the familiar C preprocessor syntax. Includes can be handled
  * directly in the lexer or in the parser. We choose the first option.
  */
^include[ \t]+\".*?\"[ \t]* {
    yyextra_t *extra = yyget_extra(yyscanner);
    const char *ptr = strchr(yytext, '"') + 1;
    const size_t len = yyleng - (ptr - yytext) - 1;
    PyObject *name = NULL, *ret = NULL, *path, *input;

    if (extra->depth == MAX_INCLUDE_DEPTH - 1) {
        PyErr_Format(PyExc_ValueError, "#include cannot be nested more than %d levels deep.", MAX_INCLUDE_DEPTH);
        goto out;
    }

    name = PyUnicode_FromStringAndSize(ptr, len);
    if (!name) {
        goto out;
    }

    ret = PyObject_CallMethod(builder, "include", "OO", name, extra->names[extra->depth]);
    if (!ret) {
        goto out;
    }
    
    path = PyTuple_GetItem(ret, 0);
    input = PyTuple_GetItem(ret, 1);
    if (!path || !input) {
        goto out;
    }

    YY_BUFFER_STATE buf = yy_create_buffer((void *)input, YY_BUF_SIZE, yyscanner);
    yypush_buffer_state(buf, yyscanner);
    extra->names[++extra->depth] = path;
    Py_INCREF(input);
    Py_INCREF(path);
    
out:
    Py_XDECREF(ret);
    Py_XDECREF(name);

    if (PyErr_Occurred()) {
	build_lexer_error_from_exception(yylloc, builder);
	return YYerror;
    }
}

 /* Characters with special meanings. */
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
\#		{ return HASH; }
\*		{ return ASTERISK; }
\:		{ return COLON; }

{FLAGS} {
    yylval->character = yytext[0];
    return FLAG;
}
'[A-Z] {
    yylval->character = yytext[1];
    return FLAG;
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

 /* Dates. */
[0-9]{4,}[\-/][0-9]+[\-/][0-9]+ {
    return TOKEN(DATE, yytext);
}

 /* Account names. */
{ACCOUNTTYPE}(:{ACCOUNTNAME})+ {
    return TOKEN(ACCOUNT, yytext);
}

 /* String literals. */
\"([^\\\"]|\\.)*\" {
    return TOKEN(STRING, yytext + 1, yyleng - 2, yyget_extra(yyscanner)->encoding);
}

 /* Numbers. */
([0-9]+|[0-9][0-9,]+[0-9])(\.[0-9]*)? {
    return TOKEN(NUMBER, yytext);
}

 /* Tags. */
#[A-Za-z0-9\-_/.]+ {
    return TOKEN(TAG, yytext + 1, yyleng - 1);
}

 /* Links. */
\^[A-Za-z0-9\-_/.]+ {
    return TOKEN(LINK, yytext + 1, yyleng - 1);
}

 /* Keys. */
[a-z][a-zA-Z0-9\-_]+/: {
    return TOKEN(KEY, yytext, yyleng);
}

 /* Lines starting with an asterisk, a colon, an hash, or a character
  * in the FLAGS characters set are ignored. This rule is inserted
  * here to give higher precedence to rules matching valid tokens. */
^[\*\:\#]/.	{ BEGIN(IGNORE); }
^{FLAGS}/.	{ BEGIN(IGNORE); }

 /* Default rule. {bf253a29a820} */
. {
    unput(*yytext);
    BEGIN(INVALID);
}

<<EOF>> {
    yyextra_t *extra = yyget_extra(yyscanner);

    if (extra->depth == 0) {
        /* Ensure location data is populated. */
        YY_USER_ACTION;
        return YYEOF;
    }

    Py_XDECREF(yyget_in(yyscanner));
    Py_XDECREF(extra->names[extra->depth--]);
    yypop_buffer_state(yyscanner);
}

 /* Ivalid input: skip over to to the next whitespace character. */
<INVALID>[^ \t\n\r]+ {
    PyObject* input = PyUnicode_Decode(yytext, yyleng, "utf-8", "backslashreplace");
    build_lexer_error(yylloc, builder, "Invalid token: '%U'", input);
    BEGIN(INITIAL);
    return YYerror;
}
 /* Ignore input till the newline. */
<IGNORE>.* {
    BEGIN(INITIAL);
}

%% /* User code. */

yyscan_t yylex_new(void)
{
    yyscan_t scanner;
    yyextra_t* extra;

    extra = malloc(sizeof(*extra));
    if (!extra)
        return NULL;

    extra->depth = 0;
    extra->names[extra->depth] = NULL;

    yylex_init_extra(extra, &scanner);
    if (!scanner) {
        free(extra);
        return NULL;
    }

    return scanner;
}

yyscan_t yylex_free(yyscan_t scanner)
{
    yyextra_t* extra = yyget_extra(scanner);

    while (extra->depth >= 0) {
        Py_XDECREF(extra->names[extra->depth--]);
        Py_XDECREF(yyget_in(scanner));
        yypop_buffer_state(scanner);
    }
    free(extra);

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

void yylex_initialize(PyObject* file, PyObject* filename, int lineno,
                      const char* encoding, PyObject* missing_obj, yyscan_t scanner)
{
    yyextra_t* extra = yyget_extra(scanner);

    while (extra->depth >= 0) {
        Py_XDECREF(extra->names[extra->depth--]);
        Py_XDECREF(yyget_in(scanner));
        yypop_buffer_state(scanner);
    }

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

    extra->depth = 0;
    extra->names[extra->depth] = filename;

    extra->encoding = encoding ? encoding : "utf-8";

    extra->missing_obj = missing_obj;

    Py_INCREF(file);
    yyrestart((void *)file, scanner);
    yybegin(scanner);

    yyset_lineno(lineno, scanner);
}

void build_lexer_error(YYLTYPE* loc, PyObject* builder, const char* format, ...)
{
    PyObject* error;
    PyObject* rv;
    va_list va;

    va_start(va, format);
    error = PyUnicode_FromFormatV(format, va);
    va_end(va);

    if (!error) {
        return;
    }

    rv = PyObject_CallMethod(builder, "build_lexer_error", "OiO",
                             loc->file_name, loc->first_line, error);

    Py_XDECREF(rv);
    Py_XDECREF(error);
}

void build_lexer_error_from_exception(YYLTYPE* loc, PyObject* builder)
{
    PyObject* type;
    PyObject* value;
    PyObject* traceback;

    PyErr_Fetch(&type, &value, &traceback);
    PyErr_NormalizeException(&type, &value, &traceback);

    build_lexer_error(loc, builder, "%s: %S", PyExceptionClass_Name(type), value);

    Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(traceback);
}

int pyfile_read_into(PyObject *file, char *buf, size_t max_size)
{
    PyObject* dest = NULL;
    PyObject* read = NULL;
    int ret = 0;

    /* This function could be optimized in two ways: avoiding to
     * allocate a new memory view object for every block read and
     * caching the lookup of the readinto method of the file
     * object. */

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
