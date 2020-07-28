/* -*- mode: c++ -*- */
/* A lexer for Beancount. */

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
%option nostdinit
%option params="PyObject* builder"

%class{
    virtual size_t LexerInput(char *s, size_t n) {
        PyObject* in = yyget_extra(this)->in;
        if (!in) {
            return 0;
        }
        return pyfile_read_into(in, s, n);
    }
}

/* Top code. This is included in the generated header file. */
%top{

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "beancount/parser/macros.h"
#include "beancount/parser/grammar.h"

typedef void* yyscan_t;
typedef struct _yyextra_t yyextra_t;

struct _yyextra_t {
  /* The Python file object to read from. */
  PyObject* in;

  /* The filename being tokenized. */
  PyObject* filename;

  /* The encoding to use for converting strings. */
  const char* encoding;

  /* A reference to the beancount.core.number.MISSING object */
  PyObject* missing_obj;
};

#define YY_USER_ACTION                                          \
    {                                                           \
        yylloc.file_name = yyget_extra(yyscanner)->filename;    \
    }

#define YY_EXTERN_C extern "C"

C_BEGIN_DECLS

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

/**
 * Read from a io.IOBase Python object into a buffer.
 */
size_t pyfile_read_into(PyObject *file, char *buf, size_t max_size);

C_END_DECLS

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
void build_lexer_error(YYLTYPE& loc, PyObject* builder, const char* format, ...);

/**
 * Build and accumulate an error using the current exception state.
 */
void build_lexer_error_from_exception(YYLTYPE& loc, PyObject* builder);

%}

%x INVALID
%x IGNORE

ACCOUNTTYPE     [\p{Lu}][\p{L}\p{Nd}\-]*
ACCOUNTNAME     [\p{Lu}\p{Nd}][\p{L}\p{Nd}\-]*

 /* Characters that may be used as flags. Make sure to include all the
  * values from beancount.core.flags to allow round-trips between text
  * and entries. {5307d8fa1e7b} */
FLAGS           [!&#?%PSTCURM]

%% /* Rules. */

 /* Newlines matter. */
\n {
    return EOL;
}

 /* Whitespace: ignored, except when found at the beginning of a line
  * and followed by a regular character. This is how we detect an
  * initial indent and thus group syntax elements in the grammar. */
^[ \t]+/[^ \t\r\n] { return INDENT; }
[ \t\r]+ { }

 /* Comments. */
;.* { }

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
    yylval.character = yytext[0];
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
include		{ return INCLUDE; }

TRUE {
    return token(BOOL, true);
}

FALSE {
    return token(BOOL, false);
}

NULL {
    return token(NONE);
}

 /* Dates. */
[0-9]{4,}[\-/][0-9]+[\-/][0-9]+ {
    return token(DATE, yytext);
}

 /* Account names. */
{ACCOUNTTYPE}(:{ACCOUNTNAME})+ {
    return token(ACCOUNT, yytext);
}

 /* Currencies. These are defined as uppercase only in order to
  * disambiguate the syntax. This is to be kept in sync with
  * beancount.core.amount.CURRENCY_RE. */
[A-Z][A-Z0-9\'\.\_\-]{0,22}[A-Z0-9] {
    return token(CURRENCY, yytext, yyleng);
}

 /* String literals. */
\"([^\\\"]|\\.)*\" {
    return token(STRING, yytext + 1, yyleng - 2, yyget_extra(yyscanner)->encoding);
}

 /* Numbers. */
([0-9]+|[0-9][0-9,]+[0-9])(\.[0-9]*)? {
    return token(NUMBER, yytext);
}

 /* Tags. */
#[A-Za-z0-9\-_/.]+ {
    return token(TAG, yytext + 1, yyleng - 1);
}

 /* Links. */
\^[A-Za-z0-9\-_/.]+ {
    return token(LINK, yytext + 1, yyleng - 1);
}

 /* Keys. */
[a-z][a-zA-Z0-9\-_]+/: {
    return token(KEY, yytext, yyleng);
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
    /* Ensure location data is populated. */
    YY_USER_ACTION;
    return YYEOF;
}

 /* Ivalid input: skip over to to the next whitespace character. */
<INVALID>[^ \t\n\r]+ {
    build_lexer_error(yylloc, builder, "Invalid token: '%s'", yytext);
    BEGIN(INITIAL);
    return YYerror;
}

 /* Only the `.` pattern matches invalid UTF-8 bytes. */
<INVALID>. {
    build_lexer_error(yylloc, builder, "Unicode decode error: byte \\x%x is not valid", yytext[0] & 0xFF);
    BEGIN(INITIAL);
    return YYerror;
}

 /* Ignore input till the newline. */
<IGNORE>.* {
    BEGIN(INITIAL);
}

%% /* User code. */

YY_EXTERN_C yyscan_t yylex_new(void)
{
    yyscan_t scanner;
    yyextra_t* extra;
    YYSTYPE lval;
    YYLTYPE lloc;

    extra = (yyextra_t*)malloc(sizeof(*extra));
    if (!extra)
        return NULL;

    extra->filename = NULL;
    extra->in = NULL;

    yylex_init_extra(extra, &scanner);
    if (!scanner) {
        free(extra);
        return NULL;
    }

    /* Start scanning with a NULL input source assigned to the lexer.
     * This forces a Martcher obejct to be associated to the Lexer and
     * then allows to set the scanner initial line number. */
    yylex(&lval, &lloc, scanner, NULL);

    return scanner;
}

YY_EXTERN_C yyscan_t yylex_free(yyscan_t scanner)
{
    yyextra_t* extra = yyget_extra(scanner);

    Py_XDECREF(extra->filename);
    Py_XDECREF(extra->in);
    free(extra);

    yylex_destroy(scanner);

    return NULL;
}

YY_EXTERN_C void yylex_initialize(PyObject* file, PyObject* filename, int lineno,
                                  const char* encoding, PyObject* missing_obj, yyscan_t scanner)
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

    extra->encoding = encoding ? encoding : "utf-8";

    extra->missing_obj = missing_obj;

    Py_XDECREF(extra->in);
    extra->in = file;
    Py_INCREF(file);

    yyrestart("", scanner);
    yyset_lineno(lineno, scanner);
}

void build_lexer_error(YYLTYPE& loc, PyObject* builder, const char* format, ...)
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
                             loc.file_name, loc.first_line, error);

    Py_XDECREF(rv);
    Py_XDECREF(error);
}

void build_lexer_error_from_exception(YYLTYPE& loc, PyObject* builder)
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

size_t pyfile_read_into(PyObject *file, char *buf, size_t max_size)
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
