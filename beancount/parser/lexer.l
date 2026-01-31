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
/* %option nodefault */
/* %option debug */
/* %option stack */
/* %option 8bit */
%option nostdinit
%option params="PyObject* builder"
%option lexer="Lexer"

%class{
  public:
    /* The Python file object to read from. */
    PyObject* pyin;

    /* The filename being tokenized. */
    PyObject* filename;

    /* The encoding to use for converting strings. */
    const char* encoding;

    /* A reference to the beancount.core.number.MISSING object */
    PyObject* missing_obj;

    ~Lexer() {
        Py_XDECREF(filename);
        Py_XDECREF(pyin);
    }

    virtual size_t LexerInput(char *s, size_t n) {
        if (!this->pyin) {
            return 0;
        }
        return pyfile_read_into(this->pyin, s, n);
    }
}

%init{
    filename = NULL;
    pyin = NULL;
}

/* Top code. This is included in the generated header file. */
%top{

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#ifdef PYPY_VERSION_NUM
/* PyPy does not export this function. */
#define PyExceptionClass_Name(x) (((PyTypeObject*)(x))->tp_name)
#endif

#include "beancount/parser/macros.h"
#include "beancount/parser/grammar.h"
#include "beancount/parser/tokens.h"

typedef void* yyscan_t;

#define YY_EXTERN_C extern "C"

#define YY_USER_ACTION                          \
    {                                           \
        yylloc.file_name = this->filename;      \
    }

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
 * Setup @scanner to read from the Python file-like object @file. Set the
 * reported file name to @filename, if not NULL and not None. Otherwise try to
 * obtain the file name from the @name attribute of the @file object. If this
 * fails, use the empty string. Python objects references are incremented. It
 * is safe to call this multiple times.
 */
void yylex_initialize(PyObject* file, PyObject* filename, int lineno,
                      PyObject* missing_obj, yyscan_t _scanner);

/**
 * Read from a io.IOBase Python object into a buffer.
 */
size_t pyfile_read_into(PyObject *file, char *buf, size_t max_size);

/**
 * Return a reference to the "missing object" Python marker object.
 */
PyObject* yyget_missing_obj(yyscan_t scanner);

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

 /* Characters that may be used as flags. Single-character letters may also be
  * used as flags, but must be prefixed by a quote, as in "'P' for "P". */
FLAGS           [!&#?%]

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
  * Upper case letters are both valid currencies and valid flags. To
  * kepe the lexer simple, these are lexed as their own token, see the
  * rule for CAPITAL below. They are interpreted ad a flag or as a
  * currency in the grammar.
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
 /* The whitespace lookahead is not necessary to correctly parse valid
  * ledgers, but it enables the lexer to throw an error sooner on
  * invalid input. For example, without the lookahead, an invalid
  * account name like "Assets:invalid" is tokenized into CAPITAL
  * ('A'), KEY ('ssets:'), erorr ('invalid'). With the lookahead the
  * lexer errors out immediately. The first form would be anyhow
  * rejected by the grammar, but the latter seems preferable. */
[A-Z]/[ \t\n] {
    yylval.character = yytext[0];
    return CAPITAL;
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
    return token(STRING, yytext + 1, yyleng - 2, this->encoding);
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
    YYSTYPE lval;
    YYLTYPE lloc;

    yylex_init(&scanner);
    if (!scanner) {
        return NULL;
    }

    /* Start scanning with a NULL input source assigned to the lexer.
     * This forces a Martcher obejct to be associated to the Lexer and
     * then allows to set the scanner initial line number. */
    yylex(&lval, &lloc, scanner, NULL);

    return scanner;
}

YY_EXTERN_C yyscan_t yylex_free(yyscan_t _scanner)
{
    yylex_destroy(_scanner);

    return NULL;
}

YY_EXTERN_C
void yylex_initialize(PyObject* file, PyObject* filename, int lineno,
                      PyObject* missing_obj, yyscan_t _scanner)
{
    yyscanner_t* scanner = static_cast<yyscanner_t*>(_scanner);

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

    Py_XDECREF(scanner->filename);
    scanner->filename = filename;

    extra->missing_obj = missing_obj;
    scanner->encoding = "utf-8";

    Py_XDECREF(scanner->pyin);
    scanner->pyin = file;
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

#ifdef PYPY_VERSION_NUM
    Py_ssize_t length;
    char* buffer;

    /* PyPy does not support readinto() a memory view. Resort to
     * read() and copy the content of the returned bytes object into
     * the lexer buffer. */

    read = PyObject_CallMethod(file, "read", "n", (Py_ssize_t)max_size);
    if (!read) {
	goto error;
    }

    ret = PyBytes_AsStringAndSize(read, &buffer, &length);
    if (ret < 0) {
        goto error;
    }

    assert(length <= max_size);
    memcpy(buf, buffer, length);
    ret = length;

#else
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
#endif

error:
    Py_XDECREF(dest);
    Py_XDECREF(read);
    return ret;
}

YY_EXTERN_C PyObject* yyget_missing_obj(yyscan_t scanner)
{
    return static_cast<yyscanner_t*>(scanner)->missing_obj;
}
