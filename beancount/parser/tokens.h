#ifndef BEANCOUNT_TOKENS_H
#define BEANCOUNT_TOKENS_H

#include <stdbool.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "beancount/parser/decimal.h"
#include "beancount/parser/macros.h"

#ifdef _MSC_VER
#include <basetsd.h>
typedef SSIZE_T ssize_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Dispatch to token values build functions.
 *
 * Build the semantic value for token kind @name. Further arguments
 * are passed to the factory function. Exceptions raised in the build
 * functions are handled and reported as lexing errors. Return @name
 * or %YYerror in case an exception has been raised and handled.
 */
#define TOKEN(name, ...)                                                \
    (                                                                   \
        yylval->pyobj = EXPAND(build_##name(__VA_ARGS__)),              \
        yylval->pyobj ? name : build_EXCEPTION(yylloc, builder)         \
    )

#define build_STR(_ptr, _len) PyUnicode_FromStringAndSize(_ptr, _len)
#define build_KEY build_STR
#define build_TAG build_STR
#define build_LINK build_STR
#define build_CURRENCY build_STR
#define build_BOOL(_value) PyBool_FromLong(_value)
#define build_NONE() ( Py_INCREF(Py_None), Py_None )
#define build_NUMBER(_str) pydecimal_from_cstring(_str)
#define build_DATE(_str) pydate_from_cstring(_str)
#define build_STRING(_str, _len) pyunicode_from_cquotedstring(_str, _len)
#define build_ACCOUNT(_str) PyUnicode_InternFromString(_str)
#define build_EXCEPTION(_loc, _builder) ( build_lexer_error_from_exception(_loc, _builder), YYerror )


/**
 * Validate number string representation and remove commas.
 *
 * Store the cleaned up version of @str in the provided buffer @buffer
 * of lenght @len. Strings are checked to match the equivalent of the
 * "^(\d+|\d{1,3}(,\d{3})+)(\.\d+)?$" Python regular expression.
 *
 * Returns: -EINVAL if the string is not valid, -ENOMEM if the output
 * does not fit fit in the provided buffer, the length of the cleaned
 * up string otherwise.
 */
ssize_t validate_decimal_number(const char* str, char* buffer, size_t len);


/**
 * Convert an ASCII string to a PyDecimal object.
 *
 * The string may contain commas as thouands separator.
 */
PyObject* pydecimal_from_cstring(const char* str);


/**
 * Unescape a C-style escaped string.
 *
 * Unescape the string @string of length @len and return the result in
 * a newly allocated buffer @ret. Also return the number of lines in
 * the input string in @lines. If @strict is true, reject invalid
 * escapes sequences.
 *
 * Returns: the number of lines in the string, -EINVAL if the input
 * string contains is not valid, -ENOMEM if the output string cannot
 * be allocated.
 */
ssize_t cunescape(const char* string, size_t len, int strict, char** ret);


/**
 * Convert an UTF-8 encoded string to a PyUnicode object.
 *
 * C-style escape codes contained in the string are unescaped. An
 * exception is raised if the string contains invalid escape sequences
 * or if the string contains more than 64 lines.
 */
PyObject* pyunicode_from_cquotedstring(const char* string, size_t len);


/**
 * Convert date string to (year, month, date).
 */
void parse_date_from_string(const char* string, int* year, int* month, int* day);


/**
 * Convert integer (year, month, date) to a datetime object.
 */
PyObject* pydate_from_civil_day(int year, int month, int day);


/**
 * Convert an ASCII string to a PyDate object.
 *
 * The @string is assumed to be a valid date represetation in the
 * format YYYY-MM-DD allowing for any character to divide the three
 * digits groups.
 */
PyObject* pydate_from_cstring(const char* string);

/**
 * Convert an ASCII string to a PyDate object.
 *
 * The @string is assumed to be a valid date represetation in the
 * format YYYY-MM-DD allowing for any character to divide the three
 * digits groups.
 */
PyObject* pydate_from_cstring(const char* string);

/**
 * Initialize the local datetime globals.
 */
void initialize_datetime();

#ifdef __cplusplus
}
#endif

#endif /* BEANCOUNT_TOKENS_H */
