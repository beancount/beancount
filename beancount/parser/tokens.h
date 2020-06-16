#ifndef BEANCOUNT_TOKENS_H
#define BEANCOUNT_TOKENS_H

#include <stdbool.h>
#include <datetime.h>

#include "macros.h"
#include "decimal.h"


/**
 * Dispatch to token values build functions.
 *
 * Build the semantic value for token kind @name. Further arguments
 * are passed to the factory function. Exceptions raised in the build
 * functions are handled and reported as lexing errors. Return @name
 * or %YYerror in case an exception has been raised and handled.
 */
#define token(name, ...)                                                \
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
#define build_STRING(_str, _len, _enc) pyunicode_from_cquotedstring(_str, _len, _enc)
#define build_ACCOUNT(_str) PyUnicode_InternFromString(_str)
#define build_EXCEPTION(_loc, _builder) ( build_lexer_error_from_exception(_loc, _builder), YYerror )

#define DIGITS "0123456789"
#define LONG_STRING_LINES_MAX 64


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
static ssize_t validate_decimal_number(const char* str, char* buffer, size_t len)
{
    size_t n, digits = 0;
    bool comma = false;
    bool dot = false;
    char* dst;

    if (len == 0) {
        return -ENOMEM;
    }

    for (n = 0, dst = buffer; str[n] != '\0'; n++) {
        if (str[n] == ',') {
            if (n == 0 || (n > 2 && digits != 3) || dot)
                return -EINVAL;
            comma = true;
            digits = 0;
            continue;
        }

        if (isdigit(str[n])) {
            *dst++ = str[n];
            digits++;
        }

        if (str[n] == '.') {
            if (n == 0 || (comma && digits != 3))
                return -EINVAL;
            *dst++ = str[n];
            dot = true;
            digits = 0;
        }

        if (dst == buffer + len) {
            return -ENOMEM;
        }
    }

    if (comma && !dot && digits != 3) {
        return -EINVAL;
    }

    *dst = '\0';

    return dst - buffer;
}


static PyObject* pydecimal_from_cstring(const char* str)
{
    char buffer[256];
    Py_ssize_t len;

    len = validate_decimal_number(str, buffer, sizeof(buffer));
    if (len < 0) {
        PyErr_Format(PyExc_ValueError, "Invalid number format: '%s'", str);
        return NULL;
    }

    return PyDec_FromCString(buffer, len);
}


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
static ssize_t cunescape(const char* string, size_t len, int strict, char** ret, int* lines)
{
    const char* src;
    char* buffer;
    char* dst;
    int lin = 1;

    /* The unescaped string can be at most as long as the escaped
     * string. Make sure we hace space for the string terminator: it
     * is safer to handle NULL terminated strings. */
    buffer = malloc(len + 1);
    if (!buffer) {
        return -ENOMEM;
    }

    for (src = string, dst = buffer; src < string + len; src++) {
        if (*src == '\n')
            lin++;

        if (*src != '\\') {
            *dst = *src;
            dst++;
            continue;
        }

        if (string + len - src < 2) {
            free(buffer);
            return -EINVAL;
        }

        src++;

        switch (*src) {
        case '"':
            *dst++ = '"';
            break;
        case 'n':
            *dst++ = '\n';
            break;
        case 't':
            *dst++ = '\t';
            break;
        case 'r':
            *dst++ = '\r';
            break;
        case 'f':
            *dst++ = '\f';
            break;
        case 'b':
            *dst++ = '\b';
            break;
        default:
            if (strict) {
                free(buffer);
                return -EINVAL;
            }
            *dst++ = *src;
        }
    }

    /* Add NULL terminator to the unescaped string. */
    *dst = '\0';

    *ret = buffer;
    *lines = lin;

    return dst - buffer;
}


static PyObject* pyunicode_from_cquotedstring(char* string, size_t len, const char* encoding)
{
    char* unescaped = NULL;
    ssize_t r;
    int lines;

    r = cunescape(string, len, false, &unescaped, &lines);
    if (r < 0) {
        PyErr_Format(PyExc_ValueError, "Invalid string");
        free(unescaped);
        return NULL;
    }

    if (lines > LONG_STRING_LINES_MAX) {
        PyErr_Format(PyExc_ValueError, "String too long (%d lines)", lines);
        free(unescaped);
        return NULL;
    }

    PyObject* rv = PyUnicode_Decode(unescaped, r, encoding, "ignore");
    free(unescaped);

    return rv;
}


/**
 * Convert ASCII string to an integer.
 *
 * Converts the @string string of length @len to int. The input is
 * assumed to be a valid representation of an integer number. No input
 * validation or error checking is performed.
 */
static int strtonl(const char* string, size_t len)
{
    int result = 0;

    for (size_t i = 0; i < len; ++i) {
        result *= 10;
        result += string[i] - '0';
    }

    return result;
}


/**
 * Convert an ASCII string to a PyDate object.
 *
 * The @string is assumed to be a valid date represetation in the
 * format YYYY-MM-DD allowing for any character to divide the three
 * digits groups.
 */
static PyObject* pydate_from_cstring(const char* string)
{
    int year, month, day;
    size_t n;

    n = strspn(string, DIGITS);
    year = strtonl(string, n);
    string += n + 1;

    n = strspn(string, DIGITS);
    month = strtonl(string, n);
    string += n + 1;

    n = strspn(string, DIGITS);
    day = strtonl(string, n);

    return PyDate_FromDate(year, month, day);
}


#endif /* BEANCOUNT_TOKENS_H */
