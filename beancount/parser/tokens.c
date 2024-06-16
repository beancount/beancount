#include "beancount/parser/tokens.h"

#define PY_SSIZE_T_CLEAN
#include <datetime.h>

#include <assert.h>


ssize_t validate_decimal_number(const char* str, char* buffer, size_t len) {
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

PyObject* pydecimal_from_cstring(const char* str)
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

ssize_t cunescape(const char* string, size_t len, int strict, char** ret) {
    const char* src;
    char* buffer;
    char* dst;

    /* The unescaped string can be at most as long as the escaped
     * string. Make sure we have space for the string terminator: it
     * is safer to handle NULL terminated strings. */
    buffer = malloc(len + 1);
    if (!buffer) {
        return -ENOMEM;
    }

    for (src = string, dst = buffer; src < string + len; src++) {
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

    return dst - buffer;
}

PyObject* pyunicode_from_cquotedstring(const char* string, size_t len) {
    char* unescaped = NULL;
    ssize_t r;

    r = cunescape(string, len, false, &unescaped);
    if (r < 0) {
        PyErr_Format(PyExc_ValueError, "Invalid string");
        free(unescaped);
        return NULL;
    }

    PyObject* rv = PyUnicode_FromStringAndSize(unescaped, r);
    free(unescaped);

    return rv;
}

#define DIGITS "0123456789"

/**
 * Convert ASCII string to an integer.
 *
 * Converts the @string string of length @len to int. The input is
 * assumed to be a valid representation of an integer number. No input
 * validation or error checking is performed.
 */
int strtonl(const char* string, size_t len) {
    int result = 0;
    for (size_t i = 0; i < len; ++i) {
        result *= 10;
        result += string[i] - '0';
    }
    return result;
}

void parse_date_from_string(const char* string, int* year, int* month, int* day) {
    size_t n;

    n = strspn(string, DIGITS);
    *year = strtonl(string, n);
    string += n + 1;

    n = strspn(string, DIGITS);
    *month = strtonl(string, n);
    string += n + 1;

    n = strspn(string, DIGITS);
    *day = strtonl(string, n);
}

PyObject* pydate_from_civil_day(int year, int month, int day) {
    assert(PyDateTimeAPI != 0);
    return PyDate_FromDate(year, month, day);
}

PyObject* pydate_from_cstring(const char* string) {
    int year, month, day;
    parse_date_from_string(string, &year, &month, &day);
    return pydate_from_civil_day(year, month, day);
}

void initialize_datetime() {
    /* Note: This needs to be defined in the same file as PyDate* functions are
     * used due to a variable declared in the header file. */
    PyDateTime_IMPORT;
}
