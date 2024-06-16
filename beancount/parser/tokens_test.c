#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "beancount/parser/tokens.h"


void test_validate_decimal_number()
{
    char buffer[256];
    ssize_t r;
    char *s;

    s = "1";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == 1);
    assert(strcmp(buffer, s) == 0);

    s = "1.00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == 4);
    assert(strcmp(buffer, s) == 0);

    s = "1,000.00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == 7);
    assert(strcmp(buffer, "1000.00") == 0);

    s = "1,000,000.00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == 10);
    assert(strcmp(buffer, "1000000.00") == 0);

    s = "1,00.00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "1,.00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "1.,00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "1.0,0";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "1.000,";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "1.000,000";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "10,00";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "10,";
    r = validate_decimal_number(s, buffer, sizeof(buffer));
    assert(r == -EINVAL);

    s = "1,000.00";
    r = validate_decimal_number(s, buffer, 8);
    assert(r == 7);

    s = "1,000.00";
    r = validate_decimal_number(s, buffer, 7);
    assert(r == -ENOMEM);

}

void test_cunescape()
{
    char* unescaped;
    ssize_t len;
    char* s;

    s = "a";
    len = cunescape(s, strlen(s), true, &unescaped);
    assert(len == 1);
    assert(memcmp(unescaped, "a", len) == 0);
    free(unescaped);

    s = "something longer";
    len = cunescape(s, strlen(s), true, &unescaped);
    assert(len == (ssize_t)strlen(s));
    assert(memcmp(unescaped, "something longer", len) == 0);
    free(unescaped);

    s = "more\nthan\none\nline";
    len = cunescape(s, strlen(s), true, &unescaped);
    assert(len == (ssize_t)strlen(s));
    assert(memcmp(unescaped, "more\nthan\none\nline", len) == 0);
    free(unescaped);

    s = "escaping\\n\\r";
    len = cunescape(s, strlen(s), true, &unescaped);
    assert(len == (ssize_t)strlen(s) - 2);
    assert(memcmp(unescaped, "escaping\n\r", len) == 0);
    free(unescaped);
}

int main(int argc, char **argv)
{
    test_validate_decimal_number();
    test_cunescape();
}
