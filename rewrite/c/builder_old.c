#if 0
/* Convert the next 'nchars' characters to an integer. */
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

/* Parses a date of the form YYYY-MM-DD into a Python datetime instance. */
PyObject* parseDate(const char* s)
{
    assert(s != 0);
    int year = strtonl(s, 4);
    int month = strtonl(s+5, 2);
    int day = strtonl(s+8, 2);
    return PyDate_FromDate(year, month, day);

}

/* Parse a C-string into a Python string. */
PyObject* parseString(const char* s)
{
    return PyUnicode_FromString(s);
}

PyObject* parseStringWithSize(const char* s, size_t n)
{
    return PyUnicode_FromStringAndSize(s, n);
}


/* Parse a decimal number. This uses mpdecimal for performance reasons. */
PyObject* parseDecimal(const char* s)
{
#if 0
    void mpd_init(mpd_context_t *ctx, mpd_ssize_t prec);
    void mpd_set_string(mpd_t *result, const char *s, mpd_context_t *ctx);
#endif
    Py_RETURN_NONE;
}
#endif
