#include "itoa.h"

#include <cstdlib>
#include <limits>
#include <cassert>


// Converts from one of the integer types to a string in a fixed buffer.
// Returns the number of characters written.
// Does not insert a \0 terminator.

namespace {
const char* stindex = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz";

template <bool is_signed>
struct MinusSign
{
    template <typename T>
    inline static void addsign(T tmp_value, char*& ptr)
    {}
};

template <>
struct MinusSign<true>
{
    template <typename T>
    inline static void addsign(T tmp_value, char*& ptr) {
        if ( tmp_value < 0 )
            *ptr++ = '-';
    }
};

template <typename T>
inline int itoat(T value, char* result, T base)
{
    // Check the validity of the base.
    if (base < 2 || base > 36) {
        return 0;
    }

    // Convert to a string of chars.
    char* ptr = result;
    T tmp_value;
    do {
        tmp_value = value;
        value /= base;
        *ptr++ = stindex[35 + (tmp_value - value * base)];
    } while ( value );

    // Apply negative sign
    MinusSign< std::numeric_limits<T>::is_signed >::addsign(tmp_value, ptr);

    // Reverse string.
    int len = ptr - result;
    char* ptr1 = result;
    char tmp_char;
    ptr--;
    while (ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr--= *ptr1;
        *ptr1++ = tmp_char;
    }
    assert(len > 0);
    return len;
}


template <bool is_signed>
struct MinusSignFixed
{
    template <typename T>
    inline static void addsign(T tmp_value, char*& ptr)
    {}
};

template <>
struct MinusSignFixed<true>
{
    template <typename T>
    inline static void addsign(T tmp_value, char*& ptr) {
        if ( tmp_value < 0 )
            *ptr-- = '-';
    }
};

// Maximum size of temporary buffer for text rendition of any number.
#define TMPBUF_SIZE 128

template <typename T>
inline int itoat_aligned(T value, char* result, int align, T base, char fill)
{
    // Check the validity of the base.
    if (base < 2 || base > 36) {
        return 0;
    }

    // Convert to a string of chars.
    char* ptr = result + align - 1;
    T tmp_value;
    do {
        tmp_value = value;
        value /= base;
        *ptr-- = stindex[35 + (tmp_value - value * base)];
    } while ( value && ptr >= result );

    // Apply negative sign
    if ( ptr >= result ) {
        MinusSignFixed< std::numeric_limits<T>::is_signed >::addsign(tmp_value, ptr);
    }

    // Fill the prefix with spaces.
    while ( ptr >= result )
        *ptr-- = fill;

    return align;
}


}

size_t itoall(long long value, char* result, int alignment, unsigned int base, char fill)
{
    return itoat_aligned<long long>(value, result, alignment, base, fill);
}

