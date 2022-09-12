# About Decimal Types

This technical note details the data types in use across the Beancount v3 system
across C++ and Python to represent decimal numbers.

## Overview

As with Beancount v2, clearly we'd like to avoid floating-point representation
and use an IEEE decimal type throughout. In v2 this was simple: we used Python's
'decimal' module and its associated 'Decimal' datatype everywhere. Because there
was no non-native serialization, this is all that was needed.

In v3, we have two complications:

- The core is implemented in C++ in addition to Python
- The data representation is coerced to protobuf messages throughout the system.

Therefore, we encounter multiple decimal representations and conversions are
necessary. In particular, conversions involves memory management issues because

the representation involves heap-allocated memory.

## Representations
### mpdecimal: `mpd_t` (C struct)

At the most basic level, the `mpdecimal` library provides a nice C struct type
to represent decimal numbers and a rich library of functions to operate on them:

    typedef struct mpd_t {
        uint8_t flags;
        mpd_ssize_t exp;
        mpd_ssize_t digits;
        mpd_ssize_t len;
        mpd_ssize_t alloc;
        mpd_uint_t *data;
    } mpd_t;

This definition is found in `mpdecimal-2.5.1/libmpdec/mpdecimal.h.in`. This is
an implemented of the IEEE standard for unbound decimal object representations.

In particular, the `data` field is a pointer to the potentially large mantissa
of the decimal number. The memory block pointed to by this field *may* be owned
by the `mpd_t` instance or not (it may be dynamically allocated or refer to a
static block of memory). This is controlled by one of the following flags:

    (absent)        : data is dynamically allocated
    MPD_STATIC_DATA : the coefficient is statically allocated
    MPD_SHARED_DATA : the coefficient is shared
    MPD_CONST_DATA  : the coefficient is constant

Note that the `MPD_SHARED_DATA` and `MPD_CONST_DATA` are used only internally
and cannot be used in all the API functions. As `mpd_t` instances can be
mutated, a statically allocated data block may be promoted to a dynamically
allocated one if necessary.

For completeness, in addition there is a confusingly similar flag that tells us
whether the `mpd_t` struct *itself* is dynamically allocated or not (and its
`mpd_del()` deletion function honors that). This is used internally, for its
constants, and allows mixing static and dynamic allocation in its callers:

    MPD_STATIC : the mpd_d is statically allocated (or on the stack)
    (absent)   : the mpd_t is dynamically allocated using malloc()/free()

The various API functions handle this automatically, including switching of the
flags when resizing an `mpd_t` for a mutating operation. This is documented
somewhat in https://www.bytereef.org/mpdecimal/doc/libmpdec/memory.html

This allocation design makes it possible to use a small default static size to
contain most numbers yet retain the flexibility of expanding the precision of
the numbers beyond that size (using dynamic allocation). One just has to be
careful to use the API functions for the manipulation of data.


### mpdecimal: `mpd_uint128_triple_t` (C struct)

For compactness (I presume), a fixed size struct is provided that can contain
decimal objects up to 30 decimal digits. The `mpdecimal` library calls it "a
triple" (type `mpd_uint128_triple_t`). Here's its definition:

    enum mpd_triple_class {
      MPD_TRIPLE_NORMAL,
      MPD_TRIPLE_INF,
      MPD_TRIPLE_QNAN,
      MPD_TRIPLE_SNAN,
      MPD_TRIPLE_ERROR,
    };

    typedef struct {
      enum mpd_triple_class tag;
      uint8_t sign;  /* 0 for positive and 1 for negative */
      uint64_t hi;   /* ((uint128_t)hi << 64) + lo is the coefficient */
      uint64_t lo;
      int64_t exp;   /* exponent */
    } mpd_uint128_triple_t;

One can easily convert to and from an `mpd_t` and a triple. Conversion to a
triple, however, is subject to potential overflow, and this is handled like
this:

    triple = mpd_as_uint128_triple(dec);
    switch (triple.tag) {
    ...
    case MPD_TRIPLE_ERROR:
        /* The coefficient is too large for conversion. Handle the error. */
        break;
    }

(We leverage the triple in our protocol buffer representation below.)


### mpdecimal++: `decimal::Decimal` (C++ class)

The `mpdecimal` package also provides a C++ wrapper for an `mpd_t`:  the
`decimal::Decimal` class. This definition is found in
`mpdecimal-2.5.1/libmpdec++/decimal.hh`.

This class essentially wraps an `mpd_t` instance along with a small fixed-size
block of memory that can handle most numbers (thereby avoiding much dynamic
memory allocation):

    constexpr mpd_ssize_t MINALLOC = 4;

    class Decimal {
     private:
      mpd_uint_t data[MINALLOC] = {0};

      mpd_t value {
        MPD_STATIC|MPD_STATIC_DATA|MPD_SNAN, /* flags */
        0,                                   /* exp */
        0,                                   /* digits */
        0,                                   /* len */
        MINALLOC,                            /* alloc */
        data                                 /* data */
      };

As you can see it's initialized as static by default and promoted as needed.

This is the main representation we'd like to use in C++ code as it provides an
idiomatically convenient set of operators for working with the numbers. I'm not
100% convinced we can't conveniently do away from it (to avoid numerous
conversions), because we're unlikely to perform many operations on the numbers
beyond those that are defined in the syntax. (Maybe it's just fine to deal with
`mpd_t` directly to avoid copies and conversions).


### cdecimal: `PyDecType*` (C/Python type)

The Python library provides the `decimal` module. This implementation of a
`PyDecType` object is included in the Python source code itself and can be found
at `Python-X.XX.X/Modules/_decimal/_decimal.c`. Note in prior versions of
Python, the `cdecimal` implementation was not included in its source and code
and was downloadable externally from the same source as `mpdecimal`.

The main definition is a C/Python subclass of `PyObject*`:

    /* _Py_DEC_MINALLOC >= MPD_MINALLOC */
    #define _Py_DEC_MINALLOC 4

    typedef struct {
        PyObject_HEAD
        Py_hash_t hash;
        mpd_t dec;
        mpd_uint_t data[_Py_DEC_MINALLOC];
    } PyDecObject;

This is just a Python-level C container for an `mpd_t` and a static data block,
just like the C++ `decimal::Decimal` class is. These `PyObject*` instances are
accompanied by a standard dispatch table of well-known object methods (see the
associated `PyDec_Type` type object), understood by the Python interpreter and
manipulated like all the other native Python objects. This is what the C/Python
interpreter sees and what we receive when a C function is called with one of
those instances.


### Python: `Decimal` (Python class)

The Python library provides access to the `PyDecType` object above via the
following import:

    from decimal import Decimal

Instances of Python's `Decimal` are nothing more than instances of `PyDecType*`
within. However, because in prior versions of Python the `cdecimal` module was
optional, the Python module still packages a pure-Python fallback implementation
of the IEEE type. We plan never to use this implementation nor rely on it.


### Proto: `beancount.Number`

Because we represent of our internal data structures using protocol buffer
objects, we need a representation for decimal numbers that is efficient to
create and convert to/from the in-memory variants.

We define one as such:

    message Number {
      optional string exact = 1;
      optional MpdTriple triple = 2;
    }

    message MpdTriple {
      optional uint32 tag = 1;
      optional uint32 sign = 2;
      optional uint64 hi = 3;
      optional uint64 lo = 4;
      optional int64 exp = 5;
    }

The `MpdTriple` matches the definition of the `mpd_uint128_triple_t` C type and
can be efficiently converted to a the C equivalent. However, a further
conversion from that to the more powerful `mpd_t` type is still required.

The `Number` class is basically a variant, which supports string versions (for
readability) and the more efficient triple type.

Note: We plan to insert a conversion to an `mpd_t` eventually, for full
generality, and we want to benchmark it as well to see if it would be more
efficient to convert from this directly.


## Conversions
### Python to/from C/C++

Using `pybind11`, it is possible to convert from Python to C++ and vice-versa C
instances of `PyDecObject` structs to/from instances of `decimal::Decimal` C++
classes:

    template <> struct type_caster<decimal::Decimal> {
    public:
      PYBIND11_TYPE_CASTER(decimal::Decimal, const_name("Decimal"));

      // Convert Python PyObject to a C++ Decimal instance.
      // Return false upon failure.
      bool load(handle src, bool apply_implicit) {
        mpd_copy(value.get(), MPD(src.ptr()), decimal::context.get());
        return true;
      }

      // Convert a C++ Decimal instance to a Python PyObject.
      static handle cast(const decimal::Decimal& src,
                         return_value_policy /* policy */,
                         handle /* parent */) {
        py::object obj = Decimal();
        PyDecObject* dobj = reinterpret_cast<PyDecObject*>(obj.ptr());
        mpd_copy(MPD(dobj), src.getconst(), decimal::context.get());
        return obj;
      }
    };

*Performance Note* Note however, that this requires a copy of `mpd_t` and the
associated data block, with potentially dynamically allocating changes. For
methods which are likely to be called very frequently, we would do better accept
a `PyDecObject*` and use the `mpd_*()` API directly on the object, if possible
locally, to avoid dynamic allocation. `pybind11` should make that possible if
not trivial.

### Proto to/from C/C++

We either parse a string (slow) or internally plan to use a triple type to
represent our numbers efficiently. See //beancount/ccore:number.cc for details.
