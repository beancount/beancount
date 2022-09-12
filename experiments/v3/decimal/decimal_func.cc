// Test cross-language Decimal serialization.

#include <iostream>
#include <string>

#include "pybind11/pybind11.h"
#include "decimal.hh"

namespace py = pybind11;
using std::cout;
using std::endl;

//-------------------------------------------------------------------------------
// Copied from Python:Modules/_cdecimal/_decimal.c

extern "C" {
/* _Py_DEC_MINALLOC >= MPD_MINALLOC */
#define _Py_DEC_MINALLOC 4

typedef struct {
    PyObject_HEAD
    Py_hash_t hash;
    mpd_t dec;
    mpd_uint_t data[_Py_DEC_MINALLOC];
} PyDecObject;

static py::object Decimal;

#define MPD(v) (&((PyDecObject *)v)->dec)

}



//------------------------------------------------------------------------------
// Type caster

namespace pybind11 { namespace detail {

// Note: You could make a lightweight version of this which, like Decimal,
// includes just the `mpd_t` and a small associated data block. I'm not sure how
// much we'd save over the C++ Decimal wrapper since we're already just using
// the embedded `mpd_t` to copy our contents into it. Reusing the
// decimal::Decimal is likely as efficient as doing that though it requires a
// copy.
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

}} // namespace pybind11::detail

//------------------------------------------------------------------------------
// Implementations.

namespace beancount {

void AcceptDecimal(const py::object* obj) {
  const PyObject* pyobj = obj->ptr();
  const PyDecObject* dobj = reinterpret_cast<const PyDecObject*>(pyobj);
  decimal::Decimal dec(mpd_as_uint128_triple(&dobj->dec));
  cout << dec << endl;
}

void AcceptDecimalCastPtr(const decimal::Decimal* dec) {
  cout << *dec << endl;
}

void AcceptDecimalCastRef(const decimal::Decimal& dec) {
  cout << dec << endl;
}

decimal::Decimal TimesTwo(const decimal::Decimal& dec) {
  return dec * 2;
}

}  // namespace beancount

PYBIND11_MODULE(decimal_func, mod) {
  mod.doc() = "Module to test cross-language Decimal serialization.";
  using namespace beancount;

  // Get the global type constructor.
  Decimal = py::module_::import("decimal").attr("Decimal");

  // Export parsing function.
  mod.def("accept_decimal", &AcceptDecimal, "Read a Decimal instance from Python",
          py::arg("obj"));
  mod.def("accept_decimal_cast_ptr", &AcceptDecimalCastPtr, "Read a Decimal instance from Python",
          py::arg("dec"));
  mod.def("accept_decimal_cast_ref", &AcceptDecimalCastRef, "Read a Decimal instance from Python",
          py::arg("dec"));
  mod.def("times_two", &TimesTwo, "Multiply by two",
          py::arg("dec"));
}
