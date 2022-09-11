// Test cross-language Decimal serialization.

#include <iostream>
#include <string>

#include "pybind11/pybind11.h"
#include "decimal.hh"

//,------------------------------------------------------------
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
}

//`------------------------------------------------------------

namespace beancount {
namespace py = pybind11;
using std::cout;
using std::endl;

void AcceptDecimal(const py::object* obj) {
  // const PyDecObject* dobj = obj->cast<PyDecObject*>();
  const PyObject* pyobj = obj->ptr();
  const PyDecObject* dobj = reinterpret_cast<const PyDecObject*>(pyobj);
  decimal::Decimal dec(mpd_as_uint128_triple(&dobj->dec));
  cout << "XXX " << dec << endl;
}

}  // namespace beancount

PYBIND11_MODULE(decimal_func, mod) {
  mod.doc() = "Module to test cross-language Decimal serialization.";
  using namespace beancount;

  // Export parsing function.
  mod.def("accept_decimal", &AcceptDecimal, "Read a Decimal instance from Python",
          py::arg("number"));
}
