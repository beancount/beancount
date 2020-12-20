// Test module exporting proto types between C++ and Python.
// Using the proto_api.h, perhaps later pybind11_protobuf (with fast_cpp_protos).

#include "beancount/cparser/parser.h"
#include "beancount/cparser/ledger.h"
#include "beancount/ccore/data.pb.h"
#include "beancount/defs.h"

#include "pybind11/pybind11.h"
#include "python/google/protobuf/proto_api.h"
#include "absl/time/clock.h"

// The first method is to use the capsule created and provided by _message.so to
// try to call the Proto API to create objects. However, this does not work,
// because there are two distinct factories: one in this extension module, and
// one in _message.so.
//
// The second approach attempts to link pyext/_message.so in this extension
// module, so that there is a single generated pool.
//
// Defining the following enables the second approach (+ BUILD changes).
#define LINK_STATIC_PYEXT

namespace beancount {
namespace py = pybind11;
using std::cerr;
using std::endl;
using google::protobuf::python::PyProto_API;
using google::protobuf::python::PyProtoAPICapsuleName;

std::unique_ptr<Ledger> Parse(const string& filename, int lineno, const string& encoding) {
  // TODO(blais): lineno? Encoding?
  return parser::ParseFile(filename);
}

#ifndef LINK_STATIC_PYEXT

// Copied from fast_cpp_proto_casters.cc.
const PyProto_API& GetPyProtoApi() {
  static auto* result = [] {
    auto* r = static_cast<const PyProto_API*>(
      PyCapsule_Import(PyProtoAPICapsuleName(), 0));
    assert(r != nullptr);
    return r;
  }();

  return *result;
}

#else

// Return a named attribute on this module.
PyObject* GetModuleAttribute(const char* attr_name) {
  // Get the module object for this module.
  PyObject* module_name = PyUnicode_FromString("experiments.v3.protos.expose_protos");
  PyObject* module = PyImport_GetModule(module_name);
  Py_DECREF(module_name);
  assert(module);

  // Retrieve the attribute value.
  PyObject* value = PyDict_GetItemString(PyModule_GetDict(module), attr_name);
  Py_DECREF(module);
  assert(value);

  return value;
}

const PyProto_API& GetPyProtoApi() {
  static PyProto_API* result = []() -> PyProto_API* {

    // Retrieve the _message module object from the local module attribute.
    PyObject* message_module = GetModuleAttribute("_message_module");
    assert(message_module);

    const char* attr_name = "proto_API";
    PyObject* capsule = PyDict_GetItemString(PyModule_GetDict(message_module), attr_name);
    if (!capsule) {
      PyErr_Format(PyExc_AttributeError,
                   "Module '%s' has no attibute '%s'.",
                   PyModule_GetName(message_module), attr_name
      );
      return nullptr;
    }
    Py_DECREF(message_module);

    return (PyProto_API*)PyCapsule_GetPointer(
        capsule, google::protobuf::python::PyProtoAPICapsuleName());
  } ();
  return *result;
}

#endif


// Attempt to expose a Python-level list of directives via protobuf's C API for
// Python.
py::list TestProtoConversion(const Ledger& ledger) {
  py::list dirlist;
  const auto& py_proto_api = GetPyProtoApi();
  absl::Time t1 = absl::Now();
  for (auto* directive : ledger.directives) {
    dirlist.append(py_proto_api.NewMessageOwnedExternally(directive, nullptr));
  }
  absl::Time t2 = absl::Now(); // ~20-25ms
  std::cerr << "TestProtoConversion time=" << t2 - t1 << std::endl;
  return dirlist;
}

void DebugPrint(const Ledger& ledger) {
  for (auto* directive : ledger.directives) {
    std::cout << directive->DebugString() << std::endl;
    break;
  }
}

}  // namespace beancount

// From message_module.cc.
extern "C" { PyObject* PyInit__message(); }

PYBIND11_MODULE(expose_protos, mod) {
  mod.doc() = "Experiment to show how to expose protos via pybind11.";
  using namespace beancount;

#ifdef LINK_STATIC_PYEXT
  // Create and initialize the embedded pyext/_message.so module object. This
  // creates the API within. Save the module for later (on an attribute, for the
  // destruction sequence).
  // See PyInit__message() in _message.so.
  PyObject* message_module = PyInit__message();
  assert(message_module != nullptr);
  mod.add_object("_message_module", message_module);
#endif

  // Export parsing function.
  mod.def("parse", &Parse, "Parse an input file",
          py::arg("filename"),
          py::arg("lineno") = 1,
          py::arg("encoding") = "utf8");

  // Export Ledger object.
  py::class_<Ledger>(mod, "Ledger")
    .def("TestProtoConversion", &TestProtoConversion)
    .def("DebugPrint", &DebugPrint);
}
