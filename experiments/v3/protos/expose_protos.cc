// Test module exporting proto types between C++ and Python.
// Using the proto_api.h, perhaps later pybind11_protobuf (with fast_cpp_protos).

#include "beancount/cparser/parser.h"
#include "beancount/cparser/ledger.h"
#include "beancount/ccore/data.pb.h"
#include "beancount/defs.h"

#include "pybind11/pybind11.h"
#include "python/google/protobuf/proto_api.h"

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

// Copied from fast_cpp_proto_casters.cc.
const PyProto_API& GetPyProtoApi() {
  static auto *result = [] {
    auto *r = static_cast<const PyProto_API*>(
      PyCapsule_Import(PyProtoAPICapsuleName(), 0));
    assert(r != nullptr);
    return r;
  }();

  return *result;
}

// Attempt to expose a Python-level list of directives via protobuf's C API for
// Python.
py::handle TestProtoConversion(const Ledger& ledger) {
  py::list dirlist;

  const auto& py_proto_api = GetPyProtoApi();
  for (auto* directive : ledger.directives) {
    cerr << ((directive->GetReflection()->GetMessageFactory() ==
              google::protobuf::MessageFactory::generated_factory()) ?
             "GENERATED" : "NOT_GENERATED") << endl;
    cerr << "GetDirectives - factory "
         << google::protobuf::MessageFactory::generated_factory() << endl;
    cerr << "PyProto_API::GetDefaultMessageFactory() "
         << py_proto_api.GetDefaultMessageFactory() << endl;

    py::handle obj = py_proto_api.NewMessageOwnedExternally(directive, nullptr);
    (void)obj;
    // dirlist.append(obj);
    // py::reinterpret_steal<py::object>(...));
    break;
  }
  cerr << "DONE" << endl;

  return std::move(dirlist);
}

}  // namespace beancount

PYBIND11_MODULE(expose_protos, mod) {
  mod.doc() = "Experiment to show how to expose protos via pybind11.";
  using namespace beancount;

  // Export parsing function.
  mod.def("parse", &Parse, "Parse an input file",
          py::arg("filename"),
          py::arg("lineno") = 1,
          py::arg("encoding") = "utf8");

  // Export Ledger object.
  py::class_<Ledger>(mod, "Ledger")
    .def("TestProtoConversion", &TestProtoConversion);
}
