// Test module exporting proto types between C++ and Python.
// Using pybind11_protobuf with `use_fast_cpp_protos` and just the public protobuf APIs.

#include "beancount/cparser/parser.h"
#include "beancount/cparser/ledger.h"
#include "beancount/ccore/data.pb.h"

#include "pybind11/pybind11.h"
#include "pybind11_protobuf/native_proto_caster.h"

#include <string>

namespace beancount {
namespace py = pybind11;
using std::string;

std::unique_ptr<Ledger> Parse(const string& filename, int lineno, const string& encoding) {
  // TODO(blais): The free() error is located within the parser.
  return parser::ParseFile(filename);
}

}  // namespace beancount


PYBIND11_MODULE(pbpb_protos, mod) {
  pybind11_protobuf::ImportNativeProtoCasters();

  mod.doc() = "Experiment to show how to expose protos via pybind11.";
  using namespace beancount;

  // Export parsing function.
  mod.def("parse", &Parse, "Parse an input file",
          py::arg("filename"),
          py::arg("lineno") = 1,
          py::arg("encoding") = "utf8");

  // // Export Ledger object.
  // py::class_<Ledger>(mod, "Ledger")
  //   .def("TestProtoConversion", &TestProtoConversion)
  //   .def("DebugPrint", &DebugPrint);
}
