#include "beancount/cparser/parser.h"
#include "beancount/cparser/scanner.h"
#include "beancount/ccore/data.pb.h"
#include "beancount/defs.h"

#include <memory>

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "datetime.h"

#include "pybind11/pybind11.h"
#include "pybind11/stl.h"

namespace beancount {
namespace py = pybind11;
using options::Options;
using options::ProcessingInfo;

using std::cout;
using std::endl;

// A reference to the placeholder object for missing cost specifications.
// TODO(blais): You won't need this eventually, this to be defined in C++?
//
// Its reference is leaked; we could clean this up if desired; see
// https://pybind11.readthedocs.io/en/stable/advanced/misc.html#module-destructors
PyObject* missing_obj;

void Lex(const string& filename, int lineno, const string& encoding) {
  cout << "XXX" << filename << " " << lineno << " " << encoding << endl;
}

std::unique_ptr<Ledger> Parse(const string& filename, int lineno, const string& encoding) {
  // TODO(blais): lineno? Encoding?
  return parser::ParseFile(filename);
}

// std::unique_ptr<proto::Database> parse_string(const std::string& input_string) {
//   static const std::string filename = "<python-string>";
//   return ParseString(input_string, filename);
// }

// std::unique_ptr<proto::Database> parse_file(const std::string& filename) {
//   return ParseFile(filename);
// }

// std::unique_ptr<proto::Database> parse_stdin() {
//   return ParseStdin();
// }

template <typename T>
py::object GetDate(const T& parent) {
  const auto& date = parent.date();
  PyObject* py_date = PyDate_FromDate(date.year(), date.month(), date.day());
  return py::reinterpret_steal<py::object>(py_date);
}

// Explicit interface to protobuf schema.
//
// For a more complete and read/write setup, it'll be wiser to complete the
// 'pybind11_protobuf' project, which does this using C++ metaprogramming
// techniques. However, this project is still burgeoning and needs some active
// involvement in order to debug and complete.
void ExportProtoTypes(py::module& mod) {
  py::class_<Directive>(mod, "Directive")
    .def("__str__", &Directive::DebugString)
    .def_property_readonly("location", &Directive::location)
    .def_property_readonly("date", &GetDate<Directive>)
    .def_property_readonly("meta", &Directive::meta)
    // oneof
    .def_property_readonly("transaction", &Directive::transaction)
    .def_property_readonly("price", &Directive::price)
    .def_property_readonly("balance", &Directive::balance)
    .def_property_readonly("open", &Directive::open)
    .def_property_readonly("close", &Directive::close)
    .def_property_readonly("commodity", &Directive::commodity)
    .def_property_readonly("pad", &Directive::pad)
    .def_property_readonly("document", &Directive::document)
    .def_property_readonly("note", &Directive::note)
    .def_property_readonly("event", &Directive::event)
    .def_property_readonly("query", &Directive::query)
    .def_property_readonly("custom", &Directive::custom)
    ;
    // Uh-oh, repeated fields will require a custom class.
    // See RepeatedFieldContainer from pybind11_protobuf's proto_utils.cc
    // .def_property_readonly("tags", &Directive::tags)
    // .def_property_readonly("links", &Directive::links)

  py::class_<Error>(mod, "Error")
    .def("__str__", &Error::DebugString)
    .def_property_readonly("message", &Error::message)
    .def_property_readonly("location", &Error::location)
    .def_property_readonly("dirhash", &Error::dirhash)
    ;

  py::class_<Options, std::shared_ptr<Options>>(mod, "Options")
    .def("__str__", &Error::DebugString)
    ;

  py::class_<ProcessingInfo, std::shared_ptr<ProcessingInfo>>(mod, "ProcessingInfo")
    .def("__str__", &Error::DebugString)
    ;

  py::class_<Location>(mod, "Location")
    .def("__str__", &Directive::DebugString)
    .def_property_readonly("filename", &Location::filename)
    .def_property_readonly("lineno", &Location::lineno)
    .def_property_readonly("lineno_end", &Location::lineno_end)
    ;

  py::class_<Transaction>(mod, "Transaction")
    .def("__str__", &Transaction::DebugString)
    .def_property_readonly("flag", &Transaction::flag)
    .def_property_readonly("payee", &Transaction::payee)
    .def_property_readonly("narration", &Transaction::narration)
    // Uh-oh... repeated field.
    // .def_property_readonly("postings", &Transaction::postings)
    ;

  py::class_<Posting>(mod, "Posting")
    .def("__str__", &Posting::DebugString)
    .def_property_readonly("location", &Posting::location)
    .def_property_readonly("meta", &Posting::meta)
    .def_property_readonly("date", &GetDate<Posting>)
    .def_property_readonly("flag", &Posting::flag)
    .def_property_readonly("account", &Posting::account)
    .def_property_readonly("units", &Posting::units)
    .def_property_readonly("cost", &Posting::cost)
    .def_property_readonly("price", &Posting::price)
    ;

  // TODO(blais): Complete this. Perhaps auto-generate.
}

}  // namespace beancount


PYBIND11_MODULE(extmodule, mod) {
  mod.doc() = "Beancount parser extension module (v3).";

  // Lazy initialise the PyDateTime import.
  if (!PyDateTimeAPI) {
    PyDateTime_IMPORT;
  }

  using namespace beancount;
  namespace py = pybind11;

  // Fetch the MISSING object and steal a reference to a global for later use.
  py::module_ number = py::module_::import("beancount.core.number");
  missing_obj = ((py::object) number.attr("MISSING")).inc_ref().ptr();

  // Note: We never need a PARSER_SOURCE_HASH bound to "SOURCE_HASH" in the
  // Bazel build.
  // TODO(blais): Remove this?
  mod.attr("SOURCE_HASH") = Py_None;

  // TODO(blais): Set the version properly. There's an example in TF (?) on how
  // to do this properly in Bazel.
  mod.attr("__version__") = "3.0.0-dev";

  // TODO(blais): Can we remove these?
  mod.attr("__vc_changeset__") = "N/A";
  mod.attr("__vc_timestamp__") = "N/A";

  // Export the scanner entry point.
  mod.def("lex", &Lex, R"(
Run the input file object trough the Beancount tokenizer. `filename` and
`lineno` keyword arguments allow to specify the file name and start line
number to be used in error reporting. If filename is not specified or
None, the name attribute of the file object is used, if present.
The `encoding` parameter allows to specify the file encoding. Return an
iterable yielding (token name, string value, sematical value) tuples.
  )",
          py::arg("filename"),
          py::arg("lineno") = 1,
          py::arg("encoding") = "utf8");

  // Export the parser entry points.
  // TODO(blais): Support an interface that's a bit more flexible, like this:
  // mod.def("parse_string", &parse_string, "Parse a language string");
  // mod.def("parse_file", &parse_file, "Parse a language file");
  // mod.def("parse_stdin", &parse_stdin, "Parse an language from stdin");
  mod.def("parse", &Parse, R"(
Parse input from file object. The filename and lineno keyword
arguments allow to specify the file name and start line number to be
used in error reporting and in the returned metadata objects. If
filename is not specified or None, the name attribute of the file
object is used, if present. The encoding parameter allows to specify
the file encoding. Parsing results are retrieved from the Builder
object specified when the Parser object was instantiated.");
  )",
          py::arg("filename"),
          py::arg("lineno") = 1,
          py::arg("encoding") = "utf8");

  // Expose all the protobuf message types.
  ExportProtoTypes(mod);

  // Export the ultimate result of the parser.
  py::class_<Ledger>(mod, "Ledger")
    // .def(py::init<>())
    .def_readonly("directives", &Ledger::directives)
    .def_readonly("errors", &Ledger::errors)
    .def_readonly("options", &Ledger::options)
    .def_readonly("info", &Ledger::info);
    ;

  // Output the contents of a parsed ledger to a text-formatted file. This will
  // be used for cross-checking parsed output with that from the Python parser.
  mod.def("write_to_text", &WriteToText);
}
