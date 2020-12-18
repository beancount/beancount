#include "beancount/cparser/parser.h"
#include "beancount/cparser/ledger.h"
#include "beancount/ccore/data.pb.h"
#include "beancount/ccore/datapy.h"
#include "beancount/defs.h"

#include <memory>

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "datetime.h"

#include "pybind11/pybind11.h"
#include "pybind11/stl.h"

namespace beancount {
namespace py = pybind11;

using std::cout;
using std::cerr;
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

// TODO(blais): Deal with missing oneof for protobufs.
Directive::BodyCase GetDirectiveType(const Directive& dir) {
  return dir.body_case();
}

// Downgrade to V2 conversion for direct proto-to-proto comparison.
void DowngradeToV2(Directive* dir) {
  // Remove end line number in directive.
  dir->mutable_location()->clear_lineno_end();

  // Remove end line number in postings, if this is a transaction.
  auto body_case = dir->body_case();
  if (body_case == Directive::BodyCase::kTransaction) {
    auto* transaction = dir->mutable_transaction();
    for (auto& posting : *transaction->mutable_postings()) {
      posting.mutable_location()->clear_lineno_end();
    }
  }

  // Remove tags and links for directives that didn't use to support them.
  if (body_case != Directive::BodyCase::kTransaction &&
      body_case != Directive::BodyCase::kDocument) {
    dir->clear_tags();
    dir->clear_links();
  }

  // Sort tags.
  std::vector<string> tags;
  std::copy(dir->tags().begin(), dir->tags().end(), std::back_inserter(tags));
  std::sort(tags.begin(), tags.end());
  dir->clear_tags();
  for (auto tag : tags) dir->add_tags(tag);

  // Sort links.
  std::vector<string> links;
  std::copy(dir->links().begin(), dir->links().end(), std::back_inserter(links));
  std::sort(links.begin(), links.end());
  dir->clear_links();
  for (auto link : links) dir->add_links(link);
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
  ExportDataTypesToPython(mod);

  // Define some module-level conveniences.
  mod.def("GetDirectiveType", &GetDirectiveType);
  mod.def("DowngradeToV2", &DowngradeToV2);

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
