// Test module exporting proto types between C++ and Python.
// Using the proto_api.h, perhaps later pybind11_protobuf (with fast_cpp_protos).

#include "beancount/cparser/parser.h"
#include "beancount/cparser/ledger.h"
#include "beancount/ccore/data.pb.h"

#include "beancount/ccore/data.upbcpp.h"

#include "pybind11/pybind11.h"

namespace beancount {
namespace py = pybind11;

}  // namespace beancount

PYBIND11_MODULE(extmodule, mod) {
  mod.doc() = "Experiment to show how to expose protos via upb.";
  using namespace beancount;
}
