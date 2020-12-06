// #define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "beancount/ccore/data.pb.h"
#include "beancount/ccore/number.pb.h"

#include "pybind11/pybind11.h"
#include "pybind11_protobuf/fast_cpp_proto_casters.h"

namespace beancount {
namespace py = pybind11;
using std::cout;
using std::endl;

}  // namespace beancount

PYBIND11_MODULE(expose_protos, mod) {
  mod.doc() = "Experiment to show how to expose protos via pybind11.";

  pybind11::google::ImportProtoModule();
  pybind11::google::RegisterProtoMessageType<beancount::Balance>(mod);
}
