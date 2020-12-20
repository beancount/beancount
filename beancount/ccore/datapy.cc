// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/datapy.h"
#include "beancount/ccore/data.pb.h"
#include "beancount/cparser/options.pb.h"

#include <string>

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "datetime.h"

#include "pybind11/pybind11.h"
//#include "pybind11/functional.h"
#include "pybind11/stl.h"

namespace beancount {
namespace py = pybind11;
using options::Options;
using options::ProcessingInfo;

// C++11 construct to for overload method resolution.
template<class... Args, class T, class R>
auto resolve(R (T::*m)(Args...)) -> decltype(m)
{ return m; }

template<class T, class R>
auto resolve(R (T::*m)(void)) -> decltype(m)
{ return m; }




// Convert a date proto to a date object.
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
void ExportDataTypesToPython(py::module& mod) {
  // TODO(blais): In order to replace missing oneof functionality.
  py::enum_<Directive::BodyCase>(mod, "BodyCase")
    .value("kTransaction", Directive::BodyCase::kTransaction)
    .value("kPrice", Directive::BodyCase::kPrice)
    .value("kBalance", Directive::BodyCase::kBalance)
    .value("kOpen", Directive::BodyCase::kOpen)
    .value("kClose", Directive::BodyCase::kClose)
    .value("kCommodity", Directive::BodyCase::kCommodity)
    .value("kPad", Directive::BodyCase::kPad)
    .value("kDocument", Directive::BodyCase::kDocument)
    .value("kNote", Directive::BodyCase::kNote)
    .value("kEvent", Directive::BodyCase::kEvent)
    .value("kQuery", Directive::BodyCase::kQuery)
    .value("kCustom", Directive::BodyCase::kCustom)
    .value("BODY_NOT_SET", Directive::BodyCase::BODY_NOT_SET)
    .export_values()
    ;

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
    .def_property("filename", &Location::filename,
                  resolve<const std::string&>(&Location::set_filename))
    .def_property("lineno", &Location::lineno, &Location::set_lineno)
    .def_property("lineno_end", &Location::lineno_end, &Location::set_lineno_end)
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

  py::class_<Price>(mod, "Price");
  py::class_<Balance>(mod, "Balance");
  py::class_<Open>(mod, "Open");
  py::class_<Close>(mod, "Close");
  py::class_<Commodity>(mod, "Commodity");
  py::class_<Pad>(mod, "Pad");
  py::class_<Document>(mod, "Document");
  py::class_<Note>(mod, "Note");
  py::class_<Event>(mod, "Event");
  py::class_<Query>(mod, "Query");
  py::class_<Custom>(mod, "Custom");

  // TODO(blais): Complete this. Perhaps auto-generate.
}

}  // namespace beancount
