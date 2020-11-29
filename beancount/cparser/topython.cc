// Snippets of C++ to Python converters.
// Maybe use this later to build a conversion without exposing the objects.

#define PY_SSIZE_T_CLEAN
#include <Python.h>

// Default and only encoding we really have to bother supporting.
// TODO(blais): Revise this later, maybe make it part of the state of the builder.
static const char* kEncoding = "UTF-8";

// Convert string_view to Python string object.
inline PyObject* StringToPyString(const string_view s) {
  return PyUnicode_Decode(s.data(), s.size(), kEncoding, "ignore");
}

// Convert absl::CivilDay to Python date object.
inline PyObject* DateToPyDate(const absl::CivilDay& d) {
  return pydate_from_civil_day(d.year(), d.month(), d.day());
}

// Convert a decimal::Decimal to a PyDecimal.
inline PyObject* DecimalToPyDecimal(const decimal::Decimal& d) {
  return pydecimal_from_cstring(d.to_sci().c_str());
}

// Convert an Amount to a Python tuple like Amount.
// TODO(blais): This doesn't actually create a proper instance of `Amount`.
#define AmountToPyTuple(output, amount)                 \
  {                                                     \
    auto dec = ProtoToDecimal(amount->number());        \
    auto _s1 = DecimalToPyDecimal(dec);                 \
    auto _s2 = StringToPyString(amount->currency());    \
    BUILDY(DECREF(_s1, _s2),                            \
           output, "create_amount", "OO", _s1, _s2);    \
  }

// Create two sets from a TagsLinks object.
std::tuple<PyObject*, PyObject*> TagsLinksToPython(
  inter::TagsLinks* tags_links,
  const absl::flat_hash_set<string>* active_tags) {

  assert(tags_links != nullptr);
  PyObject* tags = PySet_New(0);
  for (const auto& tag : tags_links->tags()) {
    PySet_Add(tags, StringToPyString(tag));
  }
  PyObject* links = PySet_New(0);
  for (const auto& link : tags_links->links()) {
    PySet_Add(links, StringToPyString(link));
  }

  // Add the set of active tags if provided.
  if (active_tags != nullptr) {
    for (const auto& tag : *active_tags) {
      PySet_Add(tags, StringToPyString(tag));
    }
  }

  return {tags, links};
}

/*
 * Call a builder method and detect and handle a Python exception being raised
 * in the handler. Always run the code to clean the references provided by the
 * reduced rule. {05bb0fb60e86}
 */
#define BUILDY(clean, target, method_name, format, ...)                                         \
  {                                                                                             \
    auto loc = scanner.location();                                                              \
    /* cerr << "method_name: " << method_name << endl */                                        \
    /*     << "filename: " << *loc.begin.filename << ":" << loc.begin.line << endl; */          \
    target = PyObject_CallMethod(builder.builder(), method_name, "si" format,                   \
                                 loc.begin.filename->c_str(),                                   \
                                 loc.begin.line,                                                \
                                 ## __VA_ARGS__);                                               \
    clean;                                                                                      \
                                                                                                \
    if (target == NULL) {                                                                       \
      /* TODO(blais): This is for temporary debugging of internal errors while porting. */      \
      PyErr_PrintEx(1);                                                                         \
                                                                                                \
      build_grammar_error_from_exception(loc, builder.builder());                               \
      /* Figure out if this is stilil the right way to call for errors in the C++ parser. */    \
      YYERROR;                                                                                  \
    }                                                                                           \
  }

#define DECREF(...) _CC_FUNC(Py_DECREF, __VA_ARGS__)

#if 0
/* Build a grammar error from the exception context. */
void build_grammar_error_from_exception(const location& loc, PyObject* builder) {
  cerr << "build_grammar_error_from_exception " << loc << endl;
  assert(builder != nullptr);

  /* Get the exception context. */
  PyObject* ptype;
  PyObject* pvalue;
  PyObject* ptraceback;
  PyErr_Fetch(&ptype, &pvalue, &ptraceback);
  PyErr_NormalizeException(&ptype, &pvalue, &ptraceback);

  /* Clear the exception. */
  PyErr_Clear();

  if (pvalue != NULL) {
    /* Build and accumulate a new error object. {27d1d459c5cd} */
    PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "OiOOO",
                                       loc.begin.filename, loc.begin.line,
                                       pvalue, ptype, ptraceback);
    if (rv == NULL) {
      /* Note: Leave the internal error trickling up its detail. */
      /* PyErr_SetString(PyExc_RuntimeError, */
      /*                 "Internal error: While building exception"); */
    }
  }
  else {
    PyErr_SetString(PyExc_RuntimeError,
                    "Internal error: No exception");
  }

  Py_XDECREF(ptype);
  Py_XDECREF(pvalue);
  Py_XDECREF(ptraceback);
}
#endif


void yyerror(const location& loc, yyscan_t scanner, PyObject* builder, char const* message)
{
  /* Register a syntax error with the builder. */
  PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "Ois",
                                     loc->file_name, loc->first_line,
                                     message);
  if (rv == NULL) {
      PyErr_SetString(PyExc_RuntimeError,
                      "Internal error: Building exception from yyerror()");
  }
  Py_XDECREF(rv);
}
