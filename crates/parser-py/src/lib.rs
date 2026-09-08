#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
#![deny(clippy::unwrap_used, clippy::expect_used)]

use beancount_core as core;
use beancount_parser::ParseError;
use beancount_parser::ast;
use beancount_parser::parse_lossy;
use core::Directive;
use core::normalize_directives;
use jiff::civil::Date;
use pyo3::IntoPyObject;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyDate, PyDict, PyFrozenSet, PyList, PyModule, PyString, PyTuple};
use rust_decimal::Decimal;
use std::collections::{BTreeMap, HashMap};
use std::str::FromStr;

mod data;
use data::Booking;

#[pymodule]
fn _parser_rust(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
  const VERSION: &str = env!("CARGO_PKG_VERSION");

  m.add("__version__", VERSION)?;
  m.add_class::<Booking>()?;
  m.add_function(wrap_pyfunction!(load_file, m)?)?;
  m.add_function(wrap_pyfunction!(parse_string, m)?)?;
  m.add_function(wrap_pyfunction!(load_file_and_book, m)?)?;
  m.add_function(wrap_pyfunction!(load_string_and_book, m)?)?;
  m.add_function(wrap_pyfunction!(check_file_rust, m)?)?;
  m.add_class::<PyParserError>()?;
  Ok(())
}

/// Rust-only recursive load + booking that avoids converting directives into Python.
///
/// This exists purely for performance comparisons. It does NOT run the Python
/// transformations/plugins pipeline nor validation.
///
/// Returns (entries_count, load_errors, parse_errors, booking_errors).
#[pyfunction]
pub fn check_file_rust(
  py: Python<'_>,
  filename: &str,
) -> PyResult<(usize, usize, usize, usize)> {
  py.detach(|| {
    let config = core::booking::BookingConfig::default();

    let load = core::loader::load_file_recursive(filename).map_err(|err| {
      PyValueError::new_err(format!("failed to read {}: {}", filename, err))
    })?;

    let load_errors = load.load_errors.len();
    let parse_errors = load.parse_errors.len();

    // Avoid cloning directives: we don't need to return them to Python.
    let directives = load.directives;
    let (booked, booking_errors_vec) = core::booking::book_directives(directives, &config);

    Ok((
      booked.len(),
      load_errors,
      parse_errors,
      booking_errors_vec.len(),
    ))
  })
}

/// Load a file and run booking (parse + normalize + options + booking).
///
/// This is intended to replace the Python loader's recursive parse + booking step.
/// The loader will still run decryption and transformations in Python.
///
/// NOTE: Booking is still a scaffold (currently a no-op).
#[pyfunction]
pub fn load_file_and_book(
  py: Python<'_>,
  filename: &str,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
  load_recursive_and_book(py, filename, Some(filename), None)
}

/// Load a string and run booking (parse + normalize + options + booking).
///
/// NOTE: Scaffold implementation: parses only and returns (entries, errors, options_map).
#[pyfunction]
#[pyo3(signature = (content, filename = "<string>"))]
pub fn load_string_and_book(
  py: Python<'_>,
  content: &str,
  filename: &str,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
  load_recursive_and_book(py, filename, None, Some(content))
}

/// Parser error exposed to Python. Matches beancount.core.data.BeancountError protocol.
#[pyclass(module = "beancount.parser.parser", name = "ParserError", get_all)]
struct PyParserError {
  pub source: Py<PyAny>,
  pub message: String,
  pub entry: Option<Py<PyAny>>,
}

#[pymethods]
impl PyParserError {
  #[new]
  fn new(source: Py<PyAny>, message: String, entry: Option<Py<PyAny>>) -> Self {
    Self {
      source,
      message,
      entry,
    }
  }

  fn __str__(&self) -> PyResult<String> {
    Ok(self.message.clone())
  }

  fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
    let source_repr = self.source.bind(py).repr()?.extract::<String>()?;
    let entry_repr = match &self.entry {
      Some(entry) => entry.bind(py).repr()?.extract::<String>()?,
      None => "None".to_string(),
    };

    Ok(format!(
      "ParserError(source={}, message={:?}, entry={})",
      source_repr, self.message, entry_repr
    ))
  }
}

/// Global cache for imported modules and classes so we don't look them up repeatedly.
static DATA_CACHE: PyOnceLock<DataCache> = PyOnceLock::new();

struct OptionDescriptor {
  alias: Option<String>,
  converter: Option<Py<PyAny>>,
}

struct DataCache {
  // True
  py_true: Py<PyAny>,
  // False
  py_false: Py<PyAny>,

  // copy.deepcopy function.
  deepcopy_fn: Py<PyAny>,
  // beancount.core.amount.Amount constructor.
  amount_ctor: Py<PyAny>,
  // beancount.core.data.new_metadata callable.
  new_metadata: Py<PyAny>,
  // beancount.core.data.Open class.
  open_cls: Py<PyAny>,
  // beancount.core.data.Close class.
  close_cls: Py<PyAny>,
  // beancount.core.data.Balance class.
  balance_cls: Py<PyAny>,
  // beancount.core.data.Pad class.
  pad_cls: Py<PyAny>,
  // beancount.core.data.Transaction class.
  transaction_cls: Py<PyAny>,
  // beancount.core.data.Posting class.
  posting_cls: Py<PyAny>,
  // beancount.core.data.Commodity class.
  commodity_cls: Py<PyAny>,
  // beancount.core.data.Price class.
  price_cls: Py<PyAny>,
  // beancount.core.data.Event class.
  event_cls: Py<PyAny>,
  // beancount.core.data.Query class.
  query_cls: Py<PyAny>,
  // beancount.core.data.Note class.
  note_cls: Py<PyAny>,
  // beancount.core.data.Document class.
  document_cls: Py<PyAny>,
  // beancount.core.data.Custom class.
  custom_cls: Py<PyAny>,
  // beancount.core.data.Booking class and cached enum instances.
  booking_cls: Py<PyAny>,
  booking_strict: Py<PyAny>,
  booking_strict_with_size: Py<PyAny>,
  booking_none: Py<PyAny>,
  booking_average: Py<PyAny>,
  booking_fifo: Py<PyAny>,
  booking_lifo: Py<PyAny>,
  booking_hifo: Py<PyAny>,
  // beancount.core.position.CostSpec class.
  cost_spec_cls: Py<PyAny>,
  // beancount.core.position.Cost class.
  cost_cls: Py<PyAny>,
  // beancount.parser.grammar.ValueType class.
  value_type_cls: Py<PyAny>,
  // Python bool type object.
  bool_type: Py<PyAny>,
  // Python str type object.
  str_type: Py<PyAny>,
  // datetime.date type.
  date_type: Py<PyAny>,
  // decimal.Decimal type.
  decimal_type: Py<PyAny>,
  // beancount.core.account.TYPE token.
  account_type_token: Py<PyAny>,
  // beancount.parser.options.OPTIONS_DEFAULTS mapping.
  options_defaults: Py<PyAny>,
  // beancount.parser.options.OPTIONS mapping.
  options_defs: Py<PyAny>,
  // parsed option descriptors derived from OPTIONS.
  option_descriptors: HashMap<String, OptionDescriptor>,
  // beancount.parser.options.READ_ONLY_OPTIONS set.
  read_only_options: Py<PyAny>,
  // beancount.core.number.MISSING sentinel.
  missing: Py<PyAny>,
  // beancount.core.number.ZERO sentinel.
  zero: Py<PyAny>,

  // beancount.loader.LoadError class.
  load_error_cls: Py<PyAny>,
  // beancount.loader.aggregate_options_map callable.
  aggregate_options_map_fn: Py<PyAny>,

  // beancount.parser.booking_full.InterpolationError class.
  interpolation_error_cls: Py<PyAny>,

  // beancount.parser.booking_full.ReductionError class.
  reduction_error_cls: Py<PyAny>,
}

impl DataCache {
  fn init(py: Python<'_>) -> PyResult<Self> {
    let data_mod = py.import("beancount.core.data")?;
    let builtins_mod = py.import("builtins")?;
    let datetime_mod = py.import("datetime")?;
    let decimal_mod = py.import("decimal")?;
    let number_mod = py.import("beancount.core.number")?;
    let amount_mod = py.import("beancount.core.amount")?;
    let account_mod = py.import("beancount.core.account")?;
    let position_mod = py.import("beancount.core.position")?;
    let options_mod = py.import("beancount.parser.options")?;
    let grammar_mod = py.import("beancount.parser.grammar")?;
    let copy_mod = py.import("copy")?;
    let loader_mod = py.import("beancount.loader")?;
    let booking_full_mod = py.import("beancount.parser.booking_full")?;

    let bool_type: Py<PyAny> = builtins_mod.getattr("bool")?.unbind();
    let str_type: Py<PyAny> = builtins_mod.getattr("str")?.unbind();
    let date_type: Py<PyAny> = datetime_mod.getattr("date")?.unbind();
    let decimal_type: Py<PyAny> = decimal_mod.getattr("Decimal")?.unbind();

    let missing = number_mod.getattr("MISSING")?.unbind();
    let zero = number_mod.getattr("ZERO")?.unbind();
    let cost_spec_cls = position_mod.getattr("CostSpec")?.unbind();
    let cost_cls = position_mod.getattr("Cost")?.unbind();
    let options_defs = options_mod.getattr("OPTIONS")?.unbind();
    let option_descriptors = parse_option_descriptors(py, &options_defs)?;
    let deepcopy_fn = copy_mod.getattr("deepcopy")?.unbind();
    let amount_ctor = amount_mod.getattr("Amount")?.unbind();
    let open_cls = data_mod.getattr("Open")?.unbind();
    let booking_cls = data_mod.getattr("Booking")?.unbind();
    let booking_cls_bound = booking_cls.bind(py);
    let booking_strict = booking_cls_bound.getattr("STRICT")?.unbind();
    let booking_strict_with_size = booking_cls_bound.getattr("STRICT_WITH_SIZE")?.unbind();
    let booking_none = booking_cls_bound.getattr("NONE")?.unbind();
    let booking_average = booking_cls_bound.getattr("AVERAGE")?.unbind();
    let booking_fifo = booking_cls_bound.getattr("FIFO")?.unbind();
    let booking_lifo = booking_cls_bound.getattr("LIFO")?.unbind();
    let booking_hifo = booking_cls_bound.getattr("HIFO")?.unbind();

    Ok(Self {
      py_true: builtins_mod.getattr("True")?.unbind(),
      py_false: builtins_mod.getattr("False")?.unbind(),
      new_metadata: data_mod.getattr("new_metadata")?.unbind(),
      open_cls,
      close_cls: data_mod.getattr("Close")?.unbind(),
      balance_cls: data_mod.getattr("Balance")?.unbind(),
      pad_cls: data_mod.getattr("Pad")?.unbind(),
      transaction_cls: data_mod.getattr("Transaction")?.unbind(),
      posting_cls: data_mod.getattr("Posting")?.unbind(),
      commodity_cls: data_mod.getattr("Commodity")?.unbind(),
      price_cls: data_mod.getattr("Price")?.unbind(),
      event_cls: data_mod.getattr("Event")?.unbind(),
      query_cls: data_mod.getattr("Query")?.unbind(),
      note_cls: data_mod.getattr("Note")?.unbind(),
      document_cls: data_mod.getattr("Document")?.unbind(),
      custom_cls: data_mod.getattr("Custom")?.unbind(),
      booking_cls,
      booking_strict,
      booking_strict_with_size,
      booking_none,
      booking_average,
      booking_fifo,
      booking_lifo,
      booking_hifo,
      amount_ctor,
      deepcopy_fn,
      cost_spec_cls,
      cost_cls,
      value_type_cls: grammar_mod.getattr("ValueType")?.unbind(),
      account_type_token: account_mod.getattr("TYPE")?.unbind(),
      options_defaults: options_mod.getattr("OPTIONS_DEFAULTS")?.unbind(),
      options_defs,
      option_descriptors,
      read_only_options: options_mod.getattr("READ_ONLY_OPTIONS")?.unbind(),
      bool_type,
      str_type,
      date_type,
      decimal_type,
      missing,
      zero,

      load_error_cls: loader_mod.getattr("LoadError")?.unbind(),
      aggregate_options_map_fn: loader_mod.getattr("aggregate_options_map")?.unbind(),

      interpolation_error_cls: booking_full_mod.getattr("InterpolationError")?.unbind(),

      reduction_error_cls: booking_full_mod.getattr("ReductionError")?.unbind(),
    })
  }
}

fn build_load_error(py: Python<'_>, message: String) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let meta = cache.new_metadata.call1(py, ("<load>", 0))?;
  cache.load_error_cls.call1(py, (meta, message))
}

fn build_interpolation_error(
  py: Python<'_>,
  meta_filename: &str,
  meta_lineno: usize,
  message: String,
  entry: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let meta = cache.new_metadata.call1(py, (meta_filename, meta_lineno))?;
  cache
    .interpolation_error_cls
    .call1(py, (meta, message, entry))
}

fn build_reduction_error(
  py: Python<'_>,
  meta_filename: &str,
  meta_lineno: usize,
  message: String,
  entry: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let meta = cache.new_metadata.call1(py, (meta_filename, meta_lineno))?;
  cache.reduction_error_cls.call1(py, (meta, message, entry))
}
fn load_recursive_and_book(
  py: Python<'_>,
  top_filename: &str,
  file_source: Option<&str>,
  string_source: Option<&str>,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
  let cache = cache(py)?;

  // Core crate performs recursive loading; we run booking after aggregating
  // options_map so booking can depend on options.
  let load_result = if let Some(fname) = file_source {
    core::loader::load_file_recursive(fname)
      .map_err(|err| PyValueError::new_err(format!("failed to read {}: {}", fname, err)))?
  } else {
    core::loader::load_string_recursive(string_source.unwrap_or(""), top_filename)
  };

  // Destructure so we can move out large fields (directives) without cloning.
  let core::loader::LoadResult {
    units,
    directives,
    parse_errors: _,
    load_errors,
    filenames_seen,
  } = load_result;

  // Build per-unit options maps (so we can aggregate like Python does).
  let mut top_options_map: Option<Py<PyAny>> = None;
  let mut other_options_maps: Vec<Py<PyAny>> = Vec::new();
  let mut all_errors: Vec<Py<PyAny>> = Vec::new();

  for unit in &units {
    let (omap, mut errs) = build_options_map_for_unit(py, unit)?;
    all_errors.append(&mut errs);
    if top_options_map.is_none() {
      top_options_map = Some(omap);
    } else {
      other_options_maps.push(omap);
    }
  }

  // Convert per-unit parse errors with correct filenames.
  for unit in &units {
    for err in &unit.errors {
      all_errors.push(build_parser_error(py, err.clone(), unit.filename.as_str())?);
    }
  }
  for err in &load_errors {
    all_errors.push(build_load_error(py, err.message.clone())?);
  }

  let options_map = match top_options_map {
    Some(omap) => omap,
    None => {
      let omap = default_options_map(py)?;
      omap.set_item("filename", "<load>")?;
      omap.unbind().into()
    }
  };

  // options_map["include"] = sorted(filenames_seen)
  options_map
    .bind(py)
    .cast::<PyDict>()?
    .set_item("include", PyList::new(py, &filenames_seen)?)?;

  // options_map = aggregate_options_map(options_map, other_options_maps)
  let other_list = PyList::new(py, other_options_maps)?;
  let options_map = cache
    .aggregate_options_map_fn
    .call1(py, (options_map, other_list))?;

  // Run Rust booking using config derived from aggregated options.
  let booking_config = booking_config_from_options_map(py, options_map.bind(py))?;
  let (booked, booking_errors) =
    core::booking::book_directives(directives, &booking_config);

  let dcontext = options_map
    .bind(py)
    .cast::<PyDict>()?
    .get_item("dcontext")?
    .ok_or_else(|| PyValueError::new_err("dcontext option missing from defaults"))?;

  // Convert Rust booking errors into Python InterpolationError objects.
  for berr in &booking_errors {
    let filename = berr.meta.filename.as_str();
    let lineno = berr.meta.line;
    let entry_obj: Py<PyAny> = match berr.entry.as_ref() {
      Some(txn) => convert_transaction(py, txn, &dcontext)?,
      None => py.None(),
    };
    match berr.kind {
      core::booking::BookingErrorKind::ReductionNoMatch => {
        all_errors.push(build_reduction_error(
          py,
          filename,
          lineno,
          berr.message.clone(),
          entry_obj,
        )?);
      }
      _ => {
        all_errors.push(build_interpolation_error(
          py,
          filename,
          lineno,
          berr.message.clone(),
          entry_obj,
        )?);
      }
    }
  }

  let (entries_vec, _tag_errors) = convert_directives(py, booked, top_filename, &dcontext)?;
  let entries = PyList::new(py, entries_vec)?.unbind().into();
  let errors: Py<PyAny> = PyList::new(py, all_errors)?.unbind().into();
  Ok((entries, errors, options_map))
}

fn py_decimal_to_rust(value: &Bound<'_, PyAny>) -> PyResult<Decimal> {
  let s: String = value.str()?.extract()?;
  Decimal::from_str(&s)
    .map_err(|err| PyValueError::new_err(format!("invalid Decimal '{}': {}", s, err)))
}

fn booking_config_from_options_map(
  _py: Python<'_>,
  options_map: &Bound<'_, PyAny>,
) -> PyResult<core::booking::BookingConfig> {
  let omap = options_map.cast::<PyDict>()?;

  let infer_any = omap
    .get_item("infer_tolerance_from_cost")?
    .ok_or_else(|| PyValueError::new_err("infer_tolerance_from_cost option missing"))?;

  let infer_tolerance_from_cost: bool = infer_any.extract().or_else(|_| {
    let s: String = infer_any.extract()?;
    let normalized = s.trim().to_ascii_lowercase();
    Ok::<bool, PyErr>(matches!(
      normalized.as_str(),
      "true" | "t" | "1" | "yes" | "y"
    ))
  })?;

  let tolerance_multiplier_any = omap
    .get_item("tolerance_multiplier")?
    .ok_or_else(|| PyValueError::new_err("tolerance_multiplier option missing"))?;
  let tolerance_multiplier = py_decimal_to_rust(&tolerance_multiplier_any)?;

  let inferred_default_any = omap
    .get_item("inferred_tolerance_default")?
    .ok_or_else(|| PyValueError::new_err("inferred_tolerance_default option missing"))?;

  let mut inferred_tolerance_default: BTreeMap<String, Decimal> = BTreeMap::new();
  if !inferred_default_any.is_none() {
    let items_any = inferred_default_any.call_method0("items")?;
    for pair in items_any.try_iter()? {
      let pair = pair?;
      let pair = pair.cast::<PyTuple>()?;
      let key: String = pair.get_item(0)?.extract()?;
      let val_any = pair.get_item(1)?;
      let val = py_decimal_to_rust(&val_any)?;
      inferred_tolerance_default.insert(key, val);
    }
  }

  Ok(core::booking::BookingConfig {
    infer_tolerance_from_cost,
    tolerance_multiplier,
    inferred_tolerance_default,
  })
}

fn build_options_map_for_unit(
  py: Python<'_>,
  unit: &core::ParsedUnit,
) -> PyResult<(Py<PyAny>, Vec<Py<PyAny>>)> {
  let options_map = default_options_map(py)?;
  options_map.set_item("filename", &unit.filename)?;
  options_map.set_item("include", PyList::new(py, &unit.includes)?)?;

  let errors: Vec<Py<PyAny>> = apply_options(py, &options_map, &unit.options)?;

  if !unit.plugins.is_empty() {
    let plugin_list_obj = options_map
      .get_item("plugin")?
      .ok_or_else(|| PyValueError::new_err("plugin option missing from defaults"))?
      .unbind();
    let plugin_list = plugin_list_obj.bind(py).cast::<PyList>()?;
    for plugin in &unit.plugins {
      let name = PyString::new(py, &plugin.name).unbind().into();
      let config = plugin
        .config
        .as_deref()
        .map(|c| PyString::new(py, c).unbind().into())
        .unwrap_or_else(|| py.None());
      let tuple = PyTuple::new(py, [name, config])?;
      plugin_list.append(tuple)?;
    }
  }

  apply_display_context_options(py, &options_map)?;
  Ok((options_map.unbind().into(), errors))
}

fn cache(py: Python<'_>) -> PyResult<&'static DataCache> {
  DATA_CACHE.get_or_try_init(py, || DataCache::init(py))
}

fn booking_to_native(booking: Option<&str>) -> PyResult<Option<Booking>> {
  match booking {
    Some("STRICT") => Ok(Some(Booking::STRICT)),
    Some("STRICT_WITH_SIZE") => Ok(Some(Booking::STRICT_WITH_SIZE)),
    Some("NONE") | Some("None") => Ok(Some(Booking::None)),
    Some("AVERAGE") => Ok(Some(Booking::AVERAGE)),
    Some("FIFO") => Ok(Some(Booking::FIFO)),
    Some("LIFO") => Ok(Some(Booking::LIFO)),
    Some("HIFO") => Ok(Some(Booking::HIFO)),
    Some(other) => Err(PyValueError::new_err(format!(
      "Invalid booking type: {}",
      other
    ))),
    None => Ok(None),
  }
}

fn booking_to_py(
  py: Python<'_>,
  cache: &DataCache,
  booking: Option<&str>,
) -> PyResult<Py<PyAny>> {
  let booking = booking_to_native(booking)?;
  Ok(match booking {
    Some(Booking::STRICT) => cache.booking_strict.clone_ref(py),
    Some(Booking::STRICT_WITH_SIZE) => cache.booking_strict_with_size.clone_ref(py),
    Some(Booking::None) => cache.booking_none.clone_ref(py),
    Some(Booking::AVERAGE) => cache.booking_average.clone_ref(py),
    Some(Booking::FIFO) => cache.booking_fifo.clone_ref(py),
    Some(Booking::LIFO) => cache.booking_lifo.clone_ref(py),
    Some(Booking::HIFO) => cache.booking_hifo.clone_ref(py),
    None => py.None(),
  })
}

fn parse_option_descriptors(
  py: Python<'_>,
  options_defs: &Py<PyAny>,
) -> PyResult<HashMap<String, OptionDescriptor>> {
  let mut parsed = HashMap::new();
  let options_defs = options_defs.bind(py).cast::<PyDict>()?;
  for (key_obj, desc_any) in options_defs.iter() {
    let name = key_obj.extract::<String>()?;
    let alias = desc_any.getattr("alias")?.extract::<Option<String>>()?;
    let converter_obj = desc_any.getattr("converter")?;
    let converter = if converter_obj.is_none() {
      None
    } else {
      Some(converter_obj.unbind())
    };
    parsed.insert(name, OptionDescriptor { alias, converter });
  }

  Ok(parsed)
}

/// Expose the Rust parser to Python, matching `beancount.loader.load_file`.

#[pyfunction]
pub fn load_file(
  py: Python<'_>,
  filename: &str,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
  let content = std::fs::read_to_string(filename).map_err(|err| {
    PyValueError::new_err(format!("failed to read {}: {}", filename, err))
  })?;
  parse_source(py, filename, &content)
}

#[pyfunction]
#[pyo3(signature = (content, filename = "<string>"))]
pub fn parse_string(
  py: Python<'_>,
  content: &str,
  filename: Option<&str>,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
  let filename = filename.unwrap_or("<string>");
  parse_source(py, filename, content)
}

fn default_options_map(py: Python<'_>) -> PyResult<Bound<'_, PyDict>> {
  let cache = cache(py)?;
  let defaults = cache.options_defaults.clone_ref(py);
  let copied = cache.deepcopy_fn.call1(py, (defaults,))?;
  let copied = copied.into_bound(py).cast_into::<PyDict>()?;
  Ok(copied)
}

fn build_parser_error_from_meta(
  py: Python<'_>,
  meta: &ast::Meta,
  message: String,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let kv = PyDict::new(py);
  kv.set_item("column", meta.column)?;
  let source = cache
    .new_metadata
    .call1(py, (meta.filename.as_ref(), meta.line, kv))?;
  let py_err = PyParserError {
    source: source.clone_ref(py),
    message,
    entry: None,
  };
  Py::new(py, py_err).map(|e| e.into())
}

fn apply_options(
  py: Python<'_>,
  options_map: &Bound<'_, PyDict>,
  options: &[core::OptionDirective],
) -> PyResult<Vec<Py<PyAny>>> {
  let cache = cache(py)?;
  let option_descriptors = &cache.option_descriptors;
  let read_only_options = cache.read_only_options.bind(py);
  let mut option_errors: Vec<Py<PyAny>> = Vec::new();

  for opt in options {
    let mut key = opt.key.clone();
    let mut value: Bound<'_, PyAny> = PyString::new(py, opt.value.as_str())
      .unbind()
      .into_bound(py)
      .into_any();

    if let Some(desc) = option_descriptors.get(key.as_str()) {
      if let Some(alias) = &desc.alias {
        key = alias.clone();
      }

      if let Some(converter) = desc.converter.as_ref() {
        match converter.bind(py).call1((value,)) {
          Ok(converted) => value = converted,
          Err(err) => {
            let message = err.value(py).str()?;
            let message = message.to_string_lossy().into_owned();
            let message = format!("Error for option '{}': {}", key, message);
            option_errors.push(build_parser_error_from_meta(py, &opt.meta, message)?);
            continue;
          }
        }
      }
    }

    // Skip read-only options (the grammar would have rejected them earlier).
    if read_only_options
      .call_method1("__contains__", (&key,))?
      .extract::<bool>()?
    {
      continue;
    }

    if let Some(current) = options_map.get_item(&key)? {
      if current.is_instance_of::<PyList>() {
        current.cast::<PyList>()?.append(value)?;
        continue;
      }

      if current.is_instance_of::<PyDict>() {
        let tuple = value.cast::<PyTuple>()?;
        if tuple.len() != 2 {
          return Err(PyValueError::new_err(format!(
            "option '{}' expects a (key, value) tuple",
            key
          )));
        }
        let k = tuple.get_item(0)?;
        let v = tuple.get_item(1)?;
        current.cast::<PyDict>()?.set_item(k, v)?;
        continue;
      }
    }

    options_map.set_item(&key, value)?;
  }

  Ok(option_errors)
}

fn apply_display_context_options(
  py: Python<'_>,
  options_map: &Bound<'_, PyDict>,
) -> PyResult<()> {
  let dcontext = options_map
    .get_item("dcontext")?
    .ok_or_else(|| PyValueError::new_err("dcontext option missing from defaults"))?;

  if let Some(render_commas) = options_map.get_item("render_commas")? {
    let render_commas: bool = render_commas.extract()?;
    dcontext.call_method1("set_commas", (render_commas,))?;
  }

  if let Some(display_precision) = options_map.get_item("display_precision")?
    && display_precision.is_instance_of::<PyDict>()
  {
    let display_precision = display_precision.cast::<PyDict>()?;
    let mut items: Vec<(String, Py<PyAny>)> = Vec::new();
    for (currency_obj, example) in display_precision {
      let currency: String = currency_obj.extract()?;
      items.push((currency, example.unbind()));
    }

    items.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    for (currency, example) in items {
      let example = example.bind(py);
      let tuple = example.call_method0("as_tuple")?;
      let exponent: i32 = tuple.getattr("exponent")?.extract()?;
      let precision = -exponent;
      dcontext.call_method1("set_fixed_precision", (currency.as_str(), precision))?;
    }
  }

  Ok(())
}

fn build_parser_error(
  py: Python<'_>,
  err: ParseError,
  filename: &str,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let kv = PyDict::new(py);
  kv.set_item("column", err.column)?;
  let meta = cache.new_metadata.call1(py, (filename, err.line, kv))?;
  let py_err = PyParserError {
    source: meta.clone_ref(py),
    message: err.message,
    entry: None,
  };
  Py::new(py, py_err).map(|e| e.into())
}

fn partition_directives(
  directives: Vec<Directive>,
) -> (
  Vec<String>,
  Vec<Directive>,
  Vec<core::OptionDirective>,
  Vec<core::Plugin>,
) {
  let mut includes = Vec::new();
  let mut filtered = Vec::new();
  let mut options = Vec::new();
  let mut plugins = Vec::new();

  for directive in directives {
    match directive {
      Directive::Include(include) => includes.push(include.filename.clone()),
      Directive::Option(opt) => options.push(opt),
      Directive::Plugin(plugin) => plugins.push(plugin),
      Directive::PushTag(_)
      | Directive::PopTag(_)
      | Directive::PushMeta(_)
      | Directive::PopMeta(_)
      | Directive::Headline(_)
      | Directive::Comment(_)
      | Directive::Raw(_) => filtered.push(directive),
      other => filtered.push(other),
    }
  }
  (includes, filtered, options, plugins)
}

type PyEntriesAndErrors = (Vec<Py<PyAny>>, Vec<Py<PyAny>>);

fn convert_directives(
  py: Python<'_>,
  directives: Vec<Directive>,
  filename: &str,
  dcontext: &Bound<'_, PyAny>,
) -> PyResult<PyEntriesAndErrors> {
  use std::collections::BTreeSet;

  let mut entries: Vec<Py<PyAny>> = Vec::new();
  let mut errors: Vec<Py<PyAny>> = Vec::new();
  let mut active_tags: BTreeSet<String> = BTreeSet::new();
  let mut active_meta: BTreeMap<String, Vec<core::KeyValue>> = BTreeMap::new();

  for directive in directives {
    match directive {
      Directive::PushMeta(pm) => {
        let kv = core::KeyValue {
          span: pm.span,
          key: pm.key.clone(),
          value: pm.value.clone(),
        };
        active_meta.entry(pm.key).or_default().push(kv);
      }
      Directive::PopMeta(pm) => match active_meta.get_mut(&pm.key) {
        Some(stack) => {
          if stack.pop().is_none() {
            let err = ParseError {
              line: pm.meta.line,
              column: pm.meta.column,
              message: format!("Attempting to pop absent metadata key: '{}'", pm.key),
            };
            errors.push(build_parser_error(py, err, filename)?);
          }
          if stack.is_empty() {
            active_meta.remove(&pm.key);
          }
        }
        None => {
          let err = ParseError {
            line: pm.meta.line,
            column: pm.meta.column,
            message: format!("Attempting to pop absent metadata key: '{}'", pm.key),
          };
          errors.push(build_parser_error(py, err, filename)?);
        }
      },
      Directive::PushTag(tag) => {
        active_tags.insert(tag.tag.clone());
      }
      Directive::PopTag(tag) => {
        if !active_tags.remove(&tag.tag) {
          let err = ParseError {
            line: tag.meta.line,
            column: tag.meta.column,
            message: format!("Attempting to pop absent tag: '{}'", tag.tag),
          };
          errors.push(build_parser_error(py, err, filename)?);
        }
      }
      Directive::Transaction(txn) => {
        let mut txn = txn.clone();
        txn.key_values = apply_meta_to_key_values(txn.key_values, &active_meta);

        if active_tags.is_empty() {
          entries.push(convert_transaction(py, &txn, dcontext)?);
        } else {
          let mut tagged = txn;
          let mut tag_set: BTreeSet<String> = tagged.tags.iter().cloned().collect();
          tag_set.extend(active_tags.iter().cloned());
          tagged.tags = tag_set.into_iter().collect();
          entries.push(convert_transaction(py, &tagged, dcontext)?);
        }
      }
      Directive::Document(doc) => {
        let mut doc = doc.clone();
        doc.key_values = apply_meta_to_key_values(doc.key_values, &active_meta);

        if active_tags.is_empty() {
          entries.push(convert_document(py, &doc)?);
        } else {
          let mut tagged = doc;
          let mut tag_set: BTreeSet<String> = tagged.tags.iter().cloned().collect();
          tag_set.extend(active_tags.iter().cloned());
          tagged.tags = tag_set.into_iter().collect();
          entries.push(convert_document(py, &tagged)?);
        }
      }
      Directive::Comment(_) | Directive::Headline(_) => {
        // Ignore comments in Python bindings.
      }
      Directive::Raw(raw) => {
        let err = ParseError {
          line: raw.meta.line,
          column: raw.meta.column,
          message: format!("Unrecognized directive: {}", raw.text),
        };
        errors.push(build_parser_error(py, err, filename)?);
      }
      mut other => {
        // Apply pushed metadata to directives that carry key-value metadata.
        other = apply_meta_to_directive(other, &active_meta);

        if let Some(entry) = convert_directive(py, &other, dcontext)? {
          entries.push(entry);
        }
      }
    }
  }

  if !active_tags.is_empty() {
    for tag in active_tags {
      let err = ParseError {
        line: 0,
        column: 0,
        message: format!("Unbalanced pushed tag: '{}'", tag),
      };
      errors.push(build_parser_error(py, err, filename)?);
    }
  }

  if !active_meta.is_empty() {
    for key in active_meta.keys() {
      let err = ParseError {
        line: 0,
        column: 0,
        message: format!("Unbalanced metadata key: '{}'", key),
      };
      errors.push(build_parser_error(py, err, filename)?);
    }
  }

  Ok((entries, errors))
}

fn apply_meta_to_key_values(
  mut key_values: core::SmallKeyValues,
  active_meta: &BTreeMap<String, Vec<core::KeyValue>>,
) -> core::SmallKeyValues {
  use std::collections::BTreeSet;

  let present: BTreeSet<String> = key_values.iter().map(|kv| kv.key.clone()).collect();

  for (key, stack) in active_meta {
    if present.contains(key) {
      continue;
    }
    if let Some(kv) = stack.last() {
      key_values.push(kv.clone());
    }
  }

  key_values
}

fn apply_meta_to_directive(
  directive: Directive,
  active_meta: &BTreeMap<String, Vec<core::KeyValue>>,
) -> Directive {
  match directive {
    Directive::Open(mut open) => {
      open.key_values = apply_meta_to_key_values(open.key_values, active_meta);
      Directive::Open(open)
    }
    Directive::Close(mut close) => {
      close.key_values = apply_meta_to_key_values(close.key_values, active_meta);
      Directive::Close(close)
    }
    Directive::Balance(mut bal) => {
      bal.key_values = apply_meta_to_key_values(bal.key_values, active_meta);
      Directive::Balance(bal)
    }
    Directive::Pad(mut pad) => {
      pad.key_values = apply_meta_to_key_values(pad.key_values, active_meta);
      Directive::Pad(pad)
    }
    Directive::Commodity(mut c) => {
      c.key_values = apply_meta_to_key_values(c.key_values, active_meta);
      Directive::Commodity(c)
    }
    Directive::Price(mut p) => {
      p.key_values = apply_meta_to_key_values(p.key_values, active_meta);
      Directive::Price(p)
    }
    Directive::Event(mut e) => {
      e.key_values = apply_meta_to_key_values(e.key_values, active_meta);
      Directive::Event(e)
    }
    Directive::Query(mut q) => {
      q.key_values = apply_meta_to_key_values(q.key_values, active_meta);
      Directive::Query(q)
    }
    Directive::Note(mut n) => {
      n.key_values = apply_meta_to_key_values(n.key_values, active_meta);
      Directive::Note(n)
    }
    Directive::Document(mut d) => {
      d.key_values = apply_meta_to_key_values(d.key_values, active_meta);
      Directive::Document(d)
    }
    Directive::Custom(mut c) => {
      c.key_values = apply_meta_to_key_values(c.key_values, active_meta);
      Directive::Custom(c)
    }
    Directive::Transaction(mut t) => {
      t.key_values = apply_meta_to_key_values(t.key_values, active_meta);
      Directive::Transaction(t)
    }
    other => other,
  }
}

fn convert_directive(
  py: Python<'_>,
  directive: &Directive,
  dcontext: &Bound<'_, PyAny>,
) -> PyResult<Option<Py<PyAny>>> {
  match directive {
    Directive::Open(open) => convert_open(py, open).map(Some),
    Directive::Transaction(txn) => convert_transaction(py, txn, dcontext).map(Some),
    Directive::Close(close) => convert_close(py, close).map(Some),
    Directive::Balance(balance) => convert_balance(py, balance, dcontext).map(Some),
    Directive::Pad(pad) => convert_pad(py, pad).map(Some),
    Directive::Commodity(commodity) => convert_commodity(py, commodity).map(Some),
    Directive::Price(price) => convert_price(py, price, dcontext).map(Some),
    Directive::Event(event) => convert_event(py, event).map(Some),
    Directive::Query(query) => convert_query(py, query).map(Some),
    Directive::Note(note) => convert_note(py, note).map(Some),
    Directive::Document(doc) => convert_document(py, doc).map(Some),
    Directive::Custom(custom) => convert_custom(py, custom).map(Some),
    _ => Ok(None),
  }
}

fn make_metadata(
  py: Python<'_>,
  meta: &ast::Meta,
  extra: Option<Bound<'_, PyDict>>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let new_metadata = &cache.new_metadata;
  let kv = if let Some(extra) = extra {
    extra
  } else {
    PyDict::new(py)
  };
  // kv.set_item("column", meta.column)?;
  new_metadata.call1(py, (meta.filename.as_ref(), meta.line, kv))
}

fn meta_extra<'py>(
  py: Python<'py>,
  key_values: &[core::KeyValue],
) -> PyResult<Option<Bound<'py, PyDict>>> {
  if key_values.is_empty() {
    return Ok(None);
  }

  let kv = PyDict::new(py);
  for item in key_values {
    match &item.value {
      None => kv.set_item(item.key.as_str(), py.None())?,
      Some(v) => match v {
        core::KeyValueValue::String(s)
        | core::KeyValueValue::UnquotedString(s)
        | core::KeyValueValue::Raw(s) => kv.set_item(item.key.as_str(), s.as_str())?,
        core::KeyValueValue::Date(s) => kv.set_item(item.key.as_str(), py_date(py, s)?)?,
        core::KeyValueValue::Bool(b) => kv.set_item(item.key.as_str(), *b)?,
      },
    }
  }

  Ok(Some(kv))
}

fn convert_open(py: Python<'_>, open: &core::Open) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let meta = make_metadata(py, &open.meta, meta_extra(py, &open.key_values)?)?;
  let date = py_date_from_naive(py, &open.date)?;
  let currencies = PyList::new(py, open.currencies.iter().map(|c| c.as_str()))?;
  let booking = booking_to_py(py, cache, open.opt_booking.as_deref())?;
  cache
    .open_cls
    .call1(py, (meta, date, open.account.as_str(), currencies, booking))
}

fn parse_naive_date_fast<'py>(py: Python<'py>, date: &str) -> PyResult<Bound<'py, PyDate>> {
  let trimmed = date.trim();
  if trimmed.len() != 10
    || trimmed.as_bytes()[4] != b'-'
    || trimmed.as_bytes()[7] != b'-'
    || !trimmed
      .as_bytes()
      .iter()
      .enumerate()
      .all(|(i, b)| (i == 4 || i == 7) || b.is_ascii_digit())
  {
    return Err(PyValueError::new_err(format!("invalid date `{}`", date)));
  }

  let year: i16 = trimmed[0..4]
    .parse()
    .map_err(|err| PyValueError::new_err(format!("invalid year `{}`: {}", date, err)))?;

  let month: i8 = trimmed[5..7]
    .parse()
    .map_err(|err| PyValueError::new_err(format!("invalid month `{}`: {}", date, err)))?;

  let day: i8 = trimmed[8..10]
    .parse()
    .map_err(|err| PyValueError::new_err(format!("invalid day `{}`: {}", date, err)))?;

  let jiff_date = jiff::civil::date(year, month, day);

  let py_date = PyDate::new(
    py,
    jiff_date.year() as i32,
    jiff_date.month() as u8,
    jiff_date.day() as u8,
  )?;

  Ok(py_date)
}

fn convert_close(py: Python<'_>, close: &core::Close) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.close_cls;
  let meta = make_metadata(py, &close.meta, meta_extra(py, &close.key_values)?)?;
  let date = py_date_from_naive(py, &close.date)?;
  cls.call1(py, (meta, date, close.account.as_str()))
}

fn convert_balance(
  py: Python<'_>,
  balance: &core::Balance,
  dcontext: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.balance_cls;
  let meta = make_metadata(py, &balance.meta, meta_extra(py, &balance.key_values)?)?;
  let date = py_date_from_naive(py, &balance.date)?;
  update_dcontext(
    py,
    cache,
    dcontext,
    &balance.amount.number,
    balance.amount.currency.as_deref(),
  )?;
  let amount = amount_to_py(py, cache, &balance.amount)?;
  let tolerance = match balance.tolerance.as_deref() {
    Some(raw) => {
      let expr = core::NumberExpr::Literal(raw.to_string());
      py_decimal(py, cache, &expr)?
    }
    None => py.None(),
  };
  cls.call1(
    py,
    (
      meta,
      date,
      balance.account.as_str(),
      amount,
      tolerance,
      py.None(),
    ),
  )
}

fn convert_pad(py: Python<'_>, pad: &core::Pad) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.pad_cls;
  let meta = make_metadata(py, &pad.meta, meta_extra(py, &pad.key_values)?)?;
  let date = py_date_from_naive(py, &pad.date)?;
  cls.call1(
    py,
    (meta, date, pad.account.as_str(), pad.from_account.as_str()),
  )
}

fn convert_transaction(
  py: Python<'_>,
  txn: &core::Transaction,
  dcontext: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.transaction_cls;

  let extra = meta_extra(py, &txn.key_values)?;
  let extra = extra.unwrap_or_else(|| PyDict::new(py));
  if let Some(tolerances) = txn.tolerances.as_ref() {
    let tol_dict = PyDict::new(py);
    for (currency, tol) in tolerances {
      let expr = core::NumberExpr::Literal(tol.to_string());
      let dec = py_decimal(py, cache, &expr)?;
      tol_dict.set_item(currency.as_str(), dec)?;
    }
    extra.set_item("__tolerances__", tol_dict)?;
  }

  let meta = make_metadata(py, &txn.meta, Some(extra))?;
  let date = py_date_from_naive(py, &txn.date)?;

  let flag = txn
    .txn
    .as_deref()
    .map(|flag| PyString::new(py, flag).unbind().into())
    .unwrap_or_else(|| py.None());
  let payee = txn
    .payee
    .as_deref()
    .map(|p| PyString::new(py, p).unbind().into())
    .unwrap_or_else(|| py.None());
  let narration = txn
    .narration
    .as_deref()
    .map(|n| PyString::new(py, n).unbind().into())
    .unwrap_or_else(|| py.None());

  let tags = PyFrozenSet::new(py, txn.tags.iter().map(|t| t.as_str()))?;
  let links = PyFrozenSet::new(py, txn.links.iter().map(|l| l.as_str()))?;

  let postings: Vec<Py<PyAny>> = txn
    .postings
    .iter()
    .map(|posting| convert_posting(py, posting, dcontext))
    .collect::<PyResult<_>>()?;
  let postings = PyList::new(py, postings)?;

  cls.call1(
    py,
    (meta, date, flag, payee, narration, tags, links, postings),
  )
}

fn cost_spec_to_py(
  py: Python<'_>,
  cache: &DataCache,
  cost_spec: &core::CostSpec,
) -> PyResult<Py<PyAny>> {
  let mut number_per: Py<PyAny> = cache.missing.clone_ref(py);
  let mut number_total: Py<PyAny> = py.None();
  let mut currency: Py<PyAny> = cache.missing.clone_ref(py);

  if let Some(amount) = &cost_spec.amount {
    if let Some(curr) = amount.currency.as_deref() {
      currency = PyString::new(py, curr).unbind().into();
    }

    if cost_spec.is_total {
      if let Some(total_raw) = amount.total.as_ref() {
        number_total = py_decimal(py, cache, total_raw)?;
        number_per = cache.zero.clone_ref(py);
      } else if let Some(per_raw) = amount.per.as_ref() {
        number_total = py_decimal(py, cache, per_raw)?;
        number_per = cache.zero.clone_ref(py);
      }
    } else {
      if let Some(per_raw) = amount.per.as_ref() {
        number_per = py_decimal(py, cache, per_raw)?;
      }
      if let Some(total_raw) = amount.total.as_ref() {
        number_total = py_decimal(py, cache, total_raw)?;
      }
    }
  }

  let date = cost_spec
    .date
    .as_ref()
    .map(|d| py_date_from_naive(py, d))
    .transpose()?
    .unwrap_or_else(|| py.None());
  let label = cost_spec
    .label
    .as_deref()
    .map(|l| PyString::new(py, l).unbind().into())
    .unwrap_or_else(|| py.None());

  cache.cost_spec_cls.call1(
    py,
    (
      number_per,
      number_total,
      currency,
      date,
      label,
      cost_spec.merge,
    ),
  )
}

fn convert_posting(
  py: Python<'_>,
  posting: &core::Posting,
  dcontext: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.posting_cls;
  let meta = make_metadata(py, &posting.meta, meta_extra(py, &posting.key_values)?)?;
  let units = posting
    .amount
    .as_ref()
    .map(|amount| {
      update_dcontext(
        py,
        cache,
        dcontext,
        &amount.number,
        amount.currency.as_deref(),
      )?;
      amount_to_py(py, cache, amount)
    })
    .transpose()?
    .unwrap_or_else(|| cache.missing.clone_ref(py));
  let cost = if let Some(cost) = posting.cost.as_ref() {
    update_dcontext(
      py,
      cache,
      dcontext,
      &cost.number,
      Some(cost.currency.as_str()),
    )?;
    let number = py_decimal(py, cache, &cost.number)?;
    let date = py_date_from_naive(py, &cost.date)?;
    let label = cost
      .label
      .as_deref()
      .map(|s| PyString::new(py, s).unbind().into())
      .unwrap_or_else(|| py.None());
    cache
      .cost_cls
      .call1(py, (number, cost.currency.as_str(), date, label))?
  } else {
    let cost = posting
      .cost_spec
      .as_ref()
      .map(|cost| {
        if let Some(amount) = &cost.amount
          && let Some(curr) = amount.currency.as_deref()
        {
          if let Some(per) = amount.per.as_ref() {
            update_dcontext(py, cache, dcontext, per, Some(curr))?;
          }
          if let Some(total) = amount.total.as_ref() {
            update_dcontext(py, cache, dcontext, total, Some(curr))?;
          }
        }
        cost_spec_to_py(py, cache, cost)
      })
      .transpose()?;
    cost.unwrap_or_else(|| py.None())
  };
  let price = if let Some(price_ast) = posting.price_annotation.as_ref() {
    let override_number: Option<String> =
      if posting.price_operator == Some(ast::PriceOperator::Total) {
        posting
          .amount
          .as_ref()
          .and_then(|units| per_unit_price_from_total(price_ast, units))
      } else {
        None
      };

    if let Some(per_unit) = override_number {
      let expr = core::NumberExpr::Literal(per_unit.clone());
      update_dcontext(py, cache, dcontext, &expr, price_ast.currency.as_deref())?;
      let curr = price_ast
        .currency
        .as_deref()
        .ok_or_else(|| PyValueError::new_err("missing currency in price annotation"))?;
      amount_from_number_and_currency(py, cache, &per_unit, curr)?
    } else {
      update_dcontext(
        py,
        cache,
        dcontext,
        &price_ast.number,
        price_ast.currency.as_deref(),
      )?;
      amount_to_py(py, cache, price_ast)?
    }
  } else {
    py.None()
  };
  let flag = posting
    .opt_flag
    .as_deref()
    .map(|flag| PyString::new(py, flag).unbind().into())
    .unwrap_or_else(|| py.None());

  cls.call1(
    py,
    (posting.account.as_str(), units, cost, price, flag, meta),
  )
}

fn convert_commodity(py: Python<'_>, commodity: &core::Commodity) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.commodity_cls;
  let meta = make_metadata(py, &commodity.meta, meta_extra(py, &commodity.key_values)?)?;
  let date = py_date_from_naive(py, &commodity.date)?;
  cls.call1(py, (meta, date, commodity.currency.as_str()))
}

fn convert_price(
  py: Python<'_>,
  price: &core::Price,
  dcontext: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.price_cls;
  let meta = make_metadata(py, &price.meta, meta_extra(py, &price.key_values)?)?;
  let date = py_date_from_naive(py, &price.date)?;
  update_dcontext(
    py,
    cache,
    dcontext,
    &price.amount.number,
    price.amount.currency.as_deref(),
  )?;
  let amount = amount_to_py(py, cache, &price.amount)?;
  cls.call1(py, (meta, date, price.currency.as_str(), amount))
}

fn update_dcontext(
  py: Python<'_>,
  cache: &DataCache,
  dcontext: &Bound<'_, PyAny>,
  number: &core::NumberExpr,
  currency: Option<&str>,
) -> PyResult<()> {
  if matches!(number, core::NumberExpr::Missing) {
    return Ok(());
  }

  if let Some(curr) = currency {
    let dec = py_decimal(py, cache, number)?;
    dcontext.call_method1("update", (dec, curr))?;
  }
  Ok(())
}

fn convert_event(py: Python<'_>, event: &core::Event) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.event_cls;
  let meta = make_metadata(py, &event.meta, meta_extra(py, &event.key_values)?)?;
  let date = py_date_from_naive(py, &event.date)?;
  cls.call1(
    py,
    (meta, date, event.event_type.as_str(), event.desc.as_str()),
  )
}

fn convert_query(py: Python<'_>, query: &core::Query) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.query_cls;
  let meta = make_metadata(py, &query.meta, meta_extra(py, &query.key_values)?)?;
  let date = py_date_from_naive(py, &query.date)?;
  cls.call1(py, (meta, date, query.name.as_str(), query.query.as_str()))
}

fn convert_note(py: Python<'_>, note: &core::Note) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.note_cls;
  let meta = make_metadata(py, &note.meta, meta_extra(py, &note.key_values)?)?;
  let date = py_date_from_naive(py, &note.date)?;
  cls.call1(
    py,
    (
      meta,
      date,
      note.account.as_str(),
      note.note.as_str(),
      py.None(),
      py.None(),
    ),
  )
}

fn convert_document(py: Python<'_>, doc: &core::Document) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.document_cls;
  let meta = make_metadata(py, &doc.meta, meta_extra(py, &doc.key_values)?)?;
  let date = py_date_from_naive(py, &doc.date)?;
  let tags = PyFrozenSet::new(py, doc.tags.iter().map(|t| t.as_str()))?;
  let links = PyFrozenSet::new(py, doc.links.iter().map(|l| l.as_str()))?;
  cls.call1(
    py,
    (
      meta,
      date,
      doc.account.as_str(),
      doc.filename.as_str(),
      tags,
      links,
    ),
  )
}

fn convert_custom(py: Python<'_>, custom: &core::Custom) -> PyResult<Py<PyAny>> {
  let cache = cache(py)?;
  let cls = &cache.custom_cls;
  let meta = make_metadata(py, &custom.meta, meta_extra(py, &custom.key_values)?)?;
  let date = py_date_from_naive(py, &custom.date)?;
  let values: Vec<Py<PyAny>> = custom
    .values
    .iter()
    .map(|value| convert_custom_value(py, cache, value))
    .collect::<PyResult<_>>()?;
  let values = PyList::new(py, values)?;
  cls.call1(py, (meta, date, custom.name.as_str(), values))
}

fn convert_custom_value(
  py: Python<'_>,
  cache: &DataCache,
  value: &core::CustomValue,
) -> PyResult<Py<PyAny>> {
  let (py_value, dtype): (Py<PyAny>, Py<PyAny>) = match value {
    core::CustomValue::String(raw) => {
      let py_value: Py<PyAny> = PyString::new(py, raw).unbind().into();
      let dtype = cache.str_type.clone_ref(py);
      (py_value, dtype)
    }
    core::CustomValue::Date(date) => {
      let py_value =
        PyDate::new(py, date.year() as i32, date.month() as u8, date.day() as u8)?
          .unbind()
          .into();
      let dtype = cache.date_type.clone_ref(py);
      (py_value, dtype)
    }
    core::CustomValue::Bool(val) => {
      let py_bool: Py<PyAny> = if *val {
        cache.py_true.clone_ref(py)
      } else {
        cache.py_false.clone_ref(py)
      };
      let dtype = cache.bool_type.clone_ref(py);
      (py_bool, dtype)
    }
    core::CustomValue::Amount(amount) => {
      let amount = amount_to_py(py, cache, amount)?;
      let dtype = cache.amount_ctor.clone_ref(py);
      (amount, dtype)
    }
    core::CustomValue::Number(expr) => {
      let decimal: Py<PyAny> = py_decimal(py, cache, expr)?;
      let dtype = cache.decimal_type.clone_ref(py);
      (decimal, dtype)
    }
    core::CustomValue::Account(raw) => {
      let py_value: Py<PyAny> = PyString::new(py, raw.as_str()).unbind().into();
      let dtype = cache.account_type_token.clone_ref(py);
      (py_value, dtype)
    }
  };

  cache.value_type_cls.call1(py, (py_value, dtype))
}

fn parse_source(
  py: Python<'_>,
  filename: &str,
  content: &str,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
  let options_map = default_options_map(py)?;
  options_map.set_item("filename", filename)?;

  let directives = parse_lossy(content);

  let normalized = match normalize_directives(&directives, filename, content) {
    Ok(normalized) => normalized,
    Err(err) => {
      options_map.set_item("include", PyList::empty(py))?;
      let _ = apply_options(py, &options_map, &[])?;
      apply_display_context_options(py, &options_map)?;
      let errors = PyList::new(py, [build_parser_error(py, err, filename)?])?
        .unbind()
        .into();
      let entries: Py<PyAny> = PyList::empty(py).unbind().into();
      return Ok((entries, errors, options_map.unbind().into()));
    }
  };

  let (includes, filtered, options, plugins) = partition_directives(normalized);
  options_map.set_item("include", PyList::new(py, &includes)?)?;
  let option_errors = apply_options(py, &options_map, &options)?;

  if !plugins.is_empty() {
    let plugin_list_obj = options_map
      .get_item("plugin")?
      .ok_or_else(|| PyValueError::new_err("plugin option missing from defaults"))?
      .unbind();
    let plugin_list = plugin_list_obj.bind(py).cast::<PyList>()?;
    for plugin in plugins {
      let name = PyString::new(py, &plugin.name).unbind().into();
      let config = plugin
        .config
        .as_deref()
        .map(|c| PyString::new(py, c).unbind().into())
        .unwrap_or_else(|| py.None());
      let tuple = PyTuple::new(py, [name, config])?;
      plugin_list.append(tuple)?;
    }
  }

  apply_display_context_options(py, &options_map)?;
  let dcontext = options_map
    .get_item("dcontext")?
    .ok_or_else(|| PyValueError::new_err("dcontext option missing from defaults"))?;
  let (entries_vec, tag_errors) = convert_directives(py, filtered, filename, &dcontext)?;
  let entries = PyList::new(py, entries_vec)?.unbind().into();
  let mut all_errors = option_errors;
  all_errors.extend(tag_errors);
  let errors: Py<PyAny> = PyList::new(py, all_errors)?.unbind().into();
  Ok((entries, errors, options_map.unbind().into()))
}

/// Convert `YYYY-MM-DD` text to `datetime.date`.
fn py_date(py: Python<'_>, date: &str) -> PyResult<Py<PyAny>> {
  let trimmed = date.trim();
  // Fast manual parse: expect YYYY-MM-DD, all ASCII digits except separators.
  if trimmed.len() != 10
    || trimmed.as_bytes()[4] != b'-'
    || trimmed.as_bytes()[7] != b'-'
    || !trimmed
      .as_bytes()
      .iter()
      .enumerate()
      .all(|(i, b)| (i == 4 || i == 7) || b.is_ascii_digit())
  {
    return Err(PyValueError::new_err(format!("invalid date `{}`", date)));
  }

  let year = trimmed[0..4]
    .parse::<i32>()
    .map_err(|err| PyValueError::new_err(format!("invalid year `{}`: {}", date, err)))?;
  let month = trimmed[5..7]
    .parse::<u8>()
    .map_err(|err| PyValueError::new_err(format!("invalid month `{}`: {}", date, err)))?;
  let day = trimmed[8..10]
    .parse::<u8>()
    .map_err(|err| PyValueError::new_err(format!("invalid day `{}`: {}", date, err)))?;

  let pydate: Py<PyAny> = PyDate::new(py, year, month, day)?.unbind().into();
  Ok(pydate)
}

fn py_date_from_naive(py: Python<'_>, date: &Date) -> PyResult<Py<PyAny>> {
  let pydate: Py<PyAny> =
    PyDate::new(py, date.year() as i32, date.month() as u8, date.day() as u8)?
      .unbind()
      .into();
  Ok(pydate)
}

fn decimal_from_number_expr(num: &core::NumberExpr) -> PyResult<Decimal> {
  core::number_expr_to_decimal(num).map_err(|err| PyValueError::new_err(err.message))
}

fn py_decimal(
  py: Python<'_>,
  cache: &DataCache,
  number: &core::NumberExpr,
) -> PyResult<Py<PyAny>> {
  if matches!(number, core::NumberExpr::Missing) {
    return Ok(cache.missing.clone_ref(py));
  }
  let parsed = decimal_from_number_expr(number)?;
  let py_decimal = parsed.into_pyobject(py)?;
  Ok(py_decimal.unbind())
}

fn py_amount(
  py: Python<'_>,
  cache: &DataCache,
  number: Py<PyAny>,
  currency: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
  cache.amount_ctor.call1(py, (number, currency))
}

fn amount_to_py(
  py: Python<'_>,
  cache: &DataCache,
  amount: &core::Amount,
) -> PyResult<Py<PyAny>> {
  let number = if matches!(amount.number, core::NumberExpr::Missing) {
    cache.missing.clone_ref(py)
  } else {
    py_decimal(py, cache, &amount.number)?
  };

  let currency: Py<PyAny> = amount
    .currency
    .as_deref()
    .map(|c| PyString::new(py, c).unbind().into())
    .unwrap_or_else(|| cache.missing.clone_ref(py));

  py_amount(py, cache, number, currency)
}

fn amount_from_number_and_currency(
  py: Python<'_>,
  cache: &DataCache,
  number: &str,
  currency: &str,
) -> PyResult<Py<PyAny>> {
  let num_expr = core::NumberExpr::Literal(number.to_string());
  let d = py_decimal(py, cache, &num_expr)?;
  let currency_obj: Py<PyAny> = PyString::new(py, currency).unbind().into();
  py_amount(py, cache, d, currency_obj)
}

fn per_unit_price_from_total(price: &core::Amount, units: &core::Amount) -> Option<String> {
  let total = decimal_from_number_expr(&price.number).ok()?;
  let qty = decimal_from_number_expr(&units.number).ok()?;
  if qty.is_zero() {
    return None;
  }
  let per_unit = (total / qty).normalize();
  Some(per_unit.to_string())
}
