#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]

use beancount_parser::ParseError;
use beancount_parser::ast;
use beancount_parser::core;
use beancount_parser::parse_str;
use chrono::Datelike;
use core::CoreDirective;
use core::normalize_directives;
use pyo3::IntoPyObject;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyDate, PyDict, PyFrozenSet, PyList, PyString, PyTuple};
use rust_decimal::Decimal;
use std::collections::HashMap;

#[pymodule]
fn _parser_rust(py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    DATA_CACHE.get_or_try_init(py, || DataCache::init(py))?;
    m.add_function(wrap_pyfunction!(load_file, m)?)?;
    m.add_function(wrap_pyfunction!(parse_string, m)?)?;
    m.add_function(wrap_pyfunction!(build_options_map, m)?)?;
    m.add_function(wrap_pyfunction!(py_date, m)?)?;
    m.add_class::<PyParserError>()?;
    Ok(())
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
    // beancount.core.data.Booking enum.
    booking_enum: Py<PyAny>,
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
    // beancount.core.position.CostSpec class.
    cost_spec_cls: Py<PyAny>,
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

        let bool_type: Py<PyAny> = builtins_mod.getattr("bool")?.unbind();
        let str_type: Py<PyAny> = builtins_mod.getattr("str")?.unbind();
        let date_type: Py<PyAny> = datetime_mod.getattr("date")?.unbind();
        let decimal_type: Py<PyAny> = decimal_mod.getattr("Decimal")?.unbind();

        let missing = number_mod.getattr("MISSING")?.unbind();
        let zero = number_mod.getattr("ZERO")?.unbind();
        let cost_spec_cls = position_mod.getattr("CostSpec")?.unbind();
        let options_defs = options_mod.getattr("OPTIONS")?.unbind();
        let option_descriptors = parse_option_descriptors(py, &options_defs)?;
        let deepcopy_fn = copy_mod.getattr("deepcopy")?.unbind();
        let amount_ctor = amount_mod.getattr("Amount")?.unbind();

        Ok(Self {
            py_true: builtins_mod.getattr("True")?.unbind(),
            py_false: builtins_mod.getattr("False")?.unbind(),
            new_metadata: data_mod.getattr("new_metadata")?.unbind(),
            booking_enum: data_mod.getattr("Booking")?.unbind(),
            open_cls: data_mod.getattr("Open")?.unbind(),
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
            amount_ctor,
            deepcopy_fn,
            cost_spec_cls,
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
        })
    }
}

fn cache(py: Python<'_>) -> PyResult<&'static DataCache> {
    DATA_CACHE.get_or_try_init(py, || DataCache::init(py))
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
pub fn load_file(py: Python<'_>, filename: &str) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
    let content = std::fs::read_to_string(filename)
        .map_err(|err| PyValueError::new_err(format!("failed to read {}: {}", filename, err)))?;
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

#[pyfunction]
#[pyo3(signature = (filename = "<string>"))]
fn build_options_map(py: Python<'_>, filename: Option<&str>) -> PyResult<Py<PyAny>> {
    let filename = filename.unwrap_or("<string>");
    let options = default_options_map(py)?;
    options.set_item("filename", filename)?;
    options.set_item("include", PyList::new(py, [filename])?)?;
    Ok(options.unbind().into())
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
        .call1(py, (meta.filename.clone(), meta.line, kv))?;
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

fn apply_display_context_options(py: Python<'_>, options_map: &Bound<'_, PyDict>) -> PyResult<()> {
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

fn build_parser_error(py: Python<'_>, err: ParseError) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let kv = PyDict::new(py);
    kv.set_item("column", err.column)?;
    let meta = cache
        .new_metadata
        .call1(py, (err.filename.clone(), err.line, kv))?;
    let py_err = PyParserError {
        source: meta.clone_ref(py),
        message: err.message,
        entry: None,
    };
    Py::new(py, py_err).map(|e| e.into())
}

fn partition_directives(
    directives: Vec<CoreDirective>,
) -> (
    Vec<String>,
    Vec<CoreDirective>,
    Vec<core::OptionDirective>,
    Vec<core::Plugin>,
) {
    let mut includes = Vec::new();
    let mut filtered = Vec::new();
    let mut options = Vec::new();
    let mut plugins = Vec::new();

    for directive in directives {
        match directive {
            CoreDirective::Include(include) => includes.push(include.filename.clone()),
            CoreDirective::Option(opt) => options.push(opt),
            CoreDirective::Plugin(plugin) => plugins.push(plugin),
            CoreDirective::Pushtag(_)
            | CoreDirective::Poptag(_)
            | CoreDirective::Pushmeta(_)
            | CoreDirective::Popmeta(_) => {}
            CoreDirective::Raw(_) => {}
            other => filtered.push(other),
        }
    }
    (includes, filtered, options, plugins)
}

fn convert_directives(
    py: Python<'_>,
    directives: Vec<CoreDirective>,
    dcontext: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let entries: Vec<Py<PyAny>> = directives
        .iter()
        .filter_map(|directive| convert_directive(py, directive, dcontext).transpose())
        .collect::<PyResult<_>>()?;
    Ok(PyList::new(py, entries)?.unbind().into())
}

fn convert_directive(
    py: Python<'_>,
    directive: &CoreDirective,
    dcontext: &Bound<'_, PyAny>,
) -> PyResult<Option<Py<PyAny>>> {
    match directive {
        CoreDirective::Open(open) => convert_open(py, open).map(Some),
        CoreDirective::Transaction(txn) => convert_transaction(py, txn, dcontext).map(Some),
        CoreDirective::Close(close) => convert_close(py, close).map(Some),
        CoreDirective::Balance(balance) => convert_balance(py, balance, dcontext).map(Some),
        CoreDirective::Pad(pad) => convert_pad(py, pad).map(Some),
        CoreDirective::Commodity(commodity) => convert_commodity(py, commodity).map(Some),
        CoreDirective::Price(price) => convert_price(py, price, dcontext).map(Some),
        CoreDirective::Event(event) => convert_event(py, event).map(Some),
        CoreDirective::Query(query) => convert_query(py, query).map(Some),
        CoreDirective::Note(note) => convert_note(py, note).map(Some),
        CoreDirective::Document(doc) => convert_document(py, doc).map(Some),
        CoreDirective::Custom(custom) => convert_custom(py, custom).map(Some),
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
    new_metadata.call1(py, (meta.filename.clone(), meta.line, kv))
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
                core::KeyValueValue::Bool(b) => kv.set_item(item.key.as_str(), *b)?,
            },
        }
    }

    Ok(Some(kv))
}

fn convert_open(py: Python<'_>, open: &core::Open) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.open_cls;
    let currencies = PyList::new(py, open.currencies.iter().map(|c| c.as_str()))?;
    let booking = match open.opt_booking {
        Some(ref name) => cache.booking_enum.getattr(py, name.as_str())?,
        None => py.None(),
    };
    let meta = make_metadata(py, &open.meta, meta_extra(py, &open.key_values)?)?;
    let date = py_date(py, open.date.as_str())?;
    cls.call1(py, (meta, date, open.account.as_str(), currencies, booking))
}

fn convert_close(py: Python<'_>, close: &core::Close) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.close_cls;
    let meta = make_metadata(py, &close.meta, meta_extra(py, &close.key_values)?)?;
    let date = py_date(py, close.date.as_str())?;
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
    let date = py_date(py, balance.date.as_str())?;
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
    let date = py_date(py, pad.date.as_str())?;
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

    let meta = make_metadata(py, &txn.meta, meta_extra(py, &txn.key_values)?)?;
    let date = py_date(py, txn.date.as_str())?;

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
        .as_deref()
        .map(|d| py_date(py, d))
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
    let cost = cost.unwrap_or_else(|| py.None());
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
    let date = py_date(py, commodity.date.as_str())?;
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
    let date = py_date(py, price.date.as_str())?;
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
    let date = py_date(py, event.date.as_str())?;
    cls.call1(
        py,
        (meta, date, event.event_type.as_str(), event.desc.as_str()),
    )
}

fn convert_query(py: Python<'_>, query: &core::Query) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.query_cls;
    let meta = make_metadata(py, &query.meta, meta_extra(py, &query.key_values)?)?;
    let date = py_date(py, query.date.as_str())?;
    cls.call1(py, (meta, date, query.name.as_str(), query.query.as_str()))
}

fn convert_note(py: Python<'_>, note: &core::Note) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.note_cls;
    let meta = make_metadata(py, &note.meta, meta_extra(py, &note.key_values)?)?;
    let date = py_date(py, note.date.as_str())?;
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
    let date = py_date(py, doc.date.as_str())?;
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
    let date = py_date(py, custom.date.as_str())?;
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
            let py_value = PyDate::new(py, date.year(), date.month() as u8, date.day() as u8)?
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

    let directives = match parse_str(content, filename) {
        Ok(directives) => directives,
        Err(err) => {
            options_map.set_item("include", PyList::empty(py))?;
            let _ = apply_options(py, &options_map, &[])?;
            apply_display_context_options(py, &options_map)?;
            let errors = PyList::new(py, [build_parser_error(py, err)?])?
                .unbind()
                .into();
            let entries: Py<PyAny> = PyList::empty(py).unbind().into();
            return Ok((entries, errors, options_map.unbind().into()));
        }
    };

    let normalized = match normalize_directives(directives) {
        Ok(normalized) => normalized,
        Err(err) => {
            options_map.set_item("include", PyList::empty(py))?;
            let _ = apply_options(py, &options_map, &[])?;
            apply_display_context_options(py, &options_map)?;
            let errors = PyList::new(py, [build_parser_error(py, err)?])?
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
    let entries = convert_directives(py, filtered, &dcontext)?;
    let errors: Py<PyAny> = PyList::new(py, option_errors)?.unbind().into();
    Ok((entries, errors, options_map.unbind().into()))
}

/// Convert `YYYY-MM-DD` text to `datetime.date`.
#[pyfunction]
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

fn decimal_from_number_expr(num: &core::NumberExpr) -> PyResult<Decimal> {
    core::number_expr_to_decimal(num).map_err(|err| PyValueError::new_err(err.message))
}

fn py_decimal(py: Python<'_>, cache: &DataCache, number: &core::NumberExpr) -> PyResult<Py<PyAny>> {
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

fn amount_to_py(py: Python<'_>, cache: &DataCache, amount: &core::Amount) -> PyResult<Py<PyAny>> {
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
