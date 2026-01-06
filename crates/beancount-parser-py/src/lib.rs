mod core;

use crate::core as bcore;

use beancount_parser::ParseError;
use beancount_parser::ast;
use bcore::CoreDirective;
use bcore::normalize_directives;
use beancount_parser::parse_amount_tokens;
use beancount_parser::parse_str;
use chrono::{Datelike, NaiveDate};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyBool, PyDate, PyDict, PyFrozenSet, PyList, PyString, PyTuple};
use pyo3::BoundObject;
use rust_decimal::Decimal;
use std::str::FromStr;

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
        Self { source, message, entry }
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
            "ParserError(source={}, message={}, entry={})",
            source_repr, self.message, entry_repr
        ))
    }
}

/// Global cache for imported modules and classes so we don't look them up repeatedly.
static DATA_CACHE: PyOnceLock<DataCache> = PyOnceLock::new();

struct DataCache {
    number_mod: Py<PyAny>,
    amount_mod: Py<PyAny>,
    new_metadata: Py<PyAny>,
    booking_enum: Py<PyAny>,
    open_cls: Py<PyAny>,
    close_cls: Py<PyAny>,
    balance_cls: Py<PyAny>,
    pad_cls: Py<PyAny>,
    transaction_cls: Py<PyAny>,
    posting_cls: Py<PyAny>,
    commodity_cls: Py<PyAny>,
    price_cls: Py<PyAny>,
    event_cls: Py<PyAny>,
    query_cls: Py<PyAny>,
    note_cls: Py<PyAny>,
    document_cls: Py<PyAny>,
    custom_cls: Py<PyAny>,
    cost_spec_cls: Py<PyAny>,
    value_type_cls: Py<PyAny>,
    account_type_token: Py<PyAny>,
    options_defaults: Py<PyAny>,
    missing: Py<PyAny>,
    zero: Py<PyAny>,
}

impl DataCache {
    fn init(py: Python<'_>) -> PyResult<Self> {
        let data_mod = py.import("beancount.core.data")?;
        let number_mod = py.import("beancount.core.number")?;
        let amount_mod = py.import("beancount.core.amount")?;
        let account_mod = py.import("beancount.core.account")?;
        let position_mod = py.import("beancount.core.position")?;
        let options_mod = py.import("beancount.parser.options")?;
        let grammar_mod = py.import("beancount.parser.grammar")?;

        let missing = number_mod.getattr("MISSING")?.unbind();
        let zero = number_mod.getattr("ZERO")?.unbind();
        let cost_spec_cls = position_mod.getattr("CostSpec")?.unbind();

        Ok(Self {
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
            number_mod: number_mod.into(),
            amount_mod: amount_mod.into(),
            cost_spec_cls,
            value_type_cls: grammar_mod.getattr("ValueType")?.unbind(),
            account_type_token: account_mod.getattr("TYPE")?.unbind(),
            options_defaults: options_mod.getattr("OPTIONS_DEFAULTS")?.unbind(),
            missing,
            zero,
        })
    }
}

fn cache(py: Python<'_>) -> PyResult<&'static DataCache> {
    DATA_CACHE.get_or_try_init(py, || DataCache::init(py))
}

/// Expose the Rust parser to Python, matching `beancount.loader.load_file`.

#[pyfunction]
pub fn load_file(py: Python<'_>, filename: &str) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
    let content = std::fs::read_to_string(filename)
        .map_err(|err| PyValueError::new_err(format!("failed to read {}: {}", filename, err)))?;
    parse_source(py, filename, &content)
}

#[pyfunction]
#[pyo3(signature = (content, filename = "<memory>"))]
pub fn parse_string(
    py: Python<'_>,
    content: &str,
    filename: Option<&str>,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
    let filename = filename.unwrap_or("<memory>");
    parse_source(py, filename, content)
}

#[pyfunction]
#[pyo3(signature = (filename = "<memory>"))]
fn build_options_map(py: Python<'_>, filename: Option<&str>) -> PyResult<Py<PyAny>> {
    let filename = filename.unwrap_or("<memory>");
    let options = default_options_map(py)?;
    options.set_item("filename", filename)?;
    options.set_item("include", PyList::new(py, [filename])?)?;
    Ok(options.unbind().into())
}

fn default_options_map(py: Python<'_>) -> PyResult<Bound<'_, PyDict>> {
    let cache = cache(py)?;
    let copy_mod = py.import("copy")?;
    let defaults = cache.options_defaults.clone_ref(py);
    let copied = copy_mod.getattr("deepcopy")?.call1((defaults,))?;
    let copied = copied.into_bound().cast_into::<PyDict>()?;
    Ok(copied)
}

fn apply_options(py: Python<'_>, options_map: &Bound<'_, PyDict>, options: &[bcore::OptionDirective]) -> PyResult<()> {
    let cache = cache(py)?;

    for opt in options {
        let key = &opt.key;
        let value = &opt.value;

        match key.as_str() {
            "infer_tolerance_from_cost" => {
                let val = matches!(value.to_ascii_lowercase().as_str(), "1" | "true" | "yes" | "on");
                options_map.set_item(key, val)?;
            }
            "operating_currency" => {
                let raw = options_map
                    .get_item("operating_currency")?
                    .ok_or_else(|| PyValueError::new_err("operating_currency not initialized as list"))?;
                let list = raw.cast::<PyList>()?;
                let py_str = PyString::new(py, value);
                list.append(py_str)?;
            }
            "insert_pythonpath" => {
                let val = matches!(value.to_ascii_lowercase().as_str(), "1" | "true" | "yes" | "on");
                options_map.set_item(key, val)?;
            }
            "plugin_processing_mode" => {
                options_map.set_item(key, PyString::new(py, value))?;
            }
            "tolerance_multiplier" | "inferred_tolerance_multiplier" => {
                let decimal = py_decimal(py, cache, value)?;
                options_map.set_item("tolerance_multiplier", decimal)?;
            }
            _ => {}
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
    Vec<bcore::OptionDirective>,
    Vec<bcore::Plugin>,
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

fn convert_directives(py: Python<'_>, directives: Vec<CoreDirective>, dcontext: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let entries: Vec<Py<PyAny>> = directives
        .iter()
        .filter_map(|directive| convert_directive(py, directive, dcontext).transpose())
        .collect::<PyResult<_>>()?;
    Ok(PyList::new(py, entries)?.unbind().into())
}

fn convert_directive(py: Python<'_>, directive: &CoreDirective, dcontext: &Bound<'_, PyAny>) -> PyResult<Option<Py<PyAny>>> {
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

fn meta_extra<'py>(py: Python<'py>, key_values: &[bcore::KeyValue]) -> PyResult<Option<Bound<'py, PyDict>>> {
    if key_values.is_empty() {
        return Ok(None);
    }

    let kv = PyDict::new(py);
    for item in key_values {
        match &item.value {
            bcore::KeyValueValue::String(s) => kv.set_item(item.key.as_str(), s.as_str())?,
            bcore::KeyValueValue::Raw(v) => kv.set_item(item.key.as_str(), v.as_str())?,
        }
    }

    Ok(Some(kv))
}

fn convert_open(py: Python<'_>, open: &bcore::Open) -> PyResult<Py<PyAny>> {
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

fn convert_close(py: Python<'_>, close: &bcore::Close) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.close_cls;
    let meta = make_metadata(py, &close.meta, meta_extra(py, &close.key_values)?)?;
    let date = py_date(py, close.date.as_str())?;
    cls.call1(py, (meta, date, close.account.as_str()))
}

fn convert_balance(py: Python<'_>, balance: &bcore::Balance, dcontext: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.balance_cls;
    let meta = make_metadata(py, &balance.meta, meta_extra(py, &balance.key_values)?)?;
    let date = py_date(py, balance.date.as_str())?;
    update_dcontext(
        py,
        cache,
        dcontext,
        balance.amount.number.as_str(),
        Some(balance.amount.currency.as_str()),
    )?;
    let amount = amount_to_py(py, cache, &balance.amount)?;
    cls.call1(
        py,
        (meta, date, balance.account.as_str(), amount, py.None(), py.None()),
    )
}

fn convert_pad(py: Python<'_>, pad: &bcore::Pad) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.pad_cls;
    let meta = make_metadata(py, &pad.meta, meta_extra(py, &pad.key_values)?)?;
    let date = py_date(py, pad.date.as_str())?;
    cls.call1(py, (meta, date, pad.account.as_str(), pad.from_account.as_str()))
}

fn convert_transaction(py: Python<'_>, txn: &bcore::Transaction, dcontext: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
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
        (
            meta,
            date,
            flag,
            payee,
            narration,
            tags,
            links,
            postings,
        ),
    )
}

fn cost_spec_to_py(
    py: Python<'_>,
    cache: &DataCache,
    cost_spec: &bcore::CostSpec,
) -> PyResult<Py<PyAny>> {
    let mut number_per: Py<PyAny> = cache.missing.clone_ref(py);
    let mut number_total: Py<PyAny> = py.None();
    let mut currency: Py<PyAny> = cache.missing.clone_ref(py);

    if let Some(amount) = &cost_spec.amount {
        if let Some(curr) = amount.currency.as_deref() {
            currency = PyString::new(py, curr).unbind().into();
        }

        if cost_spec.is_total {
            if let Some(total_raw) = amount.total.as_deref() {
                number_total = py_decimal(py, cache, total_raw.trim())?;
                number_per = cache.zero.clone_ref(py);
            } else if let Some(per_raw) = amount.per.as_deref() {
                number_total = py_decimal(py, cache, per_raw.trim())?;
                number_per = cache.zero.clone_ref(py);
            }
        } else {
            if let Some(per_raw) = amount.per.as_deref() {
                number_per = py_decimal(py, cache, per_raw.trim())?;
            }
            if let Some(total_raw) = amount.total.as_deref() {
                number_total = py_decimal(py, cache, total_raw.trim())?;
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

    cache
        .cost_spec_cls
        .call1(py, (number_per, number_total, currency, date, label, cost_spec.merge))
}

fn convert_posting(py: Python<'_>, posting: &bcore::Posting, dcontext: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.posting_cls;
    let meta = make_metadata(py, &posting.meta, None)?;
    let units = posting
        .amount
        .as_ref()
        .map(|amount| {
            update_dcontext(
                py,
                cache,
                dcontext,
                amount.number.as_str(),
                Some(amount.currency.as_str()),
            )?;
            amount_to_py(py, cache, amount)
        })
        .transpose()?
        .unwrap_or_else(|| cache.missing.clone_ref(py));
    let cost = posting
        .cost_spec
        .as_ref()
        .map(|cost| {
            if let Some(amount) = &cost.amount && let Some(curr) = amount.currency.as_deref() {
                if let Some(per) = amount.per.as_deref() {
                    update_dcontext(py, cache, dcontext, per, Some(curr))?;
                }
                if let Some(total) = amount.total.as_deref() {
                    update_dcontext(py, cache, dcontext, total, Some(curr))?;
                }
            }
            cost_spec_to_py(py, cache, cost)
        })
        .transpose()?;
    let cost = cost.unwrap_or_else(|| py.None());
    let price = if let Some(price_ast) = posting.price_annotation.as_ref() {
        let override_number: Option<String> = if let Some(op) = posting.price_operator.as_deref() {
            if op == "@@" || op == "atat" {
                posting
                    .amount
                    .as_ref()
                    .and_then(|units| per_unit_price_from_total(price_ast, units))
            } else {
                None
            }
        } else {
            None
        };

        if let Some(per_unit) = override_number {
            update_dcontext(
                py,
                cache,
                dcontext,
                &per_unit,
                Some(price_ast.currency.as_str()),
            )?;
            amount_from_number_and_currency(py, cache, &per_unit, price_ast.currency.as_str())?
        } else {
            update_dcontext(
                py,
                cache,
                dcontext,
                price_ast.number.as_str(),
                Some(price_ast.currency.as_str()),
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

fn convert_commodity(py: Python<'_>, commodity: &bcore::Commodity) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.commodity_cls;
    let meta = make_metadata(py, &commodity.meta, meta_extra(py, &commodity.key_values)?)?;
    let date = py_date(py, commodity.date.as_str())?;
    cls.call1(py, (meta, date, commodity.currency.as_str()))
}

fn convert_price(py: Python<'_>, price: &bcore::Price, dcontext: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.price_cls;
    let meta = make_metadata(py, &price.meta, meta_extra(py, &price.key_values)?)?;
    let date = py_date(py, price.date.as_str())?;
    update_dcontext(
        py,
        cache,
        dcontext,
        price.amount.number.as_str(),
        Some(price.amount.currency.as_str()),
    )?;
    let amount = amount_to_py(py, cache, &price.amount)?;
    cls.call1(py, (meta, date, price.currency.as_str(), amount))
}

fn update_dcontext(
    py: Python<'_>,
    cache: &DataCache,
    dcontext: &Bound<'_, PyAny>,
    number: &str,
    currency: Option<&str>,
) -> PyResult<()> {
    if let Some(curr) = currency {
        let dec = py_decimal(py, cache, number)?;
        dcontext.call_method1("update", (dec, curr))?;
    }
    Ok(())
}

fn convert_event(py: Python<'_>, event: &bcore::Event) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.event_cls;
    let meta = make_metadata(py, &event.meta, meta_extra(py, &event.key_values)?)?;
    let date = py_date(py, event.date.as_str())?;
    cls.call1(py, (meta, date, event.event_type.as_str(), event.desc.as_str()))
}

fn convert_query(py: Python<'_>, query: &bcore::Query) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.query_cls;
    let meta = make_metadata(py, &query.meta, meta_extra(py, &query.key_values)?)?;
    let date = py_date(py, query.date.as_str())?;
    cls.call1(py, (meta, date, query.name.as_str(), query.query.as_str()))
}

fn convert_note(py: Python<'_>, note: &bcore::Note) -> PyResult<Py<PyAny>> {
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

fn convert_document(py: Python<'_>, doc: &bcore::Document) -> PyResult<Py<PyAny>> {
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

fn convert_custom(py: Python<'_>, custom: &bcore::Custom) -> PyResult<Py<PyAny>> {
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

fn convert_custom_value(py: Python<'_>, cache: &DataCache, value: &bcore::CustomValue) -> PyResult<Py<PyAny>> {
    let (py_value, dtype): (Py<PyAny>, Py<PyAny>) = match value.kind {
        ast::CustomValueKind::String => {
            let raw = value.string.as_deref().unwrap_or(value.raw.as_str());
            let py_value: Py<PyAny> = PyString::new(py, raw).unbind().into();
            let dtype = py_value.bind(py).get_type().unbind().into();
            (py_value, dtype)
        }
        ast::CustomValueKind::Date => {
            let py_value = py_date(py, value.raw.as_str())?;
            let dtype = py_value.bind(py).get_type().unbind().into();
            (py_value, dtype)
        }
        ast::CustomValueKind::Bool => {
            let normalized = value.raw.eq_ignore_ascii_case("TRUE");
            let py_bool_bound = PyBool::new(py, normalized);
            let dtype = py_bool_bound.get_type().unbind().into();
            let py_bool: Py<PyAny> = py_bool_bound.unbind().into();
            (py_bool, dtype)
        }
        ast::CustomValueKind::Amount => {
            let raw = value.raw.trim();
            let (number, currency) = parse_amount_tokens(raw)
                .ok_or_else(|| PyValueError::new_err(format!("invalid amount `{}`", raw)))?;
            let amount = amount_from_number_and_currency(py, cache, number.trim(), currency.trim())?;
            let dtype = amount.bind(py).get_type().unbind().into();
            (amount, dtype)
        }
        ast::CustomValueKind::Number => {
            let decimal = py_decimal(py, cache, value.raw.trim())?;
            let dtype = decimal.bind(py).get_type().unbind().into();
            (decimal, dtype)
        }
        ast::CustomValueKind::Account => {
            let py_value: Py<PyAny> = PyString::new(py, value.raw.trim()).unbind().into();
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
    // Tree-sitter grammar expects a trailing newline; ensure we feed one by
    // copying the input into an owned string and appending '\n' when missing.
    let mut owned_content = content.to_owned();
    if !owned_content.ends_with('\n') {
        owned_content.push('\n');
    }

    let options_map = default_options_map(py)?;
    options_map.set_item("filename", filename)?;

    match parse_str(&owned_content, filename) {
        Ok(directives) => match normalize_directives(directives) {
            Ok(normalized) => {
                let (includes, filtered, options, plugins) = partition_directives(normalized);
                options_map.set_item("include", PyList::new(py, &includes)?)?;
                apply_options(py, &options_map, &options)?;
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
                let dcontext = options_map.get_item("dcontext")?.unwrap();
                let entries = convert_directives(py, filtered, &dcontext)?;
                let errors: Py<PyAny> = PyList::empty(py).unbind().into();
                Ok((entries, errors, options_map.unbind().into()))
            }
            Err(err) => {
                options_map.set_item("include", PyList::empty(py))?;
                apply_options(py, &options_map, &[])?;
                let errors = PyList::new(py, [build_parser_error(py, err)?])?
                    .unbind()
                    .into();
                let entries: Py<PyAny> = PyList::empty(py).unbind().into();
                Ok((entries, errors, options_map.unbind().into()))
            }
        },
        Err(err) => {
            options_map.set_item("include", PyList::empty(py))?;
            apply_options(py, &options_map, &[])?;
            let errors = PyList::new(py, [build_parser_error(py, err)?])?
                .unbind()
                .into();
            let entries: Py<PyAny> = PyList::empty(py).unbind().into();
            Ok((entries, errors, options_map.unbind().into()))
        }
    }
}

/// Convert `YYYY-MM-DD` text to `datetime.date`.
#[pyfunction]
fn py_date(py: Python<'_>, date: &str) -> PyResult<Py<PyAny>> {
    let parsed = NaiveDate::parse_from_str(date.trim(), "%Y-%m-%d")
        .map_err(|err| PyValueError::new_err(format!("invalid date `{}`: {}", date, err)))?;
    let pydate: Py<PyAny> =
        PyDate::new(py, parsed.year(), parsed.month() as u8, parsed.day() as u8)?
            .unbind()
            .into();
    Ok(pydate)
}

fn py_decimal(py: Python<'_>, cache: &DataCache, number: &str) -> PyResult<Py<PyAny>> {
    cache.number_mod.getattr(py, "D")?.call1(py, (number,))
}

fn py_amount(py: Python<'_>, cache: &DataCache, number: Py<PyAny>, currency: &str) -> PyResult<Py<PyAny>> {
    cache.amount_mod.getattr(py, "Amount")?.call1(py, (number, currency))
}

fn amount_to_py(
    py: Python<'_>,
    cache: &DataCache,
    amount: &bcore::Amount,
) -> PyResult<Py<PyAny>> {
    let d = py_decimal(py, cache, amount.number.as_str())?;
    py_amount(py, cache, d, amount.currency.as_str())
}

fn amount_from_number_and_currency(
    py: Python<'_>,
    cache: &DataCache,
    number: &str,
    currency: &str,
) -> PyResult<Py<PyAny>> {
    let d = py_decimal(py, cache, number)?;
    py_amount(py, cache, d, currency)
}

fn per_unit_price_from_total(price: &bcore::Amount, units: &bcore::Amount) -> Option<String> {
    let total = Decimal::from_str(price.number.as_str()).ok()?;
    let qty = Decimal::from_str(units.number.as_str()).ok()?;
    if qty.is_zero() {
        return None;
    }
    let per_unit = (total / qty).normalize();
    Some(per_unit.to_string())
}
