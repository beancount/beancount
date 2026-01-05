use beancount_parser::ParseError;
use beancount_parser::ast;
use beancount_parser::ast::Directive;
use beancount_parser::parse_str;
use chrono::{Datelike, NaiveDate};
use pyo3::BoundObject;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyDate, PyDict, PyFrozenSet, PyList, PyString};

#[pymodule]
fn _parser_rust(py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    DATA_CACHE.get_or_try_init(py, || DataCache::init(py))?;
    m.add_function(wrap_pyfunction!(load_file, m)?)?;
    m.add_function(wrap_pyfunction!(parse_string, m)?)?;
    m.add_function(wrap_pyfunction!(build_options_map, m)?)?;
    m.add_function(wrap_pyfunction!(py_date, m)?)?;
    Ok(())
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
    options_defaults: Py<PyAny>,
    parser_error_cls: Py<PyAny>,
}

impl DataCache {
    fn init(py: Python<'_>) -> PyResult<Self> {
        let data_mod = py.import("beancount.core.data")?;
        let number_mod = py.import("beancount.core.number")?;
        let amount_mod = py.import("beancount.core.amount")?;
        let options_mod = py.import("beancount.parser.options")?;
        let grammar_mod = py.import("beancount.parser.grammar")?;

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
            options_defaults: options_mod.getattr("OPTIONS_DEFAULTS")?.unbind(),
            parser_error_cls: grammar_mod.getattr("ParserError")?.unbind(),
        })
    }
}

fn cache(py: Python<'_>) -> PyResult<&'static DataCache> {
    DATA_CACHE.get_or_try_init(py, || DataCache::init(py))
}

/// Expose the Rust parser to Python, matching `beancount.loader.load_file`.

#[pyfunction]
fn load_file(py: Python<'_>, filename: &str) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
    let content = std::fs::read_to_string(filename)
        .map_err(|err| PyValueError::new_err(format!("failed to read {}: {}", filename, err)))?;
    parse_source(py, filename, &content)
}

#[pyfunction]
#[pyo3(signature = (content, filename = "<memory>"))]
fn parse_string(
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

fn build_parser_error(py: Python<'_>, err: ParseError) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let kv = PyDict::new(py);
    kv.set_item("column", err.column)?;
    let meta = cache
        .new_metadata
        .call1(py, (err.filename.clone(), err.line, kv))?;
    let parser_error_cls = cache.parser_error_cls.as_ref();
    parser_error_cls.call1(py, (meta, err.message))
}

fn unquote(text: &str) -> String {
    let trimmed = text.trim();
    if trimmed.len() >= 2 {
        let bytes = trimmed.as_bytes();
        let start = bytes[0];
        let end = bytes[trimmed.len() - 1];
        if (start == b'"' && end == b'"') || (start == b'\'' && end == b'\'') {
            return trimmed[1..trimmed.len() - 1].to_string();
        }
    }
    trimmed.to_string()
}

fn partition_directives<'a>(directives: Vec<Directive<'a>>) -> (Vec<String>, Vec<Directive<'a>>) {
    let mut includes = Vec::new();
    let mut filtered = Vec::new();

    for directive in directives {
        match directive {
            Directive::Include(include) => includes.push(unquote(include.filename)),
            Directive::Option(_)
            | Directive::Plugin(_)
            | Directive::Pushtag(_)
            | Directive::Poptag(_)
            | Directive::Pushmeta(_)
            | Directive::Popmeta(_) => {}
            Directive::Raw(_) => {}
            other => filtered.push(other),
        }
    }

    (includes, filtered)
}

fn convert_directives<'a>(py: Python<'_>, directives: Vec<Directive<'a>>) -> PyResult<Py<PyAny>> {
    let entries: Vec<Py<PyAny>> = directives
        .iter()
        .filter_map(|directive| convert_directive(py, directive).transpose())
        .collect::<PyResult<_>>()?;
    Ok(PyList::new(py, entries)?.unbind().into())
}

fn convert_directive<'a>(py: Python<'_>, directive: &Directive<'a>) -> PyResult<Option<Py<PyAny>>> {
    match directive {
        Directive::Open(open) => convert_open(py, open).map(Some),
        Directive::Transaction(txn) => convert_transaction(py, txn).map(Some),
        Directive::Close(close) => convert_close(py, close).map(Some),
        Directive::Balance(balance) => convert_balance(py, balance).map(Some),
        Directive::Pad(pad) => convert_pad(py, pad).map(Some),
        Directive::Commodity(commodity) => convert_commodity(py, commodity).map(Some),
        Directive::Price(price) => convert_price(py, price).map(Some),
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
    kv.set_item("column", meta.column)?;
    Ok(new_metadata.call1(py, (meta.filename.clone(), meta.line, kv))?)
}

fn meta_extra<'py, 'a>(py: Python<'py>, key_values: &[ast::KeyValue<'a>]) -> PyResult<Option<Bound<'py, PyDict>>> {
    if key_values.is_empty() {
        return Ok(None);
    }

    let kv = PyDict::new(py);
    for item in key_values {
        match &item.value {
            ast::KeyValueValue::String(s) => kv.set_item(item.key, s.as_str())?,
            ast::KeyValueValue::Raw(v) => kv.set_item(item.key, *v)?,
        }
    }

    Ok(Some(kv))
}

fn convert_open(py: Python<'_>, open: &ast::Open<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.open_cls;
    let currencies = PyList::new(py, open.currencies.iter().copied())?;
    let booking = match open.opt_booking {
        Some(name) => cache.booking_enum.getattr(py, name)?,
        None => py.None(),
    };
    let meta = make_metadata(py, &open.meta, meta_extra(py, &open.key_values)?)?;
    let date = py_date(py, open.date)?;
    Ok(cls.call1(py, (meta, date, open.account, currencies, booking))?)
}

fn convert_close(py: Python<'_>, close: &ast::Close<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.close_cls;
    let meta = make_metadata(py, &close.meta, meta_extra(py, &close.key_values)?)?;
    let date = py_date(py, close.date)?;
    Ok(cls.call1(py, (meta, date, close.account))?)
}

fn convert_balance(py: Python<'_>, balance: &ast::Balance<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.balance_cls;
    let meta = make_metadata(py, &balance.meta, meta_extra(py, &balance.key_values)?)?;
    let date = py_date(py, balance.date)?;
    let amount = amount_to_py(py, cache, &balance.amount)?;
    Ok(cls.call1(
        py,
        (meta, date, balance.account, amount, py.None(), py.None()),
    )?)
}

fn convert_pad(py: Python<'_>, pad: &ast::Pad<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.pad_cls;
    let meta = make_metadata(py, &pad.meta, meta_extra(py, &pad.key_values)?)?;
    let date = py_date(py, pad.date)?;
    Ok(cls.call1(py, (meta, date, pad.account, pad.from_account))?)
}

fn convert_transaction(py: Python<'_>, txn: &ast::Transaction<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.transaction_cls;

    let meta = make_metadata(py, &txn.meta, meta_extra(py, &txn.key_values)?)?;
    let date = py_date(py, txn.date)?;

    let flag = txn
        .txn
        .map(|flag| PyString::new(py, flag).unbind().into())
        .unwrap_or_else(|| py.None());
    let payee = txn
        .payee
        .as_deref()
        .map(|p| PyString::new(py, p).unbind().into())
        .unwrap_or_else(|| py.None());

    let tags = PyFrozenSet::new(py, txn.tags.iter().copied())?;
    let links = PyFrozenSet::new(py, txn.links.iter().copied())?;

    let postings: Vec<Py<PyAny>> = txn
        .postings
        .iter()
        .map(|posting| convert_posting(py, posting))
        .collect::<PyResult<_>>()?;
    let postings = PyList::new(py, postings)?;

    Ok(cls.call1(
        py,
        (
            meta,
            date,
            flag,
            payee,
            txn.narration.as_str(),
            tags,
            links,
            postings,
        ),
    )?)
}

fn convert_posting(py: Python<'_>, posting: &ast::Posting<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.posting_cls;
    let units = posting
        .amount
        .as_ref()
        .map(|amount| amount_to_py(py, cache, amount))
        .transpose()?;
    let units = units.unwrap_or_else(|| py.None());
    let price = posting
        .price_annotation
        .as_ref()
        .map(|price| amount_to_py(py, cache, price))
        .transpose()?;
    let price = price.unwrap_or_else(|| py.None());
    let flag = posting
        .opt_flag
        .map(|flag| PyString::new(py, flag).unbind().into())
        .unwrap_or_else(|| py.None());

    Ok(cls.call1(
        py,
        (posting.account, units, py.None(), price, flag, py.None()),
    )?)
}

fn convert_commodity(py: Python<'_>, commodity: &ast::Commodity<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.commodity_cls;
    let meta = make_metadata(py, &commodity.meta, meta_extra(py, &commodity.key_values)?)?;
    let date = py_date(py, commodity.date)?;
    Ok(cls.call1(py, (meta, date, commodity.currency))?)
}

fn convert_price(py: Python<'_>, price: &ast::Price<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.price_cls;
    let meta = make_metadata(py, &price.meta, meta_extra(py, &price.key_values)?)?;
    let date = py_date(py, price.date)?;
    let amount = amount_to_py(py, cache, &price.amount)?;
    Ok(cls.call1(py, (meta, date, price.currency, amount))?)
}

fn convert_event(py: Python<'_>, event: &ast::Event<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.event_cls;
    let meta = make_metadata(py, &event.meta, meta_extra(py, &event.key_values)?)?;
    let date = py_date(py, event.date)?;
    Ok(cls.call1(py, (meta, date, event.event_type, event.desc))?)
}

fn convert_query(py: Python<'_>, query: &ast::Query<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.query_cls;
    let meta = make_metadata(py, &query.meta, meta_extra(py, &query.key_values)?)?;
    let date = py_date(py, query.date)?;
    Ok(cls.call1(py, (meta, date, query.name, query.query))?)
}

fn convert_note(py: Python<'_>, note: &ast::Note<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.note_cls;
    let meta = make_metadata(py, &note.meta, meta_extra(py, &note.key_values)?)?;
    let date = py_date(py, note.date)?;
    Ok(cls.call1(
        py,
        (meta, date, note.account, note.note, py.None(), py.None()),
    )?)
}

fn convert_document(py: Python<'_>, doc: &ast::Document<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.document_cls;
    let meta = make_metadata(py, &doc.meta, meta_extra(py, &doc.key_values)?)?;
    let date = py_date(py, doc.date)?;
    let tags = PyFrozenSet::new(py, doc.tags.iter().copied())?;
    let links = PyFrozenSet::new(py, doc.links.iter().copied())?;
    Ok(cls.call1(
        py,
        (
            meta,
            date,
            doc.account,
            doc.filename,
            tags,
            links,
        ),
    )?)
}

fn convert_custom(py: Python<'_>, custom: &ast::Custom<'_>) -> PyResult<Py<PyAny>> {
    let cache = cache(py)?;
    let cls = &cache.custom_cls;
    let meta = make_metadata(py, &custom.meta, meta_extra(py, &custom.key_values)?)?;
    let date = py_date(py, custom.date)?;
    let values = PyList::new(py, custom.values.iter().copied())?;
    Ok(cls.call1(py, (meta, date, custom.name, values, py.None()))?)
}

fn parse_source(
    py: Python<'_>,
    filename: &str,
    content: &str,
) -> PyResult<(Py<PyAny>, Py<PyAny>, Py<PyAny>)> {
    let options_map = default_options_map(py)?;
    options_map.set_item("filename", filename)?;

    match parse_str(content, filename) {
        Ok(directives) => {
            let (includes, filtered) = partition_directives(directives);
            options_map.set_item("include", PyList::new(py, &includes)?)?;
            let entries = convert_directives(py, filtered)?;
            let errors: Py<PyAny> = PyList::empty(py).unbind().into();
            Ok((entries, errors, options_map.unbind().into()))
        }
        Err(err) => {
            options_map.set_item("include", PyList::empty(py))?;
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

fn amount_to_py(
    py: Python<'_>,
    cache: &DataCache,
    amount: &ast::Amount<'_>,
) -> PyResult<Py<PyAny>> {
    let number_mod = &cache.number_mod;
    let d = number_mod.getattr(py, "D")?.call1(py, (amount.number,))?;
    let amount_mod = &cache.amount_mod;
    amount_mod
        .getattr(py, "Amount")?
        .call1(py, (d, amount.currency))
}
