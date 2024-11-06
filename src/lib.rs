use pyo3::create_exception;
use pyo3::prelude::*;

mod base;
mod decimal;
mod parser;

#[pymodule]
fn __beancount(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("ParserError", m.py().get_type_bound::<ParserError>())?;

    m.add_class::<base::Booking>()?;

    m.add_class::<base::Price>()?;
    m.add_class::<base::Amount>()?;
    m.add_class::<base::PostingPrice>()?;

    m.add_class::<base::Cost>()?;
    m.add_class::<base::PostingPrice>()?;
    m.add_class::<base::Posting>()?;
    m.add_function(wrap_pyfunction!(parser::parse, m)?)?;

    // parser::register_child_module(m)
    return Ok(());
}

create_exception!(
    beancount.parser._parser,
    ParserError,
    pyo3::exceptions::PyException
);
