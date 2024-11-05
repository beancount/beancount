use crate::base::Amount;
use pyo3::create_exception;
use pyo3::prelude::*;

mod parser;
mod base;
mod decimal;

#[pymodule]
fn __beancount(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("ParserError", m.py().get_type_bound::<ParserError>())?;

    // m.add_class::<Transaction>()?;
    m.add_class::<Amount>()

    // parser::register_child_module(m)
}

create_exception!(
    beancount.parser._parser,
    ParserError,
    pyo3::exceptions::PyException
);