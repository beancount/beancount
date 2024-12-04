#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use pyo3::create_exception;
use pyo3::prelude::*;

mod data;
mod error;
mod parse;
mod parser;

#[pymodule]
fn __beancount(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("ParserError", m.py().get_type_bound::<ParserError>())?;

    m.add_class::<data::Booking>()?;
    m.add_class::<data::Open>()?;
    m.add_class::<data::Close>()?;
    m.add_class::<data::Pad>()?;
    m.add_class::<data::Price>()?;
    m.add_class::<data::Cost>()?;
    m.add_class::<data::CostSpec>()?;
    m.add_class::<data::Amount>()?;

    m.add_class::<data::Posting>()?;

    m.add_class::<data::Open>()?;
    m.add_class::<data::Close>()?;
    m.add_class::<data::Commodity>()?;
    m.add_class::<data::Transaction>()?;
    m.add_class::<data::Pad>()?;
    m.add_class::<data::Balance>()?;
    m.add_class::<data::Price>()?;
    m.add_class::<data::Event>()?;
    m.add_class::<data::Plugin>()?;
    m.add_class::<data::Opt>()?;
    m.add_class::<data::Custom>()?;
    m.add_class::<data::Note>()?;
    m.add_class::<data::Document>()?;
    m.add_class::<data::Query>()?;

    m.add_class::<data::Plugin>()?;
    m.add_function(wrap_pyfunction!(parse::py_parse, m)?)?;

    // parser::register_child_module(m)
    return Ok(());
}

create_exception!(
    beancount.parser._parser,
    ParserError,
    pyo3::exceptions::PyException
);
