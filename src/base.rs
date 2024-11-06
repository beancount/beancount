use crate::decimal::Decimal;
use pyo3::prelude::*;
use pyo3::types::{PyAnyMethods, PyDate, PyDict};
use pyo3::{pyclass, pymethods, Bound, Py, PyAny, PyResult};
use std::collections::HashMap;

type Metadata = HashMap<String, String>;

#[pyclass]
#[derive(Debug, Clone, PartialEq)]
pub struct Amount {
    /// The value (decimal) part
    #[pyo3(get)]
    pub number: Decimal,
    /// Currency
    #[pyo3(get)]
    pub currency: String,
}

#[pymethods]
impl Amount {
    #[new]
    fn new(value: &Bound<'_, PyAny>, currency: &Bound<'_, PyAny>) -> PyResult<Self> {
        return Ok(Amount {
            number: Decimal::from_py(value)?,
            currency: currency.extract()?,
        });
    }
}

#[pyclass]
pub struct Price {
    #[pyo3(get)]
    pub meta: Py<PyDict>, // PyDict
    #[pyo3(get)]
    pub date: Py<PyDate>, // PyDate
    #[pyo3(get)]
    pub currency: String,
    #[pyo3(get)]
    pub amount: Amount,
}

#[pymethods]
impl Price {
    #[new]
    fn new(
        meta: &Bound<'_, PyDict>,
        date: &Bound<'_, PyDate>,
        currency: String,
        amount: Amount,
    ) -> Self {
        Price {
            meta: meta.clone().unbind(),
            date: date.clone().unbind(),
            currency,
            amount,
        }
    }
}

#[pyclass]
#[derive(Debug, Clone)]
pub enum PostingPrice {
    /// Unit cost (`@`)
    Unit(Amount),
    /// Total cost (`@@`)
    Total(Amount),
}

#[pyclass]
#[derive(PartialEq)]
pub enum Booking {
    STRICT,
    STRICT_WITH_SIZE,
    None,
    AVERAGE,
    FIFO,
    LIFO,
    HIFO,
}

#[pymethods]
impl Booking {
    // make it behave like enum.Enum
    #[getter]
    fn name(&self) -> &str {
        return self.__str__();
    }

    #[getter]
    fn value(&self) -> &str {
        return self.__str__();
    }

    // to support both `Booking.STRICT == Booking.STRICT` and `Booking.STRICT == "STRICT"`
    fn __eq__(&self, other: &Bound<'_, PyAny>) -> bool {
        if let Ok(s) = other.extract::<&str>() {
            return s == self.__str__();
        }

        if let Ok(b) = other.downcast::<Self>() {
            let other = &*b.borrow();
            return self == other;
        }

        return false;
    }

    fn __str__(&self) -> &str {
        match self {
            Booking::STRICT => "STRICT",
            Booking::STRICT_WITH_SIZE => "STRICT_WITH_SIZE",
            Booking::None => "None",
            Booking::AVERAGE => "AVERAGE",
            Booking::FIFO => "FIFO",
            Booking::LIFO => "LIFO",
            Booking::HIFO => "HIFO",
        }
    }
}

//
// #[derive(Debug, Clone)]
// #[non_exhaustive]
// pub struct Posting {
//     /// Transaction flag (`*` or `!` or `None` when absent)
//     pub flag: Option<char>,
//     /// Account modified by the posting
//     pub account: Account,
//     /// Amount being added to the account
//     pub amount: Option<Amount>,
//     /// Cost (content within `{` and `}`)
//     pub cost: Option<Cost<Decimal>>,
//     /// Price (`@` or `@@`) syntax
//     pub price: Option<PostingPrice>,
//     /// The metadata attached to the posting
//     pub metadata: HashMap<Key, Value<Decimal>>,
// }
