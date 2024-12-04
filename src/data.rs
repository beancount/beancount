use crate::ParserError;
use chrono::NaiveDate;
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use pyo3::types::{PyAnyMethods, PyString, PyTuple};
use pyo3::{pyclass, pymethods, Bound, PyAny, PyResult};
use rust_decimal::Decimal;
use std::collections::{HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};

pub type Metadata = HashMap<String, String>;

pub type Currency = String;

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Open {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub account: String,
    #[pyo3(get)]
    pub currencies: Vec<Currency>,
    #[pyo3(get)]
    pub booking: Option<Booking>,
}

#[pymethods]
impl Open {
    fn __str__(&self) -> String {
        return format!(
            "Open(meta={:?}, date={:?}, account={:?}, currencies={:?}, booking={:?})",
            self.meta,
            self.date,
            self.account,
            self.currencies,
            self.booking.as_ref().map(|x| x.__str__())
        );
    }

    fn __repr__(&self) -> String {
        return self.__str__();
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Close {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub account: String,
}

#[pymethods]
impl Close {
    fn __str__(&self) -> String {
        return format!(
            "Close(meta={:?}, date={}, account={})",
            self.meta, self.date, self.account
        );
    }

    fn __repr__(&self) -> String {
        return self.__str__();
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Commodity {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub currency: Currency,
}

#[pymethods]
impl Commodity {
    fn __repr__(&self) -> String {
        return format!(
            "Commodity(meta={:?}, date={}, currency={})",
            self.meta, self.date, self.currency
        );
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Pad {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub account: String,
    #[pyo3(get)]
    pub source_account: String,
}

#[pymethods]
impl Pad {
    fn __repr__(&self) -> String {
        return format!(
            "Pad(meta={:?}, date={}, account={}, source_account={})",
            self.meta, self.date, self.account, self.source_account
        );
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Balance {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub account: String,
    #[pyo3(get)]
    pub amount: Amount,
    #[pyo3(get)]
    pub tolerance: Option<rust_decimal::Decimal>,
    #[pyo3(get)]
    pub diff_amount: Option<Amount>,
}

// #[derive(Debug, Clone)]
// #[non_exhaustive]
#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Posting {
    #[pyo3(get)]
    pub metadata: Metadata,
    #[pyo3(get)]
    pub account: String,
    #[pyo3(get)]
    pub units: Option<Amount>,
    #[pyo3(get)]
    pub cost: Option<PostingCost>,
    #[pyo3(get)]
    pub price: Option<Amount>,
    #[pyo3(get)]
    pub flag: Option<char>,

    #[pyo3(get)]
    pub source: String,
}

#[pymethods]
impl Posting {
    fn __repr__(&self) -> String {
        format!(
            "Posting(metadata={:?}, account={:?}, units={:?}, cost={:?}, price={:?}, flag={:?})",
            self.metadata, self.account, self.units, self.cost, self.price, self.flag
        )
    }
}

#[derive(Debug, Clone)]
pub enum PostingCost {
    Cost(Cost),
    CostSpec(CostSpec),
}

impl IntoPy<Py<PyAny>> for PostingCost {
    fn into_py(self, py: Python) -> Py<PyAny> {
        return match self {
            PostingCost::Cost(x) => x.into_py(py),
            PostingCost::CostSpec(x) => x.into_py(py),
        };
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Cost {
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub number: Decimal,
    #[pyo3(get)]
    pub currency: Currency,
    #[pyo3(get)]
    pub label: Option<String>,
}

#[pymethods]
impl Cost {
    #[new]
    #[pyo3(signature = (number, currency, date,  label=None))]
    fn py_new(
        number: Decimal,
        currency: Currency,
        date: NaiveDate,
        label: Option<String>,
    ) -> PyResult<Self> {
        return Ok(Cost {
            date,
            number,
            currency,
            label,
        });
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct CostSpec {
    #[pyo3(get)]
    pub number_per: Option<Decimal>,
    #[pyo3(get)]
    pub number_total: Option<Decimal>,
    #[pyo3(get)]
    pub currency: Option<Currency>,
    #[pyo3(get)]
    pub date: Option<NaiveDate>,
    #[pyo3(get)]
    pub label: Option<String>,
    #[pyo3(get)]
    pub merge: Option<bool>,
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Transaction {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub flag: char,
    #[pyo3(get)]
    pub payee: Option<String>,
    #[pyo3(get)]
    pub narration: String,
    #[pyo3(get)]
    pub tags: HashSet<String>,
    #[pyo3(get)]
    pub links: HashSet<String>,
    #[pyo3(get)]
    pub postings: Vec<Posting>,
}

#[pymethods]
impl Transaction {
    fn __rich__(&self) -> String {
        return format!("{:#?}", self);
    }

    fn __repr__(&self) -> String {
        return format!(
            "Transaction(meta={:?}, date={:?}, flag={:?}, payee={:?}, narration={:?}, tags={:?}, links={:?}, postings={:?})",
            self.meta,
            self.date,
            self.flag,
            self.payee.clone().unwrap_or("None".to_string()),
            self.narration,
            self.tags,
            self.links,
            self.postings
        );
    }
}

// #[pymethods]
// impl Posting {
//     #[new]
//     #[pyo3(signature = (flag, account, amount=None, cost=None, price=None, metadata=None))]
//     fn new(
//         flag: Option<char>,
//         account: String,
//         amount: Option<Amount>,
//         cost: Option<Cost>,
//         price: Option<PostingPrice>,
//         metadata: Option<Metadata>,
//     ) -> PyResult<Self> {
//         return Ok(Posting {
//             flag,
//             account,
//             amount,
//             cost,
//             price,
//             metadata: metadata.unwrap_or_else(|| Metadata::new()),
//         });
//     }
// }

#[pyclass(module = "beancount.__beancount", frozen)]
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Amount {
    /// The value (decimal) part
    #[pyo3(get)]
    pub number: Option<Decimal>,
    /// Currency
    #[pyo3(get)]
    pub currency: Currency,
}

#[pymethods]
impl Amount {
    #[new]
    #[pyo3(signature=(number, currency))]
    fn new(number: Option<Decimal>, currency: String) -> PyResult<Self> {
        return Ok(Amount { number, currency });
    }

    fn __str__(&self) -> String {
        match self.number {
            Some(n) => {
                return format!("{} {}", n, self.currency).to_string();
            }
            None => {
                return format!("None {}", self.currency).to_string();
            }
        }
    }

    fn to_string<'py>(
        &self,
        py: Python<'py>,
        fmt: &Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        match self.number {
            Some(n) => {
                let format: Bound<'py, PyAny> = fmt.getattr("format")?.into();
                let args = PyTuple::new_bound(py, &[n]);
                let mut s = format.call1(args)?.extract::<String>()?;
                s.push(' ');
                s.push_str(&self.currency);
                return Ok(s.into_py(py).into_bound(py));
            }
            None => {
                return Ok(format!("None {}", self.currency)
                    .to_string()
                    .into_py(py)
                    .into_bound(py));
            }
        }
    }

    fn __lt__(&self, other: &Self) -> PyResult<bool> {
        if self.currency != other.currency {
            return Ok(self.currency < other.currency);
        }

        return match (self.number, other.number) {
            (Some(a), Some(b)) => Ok(a < b),
            _ => Err(PyTypeError::new_err(
                "'<' not supported between instances of 'NoneType' and 'decimal.Decimal'"
                    .to_string(),
            )),
        };
    }

    fn __eq__(&self, other: &Self) -> bool {
        return self == other;
    }

    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        return hasher.finish();
    }

    fn __neg__(&self) -> Self {
        return Self {
            number: self.number.clone().map(|n| -n),
            currency: self.currency.clone(),
        };
    }

    // fn __hash__() {}
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Price {
    #[pyo3(get)]
    pub meta: Metadata, // PyDict
    #[pyo3(get)]
    pub date: NaiveDate, // PyDate
    #[pyo3(get)]
    pub currency: Currency,
    #[pyo3(get)]
    pub amount: Amount,
}

#[pymethods]
impl Price {
    #[new]
    fn new(meta: Metadata, date: NaiveDate, currency: String, amount: Amount) -> Self {
        Price {
            meta,
            date,
            currency,
            amount,
        }
    }
}

#[allow(deprecated)]
#[pyclass(frozen)]
#[derive(Debug, Clone, PartialEq)]
pub enum Booking {
    STRICT,
    #[allow(non_camel_case_types)]
    STRICT_WITH_SIZE,
    None,
    AVERAGE,
    FIFO,
    LIFO,
    HIFO,
}
impl TryFrom<String> for Booking {
    type Error = PyErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "STRICT" => Ok(Booking::STRICT),
            "STRICT_WITH_SIZE" => Ok(Booking::STRICT_WITH_SIZE),
            "None" => Ok(Booking::None),
            "AVERAGE" => Ok(Booking::AVERAGE),
            "FIFO" => Ok(Booking::FIFO),
            "LIFO" => Ok(Booking::LIFO),
            "HIFO" => Ok(Booking::HIFO),
            _ => Err(ParserError::new_err(format!(
                "Invalid booking type: {}",
                value
            ))),
        }
    }
}

impl TryFrom<&str> for Booking {
    type Error = PyErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "STRICT" => Ok(Booking::STRICT),
            "STRICT_WITH_SIZE" => Ok(Booking::STRICT_WITH_SIZE),
            "None" => Ok(Booking::None),
            "AVERAGE" => Ok(Booking::AVERAGE),
            "FIFO" => Ok(Booking::FIFO),
            "LIFO" => Ok(Booking::LIFO),
            "HIFO" => Ok(Booking::HIFO),
            _ => Err(ParserError::new_err(format!(
                "Invalid booking type: {}",
                value
            ))),
        }
    }
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
    fn __eq__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(s) = other.downcast::<PyString>() {
            return s.to_cow().map(|rhs| self.__str__() == rhs);
        }

        if let Ok(b) = other.downcast::<Self>() {
            return Ok(self == b.get());
        }

        return Ok(false);
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

// #[pymethods]
// impl Cost {
//     #[new]
//     #[pyo3(signature = (meta, date, currency, label=None))]
//     fn new(
//         meta: Metadata,
//         date: NaiveDate,
//         currency: &Bound<'_, PyString>,
//         label: Option<String>,
//     ) -> PyResult<Self> {
//         return Ok(Cost {
//             date,
//             currency: currency.extract()?,
//             label,
//         });
//     }
// }
#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Document {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub account: String,
    #[pyo3(get)]
    pub filename: String,
    #[pyo3(get)]
    pub tags: HashSet<String>,
    #[pyo3(get)]
    pub link: HashSet<String>,
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Event {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub description: String,
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Plugin {
    #[pyo3(get)]
    pub module: String,
    #[pyo3(get)]
    pub config: Option<String>,
}

#[pymethods]
impl Plugin {
    // fn __repr__(&self) -> String {
    // return format!("Plugin(module={:?}, config={:?})", self.module, self.config);
    // }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Custom {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub values: Vec<String>,
}

#[pymethods]
impl Custom {
    fn __repr__(&self) -> String {
        return format!(
            "Custom(meta={:?}, date={:?}, name={:?}, values={:?})",
            self.meta, self.date, self.name, self.values
        );
    }
}

#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Note {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub account: String,
    #[pyo3(get)]
    pub comment: String,
    #[pyo3(get)]
    pub tags: HashSet<String>,
    #[pyo3(get)]
    pub link: HashSet<String>,
}

#[pymethods]
impl Note {
    fn __repr__(&self) -> String {
        return format!(
            "Note(meta={:?}, date={:?}, account={:?}, comment={:?}, tags={:?}, link={:?})",
            self.meta, self.date, self.account, self.comment, self.tags, self.link
        );
    }
}
#[pyclass(module = "beancount.__beancount")]
#[derive(Debug, Clone)]
pub struct Query {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub date: NaiveDate,
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub query_string: String,
}

#[pyclass(frozen)]
#[derive(Debug, Clone)]
pub struct Opt {
    #[pyo3(get)]
    pub meta: Metadata,
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub value: String,
}

#[pymethods]
impl Opt {
    fn __str__(&self) -> String {
        return format!("Option(name={:?}, value={:?}", self.name, self.value);
    }

    fn __repr__(&self) -> String {
        return self.__str__();
    }
}
