use crate::decimal::Decimal;
use pyo3::types::PyAnyMethods;
use pyo3::{pyclass, pymethods, Bound, PyAny, PyResult};

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

#[derive(Debug, Clone, PartialEq)]
pub struct Price {
    pub currency: String,
    pub amount: Amount,
}

//
// #[derive(Debug, Clone)]
// pub enum PostingPrice {
//     /// Unit cost (`@`)
//     Unit(Amount),
//     /// Total cost (`@@`)
//     Total(Amount),
// }
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
