use crate::decimal::Decimal;
use pyo3::types::PyString;
use pyo3::{pyclass, pymethods, Py, PyAny, PyResult};

type Currency = Py<PyString>;

#[pyclass]
#[derive(Debug)]
pub struct Amount {
    /// The value (decimal) part
    #[pyo3(get)]
    pub number: Decimal,
    /// Currency
    #[pyo3(get)]
    pub currency: Currency,
}

#[pymethods]
impl Amount {
    #[new]
    fn new(
        value: &pyo3::Bound<'_, PyAny>,
        currency: Py<PyString>,
    ) -> PyResult<Self> {
        return Ok(Amount {
            number: Decimal::from_py(value)?,
            currency,
        });
    }
}
//
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
