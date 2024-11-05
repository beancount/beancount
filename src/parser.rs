use crate::base::Amount;
use pyo3::create_exception;
use pyo3::prelude::*;

pub fn register_child_module(parent_module: &Bound<'_, PyModule>) -> PyResult<()> {
    let m = PyModule::new_bound(parent_module.py(), "_parser")?;
    m.add("ParserError", m.py().get_type_bound::<ParserError>())?;

    // m.add_class::<Transaction>()?;
    m.add_class::<Amount>()?;

    parent_module.add_submodule(&m)
}

create_exception!(
    beancount.parser._parser,
    ParserError,
    pyo3::exceptions::PyException
);

// #[pyclass(subclass, module = "beancount.parser._parser")]
// #[derive(Clone, Debug)]
// struct Parser {
//     name: String,
// }
//
// #[allow(missing_docs)]
// #[derive(Debug, Clone, PartialEq)]
// #[non_exhaustive]
// pub enum DirectiveContent<Decimal> {
//     Transaction(Transaction),
//     Price(Price<Decimal>),
//     Balance(Balance<Decimal>),
//     Open(Open),
//     Close(Close),
//     Pad(Pad),
//     Commodity(Currency),
//     Event(Event),
// }
//
// #[pyclass(module = "beancount.parser._parser")]
// #[derive(Debug, Clone, PartialEq)]
// pub struct Transaction {
//     /// Transaction flag (`*` or `!` or `None` when using the `txn` keyword)
//     pub flag: Option<char>,
//     /// Payee (if present)
//     pub payee: Option<String>,
//     /// Narration (if present)
//     pub narration: Option<String>,
//     /// Set of tags
//     pub tags: HashSet<Tag>,
//     /// Set of links
//     pub links: HashSet<Link>,
//     /// Postings
//     pub postings: Vec<Posting<decimal::Decimal>>,
// }
//
// #[pymethods]
// impl Transaction {
//     #[new]
//     fn new(
//         flag: Option<char>,
//         payee: Option<String>,
//         narration: Option<String>,
//         tags: Option<HashSet<Tag>>,
//         links: Option<HashSet<Link>>,
//         postings: Option<Vec<Posting<decimal::Decimal>>>,
//     ) -> PyResult<Self> {
//         return Ok(Transaction {
//             flag,
//             payee,
//             narration,
//             tags: tags.or_else(HashSet::<Tag>::new()),
//             links: links.or_else(HashSet::<Link>::new()),
//             postings: postings.or_else(Vec::<Posting<decimal::Decimal>>::new()),
//         });
//     }
// }
//
// #[pymethods]
// impl Parser {
//     #[new]
//     fn new() -> PyResult<Self> {
//         return Ok(Parser {
//             name: String::from("test"),
//         });
//     }
//
//     // fn parse(&self, content: &str) -> PyResult<Vec<Directive<Decimal>>> {
//     //     let result = content.parse::<BeancountFile<Decimal>>();
//     //     match result {
//     //         Ok(file) => {
//     //             Ok(file.directives)
//     //         }
//     //         Err(err) => {
//     //             Err(ParserError::new_err(err.to_string()))
//     //         }
//     //     }
//     // }
// }
// // fn parse(content: &str) -> PyResult<Vec<Directive<Decimal>>> {
// //     let result = content.parse::<BeancountFile<Decimal>>();
// //     match result {
// //         Ok(file) => Ok(file.directives),
// //         Err(err) => Err(ParserError::new_err(err.to_string())),
// //     }
// // }
