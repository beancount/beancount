use crate::{base, ParserError};
use beancount_parser::BeancountFile;
use pyo3::prelude::*;
use pyo3::types::{PyDate, PyDateAccess, PyDict, PyString};
use crate::parser::DirectiveContent::Price;
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

#[pyclass(frozen)]
pub struct Opt {
    #[pyo3(get)]
    pub name: Py<PyString>,
    #[pyo3(get)]
    pub value: Py<PyString>,
}

#[pymethods]
impl Opt {
    fn __str__(&self, py: Python<'_>) -> String {
        return format!(
            "Option(name={:?}, value={:?}",
            self.name.to_str(py).unwrap(),
            self.value.to_str(py).unwrap()
        );
    }

    fn __repr__(&self, py: Python<'_>) -> String {
        return self.__str__(py);
    }
}

#[derive(Debug)]
pub enum DirectiveContent {
    Transaction(base::Posting),
    Price(base::Price),
    // Balance(base::Balance),
    // Open(base::Open),
    // Close(base::Close),
    // Pad(base::Pad),
    Commodity(base::Currency),
    // Event(base::Event),
}

impl DirectiveContent {
    fn from_parser(py: Python<'_>, x: &beancount_parser::Directive<rust_decimal::Decimal>) -> PyResult<Self> {
        return match &x.content {
            // beancount_parser::DirectiveContent::Transaction(v) => {
            //     DirectiveContent::Transaction(base::Posting {
            //         flag: v.flag,
            //         account: PyString::new_bound(v.payee.unwrap().to_string())?.unbind(),
            //         amount: None,
            //         cost: None,
            //         price: None,
            //         metadata: Default::default(),
            //     })
            // }
            // beancount_parser::DirectiveContent::Price(v) => {
            // Ok(base::Price )
            // }
            // beancount_parser::DirectiveContent::Balance(_) => {}
            // beancount_parser::DirectiveContent::Open(_) => {}
            // beancount_parser::DirectiveContent::Close(_) => {}
            // beancount_parser::DirectiveContent::Pad(_) => {}
            beancount_parser::DirectiveContent::Commodity(v) => {
                Ok(Self::Commodity(v.to_string()))
            }
            // beancount_parser::DirectiveContent::Event(_) => {}
            _ => {
                Ok(Self::Commodity(format!("{:?}", x.content)))
            }
        };
    }
}

#[pyclass]
#[derive(Debug)]
pub struct Directive {
    /// Date of the directive
    pub date: Py<PyDate>,
    /// Content of the directive that is specific to each directive type
    pub content: DirectiveContent,
    /// Metadata associated to the directive
    pub metadata: Py<PyDict>,
    /// Line number where the directive was found in the input file
    pub line_number: u32,
}

#[pymethods]
impl Directive {
    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        return Ok(format!(
            "Directive(date={:}, content={:?}, metadata={:}, line_number={:?})",
            self.date.bind(py).repr()?.to_string(),
            self.content,
            self.metadata.bind(py).repr()?.to_string(),
            self.line_number
        ));
    }
}

#[pyclass]
pub struct File {
    #[pyo3(get)]
    pub includes: Vec<Py<PyString>>,

    #[pyo3(get)]
    pub options: Vec<Py<Opt>>,

    #[pyo3(get)]
    pub directives: Vec<Py<Directive>>,
}

#[pyfunction]
pub fn parse(py: Python<'_>, content: &str) -> PyResult<File> {
    let result = content.parse::<BeancountFile<rust_decimal::Decimal>>();
    return match result {
        Ok(bean) => {
            // println!("includes: {:#?}", bean.includes);
            // println!("directives: {:#?}", bean.directives);
            println!("options: {:#?}", bean.options);
            let options = bean
                .options
                .iter()
                .map(|x| {
                    Py::new(
                        py,
                        Opt {
                            name: PyString::new_bound(py, x.name.as_str()).unbind(),
                            value: PyString::new_bound(py, x.value.as_str()).unbind(),
                        },
                    )
                })
                .collect::<Result<Vec<_>, PyErr>>()?;

            let directives = bean
                .directives
                .iter()
                .map(|x| {
                    Py::new(
                        py,
                        Directive {
                            date: PyDate::new_bound(
                                py,
                                x.date.year.into(),
                                x.date.month.into(),
                                x.date.day.into(),
                            )?
                                .unbind(),
                            content: DirectiveContent::from_parser(py, x)?,
                            metadata: PyDict::new_bound(py).unbind(),
                            line_number: x.line_number,
                        },
                    )
                })
                .collect::<Result<Vec<_>, PyErr>>()?;

            Ok(File {
                includes: bean
                    .includes
                    .iter()
                    .map(|x| PyString::new_bound(py, x.to_str().unwrap()).unbind())
                    .collect(),
                options,
                directives,
            })
        }
        Err(err) => Err(ParserError::new_err(err.to_string())),
    };
}
