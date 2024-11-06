use crate::ParserError;
use beancount_parser::BeancountFile;
use pyo3::prelude::*;
use pyo3::types::PyString;
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
        return format!("Option(name={:?}, value={:?}", self.name.to_str(py).unwrap(), self.value.to_str(py).unwrap());
    }

    fn __repr__(&self, py: Python<'_>) -> String {
        return self.__str__(py);
    }
}
#[pyclass]
pub struct File {
    #[pyo3(get)]
    pub includes: Vec<Py<PyString>>,
    #[pyo3(get)]
    pub options: Vec<Py<Opt>>,
}

#[pyfunction]
pub fn parse(py: Python<'_>, content: &str) -> PyResult<File> {
    let result = content.parse::<BeancountFile<rust_decimal::Decimal>>();
    return match result {
        Ok(bean) => {
            // println!("includes: {:#?}", bean.includes);
            // println!("directives: {:#?}", bean.directives);
            println!("options: {:#?}", bean.options);
            let options = bean.options.iter().map(|x| {
                Py::new(py, Opt {
                    name: PyString::new_bound(py, x.name.as_str()).unbind(),
                    value: PyString::new_bound(py, x.value.as_str()).unbind(),
                })
            }).collect::<Result<Vec<_>, PyErr>>()?;

            Ok(File {
                includes: bean.includes.iter().map(|x| {
                    PyString::new_bound(py, x.to_str().unwrap()).unbind()
                }).collect(),
                options,
            })
        }
        Err(err) => {
            Err(ParserError::new_err(err.to_string()))
        }
    };
}
