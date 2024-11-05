use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyString;
use pyo3::{Bound, PyAny, PyResult};
use rust_decimal::prelude::FromPrimitive;
use std::str::FromStr;

#[pyclass]
#[derive(Debug, Clone)]
pub struct Decimal(rust_decimal::Decimal);

#[pymethods]
impl Decimal {
    // For `__repr__` we want to return a string that Python code could use to recreate
    // the `Number`, like `Number(5)` for example.
    fn __repr__(&self) -> String {
        // We use the `format!` macro to create a string. Its first argument is a
        // format string, followed by any number of parameters which replace the
        // `{}`'s in the format string.
        //
        //                       👇 Tuple field access in Rust uses a dot
        format!("Decimal({})", self.0)
    }
    // `__str__` is generally used to create an "informal" representation, so we
    // just forward to `i32`'s `ToString` trait implementation to print a bare number.
    fn __str__(&self) -> String {
        self.0.to_string()
    }
}

impl Decimal {
    pub(crate) fn from_py(o: &Bound<'_, PyAny>) -> PyResult<Self> {
        if let Ok(s) = o.downcast::<PyString>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from_str(s.to_string().as_str()).or_else(|_| Err(PyValueError::new_err("Invalid decimal")))?,
            });
        }
        if let Ok(s) = o.extract::<i64>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from(s),
            });
        }

        if let Ok(s) = o.extract::<f64>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from_f64(s).ok_or_else(|| PyValueError::new_err("Invalid decimal"))?,
            });
        }

        if let Ok(s) = o.extract::<f32>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from_f32(s).ok_or_else(|| PyValueError::new_err("Invalid decimal"))?,
            });
        }

        Err(pyo3::exceptions::PyTypeError::new_err("Invalid decimal"))
    }
}
//
// impl PartialEq<Self> for Decimal {
//     fn eq(&self, other: &Self) -> bool {
//         todo!()
//     }
// }
//
// impl PartialOrd for Decimal {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         todo!()
//     }
// }