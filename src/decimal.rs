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
    fn __repr__(&self) -> String {
        format!("Decimal({})", self.0)
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Decimal {
    pub fn from_py(o: &Bound<'_, PyAny>) -> PyResult<Self> {
        if let Ok(s) = o.downcast::<PyString>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from_str(s.to_string().as_str())
                    .or_else(|_| Err(PyValueError::new_err("Invalid decimal")))?,
            });
        }
        if let Ok(s) = o.extract::<i64>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from(s),
            });
        }

        if let Ok(s) = o.extract::<f64>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from_f64(s)
                    .ok_or_else(|| PyValueError::new_err("Invalid decimal"))?,
            });
        }

        if let Ok(s) = o.extract::<f32>() {
            return Ok(Decimal {
                0: rust_decimal::Decimal::from_f32(s)
                    .ok_or_else(|| PyValueError::new_err("Invalid decimal"))?,
            });
        }

        Err(pyo3::exceptions::PyTypeError::new_err("Invalid decimal"))
    }
}
