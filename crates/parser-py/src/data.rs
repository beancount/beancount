// this is a example thay we can implement core data with rust
// a data class would be mew easy to impl

use chrono::{Datelike, NaiveDate};
use pyo3::exceptions::{PyIndexError, PyValueError};
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyAny, PyDate, PyDict, PyList, PyString, PyTuple};

const OPEN_FIELD_NAMES: [&str; 5] = ["meta", "date", "account", "currencies", "booking"];

fn open_field_index(name: &str) -> Option<usize> {
  OPEN_FIELD_NAMES.iter().position(|field| *field == name)
}

#[allow(deprecated)]
#[allow(clippy::upper_case_acronyms)]
#[pyclass(module = "beancount.core.data", name = "Booking", frozen)]
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
      "NONE" | "None" => Ok(Booking::None),
      "AVERAGE" => Ok(Booking::AVERAGE),
      "FIFO" => Ok(Booking::FIFO),
      "LIFO" => Ok(Booking::LIFO),
      "HIFO" => Ok(Booking::HIFO),
      _ => Err(PyValueError::new_err(format!(
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
      "NONE" | "None" => Ok(Booking::None),
      "AVERAGE" => Ok(Booking::AVERAGE),
      "FIFO" => Ok(Booking::FIFO),
      "LIFO" => Ok(Booking::LIFO),
      "HIFO" => Ok(Booking::HIFO),
      _ => Err(PyValueError::new_err(format!(
        "Invalid booking type: {}",
        value
      ))),
    }
  }
}

#[pymethods]
impl Booking {
  #[new]
  fn new(value: &Bound<'_, PyAny>) -> PyResult<Self> {
    booking_from_any(value.py(), value)?
      .ok_or_else(|| PyValueError::new_err("Invalid booking type: None"))
  }

  // make it behave like enum.Enum
  #[getter]
  fn name(&self) -> &str {
    self.__str__()
  }

  #[getter]
  fn value(&self) -> &str {
    self.__str__()
  }

  #[classattr]
  #[allow(non_snake_case)]
  fn NONE(py: Python<'_>) -> PyResult<Py<PyAny>> {
    Py::new(py, Booking::None).map(|b| b.into_bound(py).into_any().unbind())
  }

  // to support both `Booking.STRICT == Booking.STRICT` and `Booking.STRICT == "STRICT"`
  fn __eq__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
    if let Ok(b) = other.extract::<PyRef<'_, Booking>>() {
      return Ok(*self == *b);
    }

    if let Ok(s) = other.extract::<String>() {
      return Ok(self.__str__() == s);
    }

    if let Ok(val) = other.getattr("value")
      && let Ok(s) = val.extract::<String>()
    {
      return Ok(self.__str__() == s);
    }

    Ok(false)
  }

  fn __hash__(&self) -> PyResult<isize> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    self.__str__().hash(&mut hasher);
    Ok(hasher.finish() as isize)
  }

  fn __reduce_ex__(&self, _version: isize, py: Python<'_>) -> PyResult<Py<PyAny>> {
    // Pickle using the constructor and the string value, mirroring Python Enum behavior.
    let args = PyTuple::new(py, [self.__str__()])?;
    let booking_type = py.get_type::<Booking>().unbind().into_any();
    let args_obj = args.unbind().into_any();
    let tuple = PyTuple::new(py, [booking_type, args_obj])?;
    Ok(tuple.unbind().into_any())
  }

  fn __copy__(&self) -> PyResult<Self> {
    Ok(self.clone())
  }

  fn __deepcopy__(&self, _memo: &Bound<'_, PyAny>) -> PyResult<Self> {
    Ok(self.clone())
  }

  fn __str__(&self) -> &str {
    match self {
      Booking::STRICT => "STRICT",
      Booking::STRICT_WITH_SIZE => "STRICT_WITH_SIZE",
      Booking::None => "NONE",
      Booking::AVERAGE => "AVERAGE",
      Booking::FIFO => "FIFO",
      Booking::LIFO => "LIFO",
      Booking::HIFO => "HIFO",
    }
  }
}

fn booking_from_any(_py: Python<'_>, obj: &Bound<'_, PyAny>) -> PyResult<Option<Booking>> {
  if obj.is_none() {
    return Ok(None);
  }

  if let Ok(b) = obj.extract::<PyRef<'_, Booking>>() {
    return Ok(Some(b.clone()));
  }

  if let Ok(val) = obj.getattr("value") {
    let s = val.extract::<String>()?;
    return Booking::try_from(s).map(Some);
  }

  let s = obj.extract::<String>()?;
  Booking::try_from(s).map(Some)
}

struct BookingCache {
  strict: Py<PyAny>,
  strict_with_size: Py<PyAny>,
  none: Py<PyAny>,
  average: Py<PyAny>,
  fifo: Py<PyAny>,
  lifo: Py<PyAny>,
  hifo: Py<PyAny>,
}

static BOOKING_CACHE: PyOnceLock<BookingCache> = PyOnceLock::new();

fn booking_cache(py: Python<'_>) -> PyResult<&'static BookingCache> {
  BOOKING_CACHE.get_or_try_init(py, || {
    let booking_cls = py.import("beancount.core.data")?.getattr("Booking")?;
    Ok(BookingCache {
      strict: booking_cls.getattr("STRICT")?.unbind(),
      strict_with_size: booking_cls.getattr("STRICT_WITH_SIZE")?.unbind(),
      none: booking_cls.getattr("NONE")?.unbind(),
      average: booking_cls.getattr("AVERAGE")?.unbind(),
      fifo: booking_cls.getattr("FIFO")?.unbind(),
      lifo: booking_cls.getattr("LIFO")?.unbind(),
      hifo: booking_cls.getattr("HIFO")?.unbind(),
    })
  })
}

fn booking_py_from_native(py: Python<'_>, booking: Option<&Booking>) -> PyResult<Py<PyAny>> {
  let cache = booking_cache(py)?;
  Ok(match booking {
    Some(Booking::STRICT) => cache.strict.clone_ref(py),
    Some(Booking::STRICT_WITH_SIZE) => cache.strict_with_size.clone_ref(py),
    Some(Booking::None) => cache.none.clone_ref(py),
    Some(Booking::AVERAGE) => cache.average.clone_ref(py),
    Some(Booking::FIFO) => cache.fifo.clone_ref(py),
    Some(Booking::LIFO) => cache.lifo.clone_ref(py),
    Some(Booking::HIFO) => cache.hifo.clone_ref(py),
    None => py.None(),
  })
}

/// Tuple-backed implementation of beancount.core.data.Open.
/// Exposes tuple-like behavior and keeps native Rust copies of fields.
#[pyclass(module = "beancount.core.data", name = "Open", sequence)]
pub struct PyOpen {
  pub(crate) tuple_view: Py<PyTuple>,
  pub(crate) meta_py: Py<PyAny>,
  pub(crate) date_native: NaiveDate,
  pub(crate) account_native: String,
  pub(crate) currencies_native: Vec<String>,
  pub(crate) booking_native: Option<Booking>,
}

#[pymethods]
impl PyOpen {
  #[new]
  #[pyo3(signature = (meta, date, account, currencies, booking))]
  fn new(
    py: Python<'_>,
    meta: Py<PyAny>,
    date: Py<PyAny>,
    account: Py<PyAny>,
    currencies: Py<PyAny>,
    booking: Py<PyAny>,
  ) -> PyResult<Self> {
    let date_native = date.bind(py).extract::<NaiveDate>()?;
    let account_native = account.bind(py).extract::<String>()?;
    let currencies_native = currencies
      .bind(py)
      .extract::<Vec<String>>()
      .or_else(|_| currencies.bind(py).extract::<String>().map(|c| vec![c]))
      .unwrap_or_default();
    let booking_native = booking_from_any(py, booking.bind(py))?;

    let tuple_view = PyTuple::new(
      py,
      [
        meta.clone_ref(py),
        date.clone_ref(py),
        account.clone_ref(py),
        currencies.clone_ref(py),
        booking.clone_ref(py),
      ],
    )?
    .unbind();

    Ok(Self {
      tuple_view,
      meta_py: meta,
      date_native,
      account_native,
      currencies_native,
      booking_native,
    })
  }
  #[getter]
  fn meta(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
    Ok(self.meta_py.clone_ref(py))
  }

  #[getter]
  fn date(&self) -> PyResult<NaiveDate> {
    Ok(self.date_native)
  }

  #[getter]
  fn account(&self) -> PyResult<String> {
    Ok(self.account_native.clone())
  }

  #[getter]
  fn currencies(&self) -> PyResult<Vec<String>> {
    Ok(self.currencies_native.clone())
  }

  #[getter]
  fn booking(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
    booking_py_from_native(py, self.booking_native.as_ref())
  }

  fn __len__(&self) -> PyResult<usize> {
    Ok(OPEN_FIELD_NAMES.len())
  }

  fn __getitem__(&self, py: Python<'_>, idx: isize) -> PyResult<Py<PyAny>> {
    let tuple = self.tuple_view.bind(py);
    let len = tuple.len() as isize;
    let norm = if idx < 0 { len + idx } else { idx };
    if norm < 0 || norm >= len {
      return Err(PyIndexError::new_err("tuple index out of range"));
    }
    Ok(tuple.get_item(norm as usize)?.unbind())
  }

  #[classattr]
  fn _fields(py: Python<'_>) -> PyResult<Py<PyAny>> {
    Ok(PyTuple::new(py, OPEN_FIELD_NAMES)?.unbind().into())
  }

  #[classattr]
  fn __match_args__(py: Python<'_>) -> PyResult<Py<PyAny>> {
    Ok(PyTuple::new(py, OPEN_FIELD_NAMES)?.unbind().into())
  }

  #[classattr]
  fn __slots__(py: Python<'_>) -> PyResult<Py<PyAny>> {
    let empty: [i32; 0] = [];
    Ok(PyTuple::new(py, empty)?.unbind().into())
  }

  #[classattr]
  fn _field_defaults(py: Python<'_>) -> PyResult<Py<PyAny>> {
    Ok(PyDict::new(py).unbind().into())
  }

  fn _asdict<'py>(slf: PyRef<'py, Self>, py: Python<'py>) -> PyResult<Py<PyDict>> {
    let dict = PyDict::new(py);
    let tuple = slf.tuple_view.bind(py);
    for (idx, name) in OPEN_FIELD_NAMES.iter().enumerate() {
      dict.set_item(*name, tuple.get_item(idx)?)?;
    }
    Ok(dict.unbind())
  }

  #[pyo3(signature = (**kwargs))]
  fn _replace<'py>(
    slf: PyRef<'py, Self>,
    py: Python<'py>,
    kwargs: Option<&Bound<'py, PyDict>>,
  ) -> PyResult<Py<PyAny>> {
    let mut values: Vec<Py<PyAny>> = slf
      .tuple_view
      .bind(py)
      .iter()
      .map(|item| item.unbind())
      .collect();

    if let Some(kwargs) = kwargs {
      let mut unknown = Vec::new();
      for (key, value) in kwargs.iter() {
        let name: String = key.extract()?;
        if let Some(idx) = open_field_index(&name) {
          values[idx] = value.unbind();
        } else {
          unknown.push(name);
        }
      }

      if !unknown.is_empty() {
        return Err(PyValueError::new_err(format!(
          "Got unexpected field names: {:?}",
          unknown
        )));
      }
    }

    let created = PyOpen::new(
      py,
      values[0].clone_ref(py),
      values[1].clone_ref(py),
      values[2].clone_ref(py),
      values[3].clone_ref(py),
      values[4].clone_ref(py),
    )?;
    Py::new(py, created).map(|p| p.into())
  }

  fn __getnewargs__<'py>(slf: PyRef<'py, Self>, py: Python<'py>) -> PyResult<Py<PyTuple>> {
    let args: Vec<Py<PyAny>> = slf
      .tuple_view
      .bind(py)
      .iter()
      .map(|item| item.unbind())
      .collect();
    Ok(PyTuple::new(py, args)?.unbind())
  }

  fn __repr__<'py>(slf: PyRef<'py, Self>, py: Python<'py>) -> PyResult<String> {
    let tuple = slf.tuple_view.bind(py);
    let mut parts = Vec::with_capacity(OPEN_FIELD_NAMES.len());
    for (idx, name) in OPEN_FIELD_NAMES.iter().enumerate() {
      let value_repr = tuple.get_item(idx)?.repr()?.extract::<String>()?;
      parts.push(format!("{}={}", name, value_repr));
    }
    Ok(format!("Open({})", parts.join(", ")))
  }
}

impl PyOpen {
  pub fn from_core_parts(
    py: Python<'_>,
    meta: Py<PyAny>,
    date_native: NaiveDate,
    account: &str,
    currencies: &[String],
    booking_native: Option<Booking>,
  ) -> PyResult<Py<PyAny>> {
    let date_py: Py<PyAny> = PyDate::new(
      py,
      date_native.year(),
      date_native.month() as u8,
      date_native.day() as u8,
    )?
    .unbind()
    .into_any();
    let account_py: Py<PyAny> = PyString::new(py, account).unbind().into();
    let currencies_py: Py<PyAny> = PyList::new(py, currencies.iter().map(|c| c.as_str()))?
      .unbind()
      .into();

    let currencies_native = currencies.to_vec();
    let booking_py = booking_py_from_native(py, booking_native.as_ref())?;
    let tuple_view = PyTuple::new(
      py,
      [
        meta.clone_ref(py),
        date_py.clone_ref(py),
        account_py.clone_ref(py),
        currencies_py.clone_ref(py),
        booking_py.clone_ref(py),
      ],
    )?
    .unbind();

    let inst = PyOpen {
      tuple_view,
      meta_py: meta,
      date_native,
      account_native: account.to_string(),
      currencies_native,
      booking_native,
    };

    Py::new(py, inst).map(|p| p.into())
  }
}
