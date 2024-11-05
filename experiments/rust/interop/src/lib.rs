use pyo3::prelude::*;
use serde::{Deserialize, Serialize};

#[pyclass]
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Address {
    #[pyo3(get, set)]
    street: String,
    #[pyo3(get, set)]
    city: String,
    #[pyo3(get, set)]
    country: String,
}

#[pymethods]
impl Address {
    #[new]
    fn new(street: String, city: String, country: String) -> Self {
        Address { street, city, country }
    }
}

#[pyclass]
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ContactInfo {
    #[pyo3(get, set)]
    email: String,
    #[pyo3(get, set)]
    phone: String,
}

#[pymethods]
impl ContactInfo {
    #[new]
    fn new(email: String, phone: String) -> Self {
        ContactInfo { email, phone }
    }
}

#[pyclass]
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Person {
    #[pyo3(get, set)]
    name: String,
    #[pyo3(get, set)]
    age: u32,
    #[pyo3(get, set)]
    address: Address,
    #[pyo3(get, set)]
    contact: ContactInfo,
    #[pyo3(get, set)]
    friends: Vec<Person>,
}

#[pymethods]
impl Person {
    #[new]
    fn new(name: String, age: u32, address: Address, contact: ContactInfo) -> Self {
        Person {
            name,
            age,
            address,
            contact,
            friends: Vec::new(),
        }
    }

    fn add_friend(&mut self, friend: Person) {
        self.friends.push(friend);
    }

    fn get_friend_count(&self) -> usize {
        self.friends.len()
    }
}

#[pymodule]
fn interop(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Address>()?;
    m.add_class::<ContactInfo>()?;
    m.add_class::<Person>()?;
    Ok(())
}
