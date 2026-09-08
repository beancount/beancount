#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
// workaround https://github.com/rust-lang/rust-clippy/issues/13981
#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
#![cfg_attr(test, allow(clippy::unwrap_used, clippy::expect_used))]

pub mod booking;
pub mod core;
pub mod encryption;
pub mod inference;
pub mod loader;
pub mod path_utils;

pub use crate::booking::{BookingConfig, BookingError, BookingErrorKind, book_directives};
pub use crate::core::*;
pub use crate::inference::{
  InferredDirective, InferredPosting, InferredTransaction, infer_directives,
  infer_transaction_postings,
};
pub use beancount_parser::{Error, ParseError, Position, Result, ast};
pub use loader::{LoadAndBookResult, LoadResult, Loader, LoaderError, ParsedUnit};
