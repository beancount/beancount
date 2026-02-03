#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
#![deny(clippy::unwrap_used, clippy::expect_used)]

pub mod ast;
pub mod core;
pub mod path_utils;
#[cfg(feature = "tree-sitter")]
pub mod tree_sitter_parser;

mod file_parser;
mod parser;
mod utils;

pub use core::{CoreDirective, normalize_directives};
pub use file_parser::parse_str_chumsky;
#[cfg(feature = "file-parser")]
pub use file_parser::parse_str_chumsky as parse_str;
#[cfg(not(feature = "file-parser"))]
pub use parser::parse_str;

use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
  pub filename: String,
  pub line: usize,
  pub column: usize,
  pub message: String,
}

impl std::fmt::Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}:{}:{}: {}",
      self.filename, self.line, self.column, self.message
    )
  }
}

impl std::error::Error for ParseError {}

pub type Result<T> = std::result::Result<T, ParseError>;

pub type Error<'src> = extra::Err<Simple<'src, char>>;
