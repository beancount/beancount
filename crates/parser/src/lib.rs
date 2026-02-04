#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
#![deny(clippy::unwrap_used, clippy::expect_used)]

pub mod ast;
pub mod core;
pub mod path_utils;

mod parser;
mod utils;

pub use core::{CoreDirective, normalize_directives, normalize_directives_with_rope};
pub use parser::{parse_str, parse_str_with_rope};

use chumsky::prelude::*;
use ropey::Rope;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
  pub line: usize,
  pub column: usize,
}

pub(crate) fn position_from_rope(rope: &Rope, offset: usize) -> Position {
  let char_idx = rope.byte_to_char(offset);
  let line_idx = rope.char_to_line(char_idx);
  let line_start_char = rope.line_to_char(line_idx);
  let line_start_byte = rope.char_to_byte(line_start_char);

  Position {
    line: line_idx + 1,
    column: offset.saturating_sub(line_start_byte) + 1,
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
  pub line: usize,
  pub column: usize,
  pub message: String,
}

impl std::fmt::Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}:{}: {}", self.line, self.column, self.message)
  }
}

impl std::error::Error for ParseError {}

pub type Result<T> = std::result::Result<T, ParseError>;

pub type Error<'src> = extra::Err<Simple<'src, char>>;

#[cfg(test)]
mod tests {
  use super::parse_str;

  #[test]
  fn parses_crlf_input() {
    let src = [
      "2014-01-01 open Assets:Cash USD",
      "",
      "2014-01-02 * \"Lunch\"",
      "  Assets:Cash -10 USD",
      "  Expenses:Food 10 USD",
      "",
    ]
    .join("\r\n");

    let directives = parse_str(&src).expect("CRLF input should parse");

    assert_eq!(2, directives.len());
  }
}
