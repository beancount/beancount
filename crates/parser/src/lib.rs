#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
#![deny(clippy::unwrap_used, clippy::expect_used)]

pub mod ast;
pub mod core;
pub mod path_utils;

mod parser;
mod utils;

pub use core::{CoreDirective, normalize_directives, normalize_directives_with_rope};
pub use parser::{
  parse_lossy, parse_lossy_with_rope, parse_str_strict, parse_str_strict_with_rope,
};

#[deprecated(note = "use parse_lossy instead")]
pub fn parse_str(input: &str) -> Vec<ast::Directive<'_>> {
  parse_lossy(input)
}

#[deprecated(note = "use parse_lossy_with_rope instead")]
pub fn parse_str_with_rope(source: &str) -> (Vec<ast::Directive<'_>>, Rope) {
  parse_lossy_with_rope(source)
}

use chumsky::prelude::*;
use ropey::Rope;

#[cfg(feature = "rich-errors")]
use chumsky::error::Rich;

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

#[cfg(not(feature = "rich-errors"))]
pub type Error<'src> = extra::Err<Simple<'src, char>>;

#[cfg(feature = "rich-errors")]
pub type Error<'src> = extra::Err<Rich<'src, char>>;

#[cfg(test)]
mod tests {
  use super::{ast, parse_lossy};

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

    let directives = parse_lossy(&src);

    assert_eq!(2, directives.len(), "{:?}", directives);
  }

  #[test]
  fn recovers_to_raw_after_error_line() {
    let src = [
      "2014-01-01 open Assets:Cash USD",      // 0
      "This is not valid",                    // 1
      "2014-01-02 balance Assets:Bank 1 USD", // 2
      "  b: TRUE",
      "2014-01-01 broken Assets:Cash USD", // 3 raw
      "2014-01-01 open Assets:Cash USD",   // 4
      "2014-01-01 open Assets:Cash USD",   // 5 raw
      "  broken meta",
      "2014-01-01 open Assets:Cash USD", // 6
      "",
    ]
    .join("\n");

    let directives = parse_lossy(&src);
    assert!(matches!(directives[0], ast::Directive::Open(_)));
    assert!(matches!(directives[1], ast::Directive::Raw(_)));
    assert!(matches!(directives[2], ast::Directive::Balance(_)));
    assert!(matches!(directives[3], ast::Directive::Raw(_)));
    assert!(matches!(directives[4], ast::Directive::Open(_)));
    assert!(matches!(directives[5], ast::Directive::Raw(_)));
    assert!(matches!(directives[6], ast::Directive::Open(_)));
    assert_eq!(directives.len(), 7);
  }
}
