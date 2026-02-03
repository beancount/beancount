#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
#![deny(clippy::unwrap_used, clippy::expect_used)]

pub mod ast;
pub mod core;
pub mod path_utils;

mod parser;
mod utils;

pub use core::{CoreDirective, normalize_directives, normalize_directives_with_rope};
pub use parser::parse_str;

use chumsky::prelude::*;
use ropey::Rope;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub(crate) struct ParseCtx {
  pub(crate) filename: Arc<String>,
  pub(crate) rope: Rope,
}

#[derive(Debug, Clone)]
pub(crate) struct MetaAt {
  filename: Arc<String>,
  rope: Rope,
}

impl MetaAt {
  pub(crate) fn at(&self, offset: usize) -> ast::Meta {
    meta_from_rope(&self.filename, &self.rope, offset)
  }
}

impl From<&ParseCtx> for MetaAt {
  fn from(ctx: &ParseCtx) -> Self {
    MetaAt {
      filename: ctx.filename.clone(),
      rope: ctx.rope.clone(),
    }
  }
}

fn meta_from_rope(filename: &Arc<String>, rope: &Rope, offset: usize) -> ast::Meta {
  let char_idx = rope.byte_to_char(offset);
  let line_idx = rope.char_to_line(char_idx);
  let line_start_char = rope.line_to_char(line_idx);
  let line_start_byte = rope.char_to_byte(line_start_char);

  ast::Meta {
    filename: filename.clone(),
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
