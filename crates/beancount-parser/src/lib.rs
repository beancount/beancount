pub mod ast;
pub mod parse;

pub use parse::{parse_amount_tokens, parse_directives, ParseError};

/// Parse a beancount source string into typed directives.
pub fn parse_str<'a>(source: &'a str, filename: &str) -> Result<Vec<ast::Directive<'a>>, ParseError> {
  let mut parser = tree_sitter::Parser::new();

  parser
    .set_language(&tree_sitter_beancount::language())
    .map_err(|err| ParseError {
      filename: filename.to_owned(),
      line: 0,
      column: 0,
      message: format!("failed to load beancount grammar: {}", err),
    })?;

  let tree = parser.parse(source, None).ok_or_else(|| ParseError {
    filename: filename.to_owned(),
    line: 0,
    column: 0,
    message: format!("failed to parse {}", filename),
  })?;

  let root = tree.root_node();

  if root.has_error() {
    let pos = root.start_position();
    return Err(ParseError {
      filename: filename.to_owned(),
      line: pos.row + 1,
      column: pos.column + 1,
      message: "syntax error".to_string(),
    });
  }

  parse::parse_directives(root, source, filename.to_owned())
}

/// Parse file into typed directives.
///
/// This is primarily intended for tests and debugging.
pub fn parse_directives_with_meta<'a>(
  root: tree_sitter::Node,
  source: &'a str,
  filename: String,
) -> Result<Vec<ast::Directive<'a>>, parse::ParseError> {
  parse::parse_directives(root, source, filename)
}
