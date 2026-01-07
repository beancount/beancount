#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]

pub mod ast;
pub mod core;
pub mod parse;

pub use core::{CoreDirective, normalize_directives};
pub use parse::{ParseError, parse_amount_tokens, parse_directives};

/// Parse a beancount source string into typed directives.
pub fn parse_str<'a>(
    source: &'a str,
    filename: &str,
) -> Result<Vec<ast::Directive<'a>>, ParseError> {
    let mut parser = tree_sitter::Parser::new();

    parser
        .set_language(&beancount_tree_sitter::language())
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

    let directives = parse::parse_directives(root, source, filename.to_owned())?;

    let all_raw = directives
        .iter()
        .all(|d| matches!(d, crate::ast::Directive::Raw(_)));

    if all_raw && !directives.is_empty() {
        let meta = match &directives[0] {
            crate::ast::Directive::Raw(raw) => &raw.meta,
            _ => unreachable!(),
        };

        return Err(parse::ParseError {
            filename: meta.filename.clone(),
            line: meta.line,
            column: meta.column,
            message: "syntax error".to_string(),
        });
    }

    Ok(directives)
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
