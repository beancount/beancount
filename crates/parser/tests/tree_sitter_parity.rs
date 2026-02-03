#![cfg(feature = "tree-sitter")]

mod chumsky_common;

use beancount_parser::ast::Directive;
use beancount_parser::parse_str_chumsky;
use beancount_parser::tree_sitter_parser::parse_directives as parse_tree_sitter;
use beancount_tree_sitter::language;
use beancount_tree_sitter::tree_sitter::Parser;
use chumsky_common::lines;

fn parse_with_tree_sitter(input: &str, filename: &str) -> Vec<Directive> {
  let mut parser = Parser::new();
  parser
    .set_language(&language())
    .expect("failed to load beancount language");
  let tree = parser.parse(input, None).expect("tree-sitter parse failed");
  parse_tree_sitter(tree.root_node(), input, filename.to_string())
    .expect("tree-sitter AST conversion failed")
}

fn assert_parsers_match(input: &str) {
  let filename = "book.bean";
  let chumsky = parse_str_chumsky(input, filename).expect("chumsky parse failed");
  let tree_sitter = parse_with_tree_sitter(input, filename);
  assert_eq!(chumsky, tree_sitter);
}

#[test]
fn directives_and_options_match() {
  let input = lines(&[
    r#"option "operating_currency" "USD""#,
    r#"2000-01-01 open Assets:Cash USD"#,
    r#"2000-01-02 close Assets:Cash"#,
    r#"1970-01-01 * "Opening" "Balance""#,
    r#"  Assets:Cash  1.00 USD"#,
    r#"  Equity:Opening-Balances"#,
  ]);

  assert_parsers_match(&input);
}

#[test]
fn transactions_with_metadata_match() {
  let input = lines(&[
    r#"2013-05-18 * "Store" "Groceries" #food ^receipt"#,
    r#"  note: "stash receipt in folder""#,
    r#"  key: unquoted"#,
    r#"  Assets:Checking  -20.00 USD @ 1.10 EUR"#,
    r#"  Expenses:Food   20.00 USD"#,
    r#"; trailing comment stays raw"#,
  ]);

  assert_parsers_match(&input);
}
