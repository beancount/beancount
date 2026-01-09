mod common;
use beancount_parser::core::Include;
use common::{lines, parse_as};
use std::path::PathBuf;

#[test]
fn include_directive_resolves_path() {
  let filename = "books/main.bean";
  let expected = PathBuf::from("books").join("includes/extra.bean");
  let input = lines(&[r#"include "includes/extra.bean""#]);

  let include: Include = parse_as(&input, filename);

  let expected = Include {
    meta: include.meta.clone(),
    span: include.span,
    filename: expected.to_string_lossy().into_owned(),
  };

  assert_eq!(include, expected);
}
