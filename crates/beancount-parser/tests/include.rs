mod common;
use beancount_parser::core::Include;
use common::{lines, parse_as};
use std::path::PathBuf;

#[test]
fn include_directive_resolves_path() {
  let filename = "books/main.bean";
  let input = lines(&[r#"include "includes/extra.bean""#]);

  let include: Include = parse_as(&input, filename);

  let expected = Include {
    meta: include.meta.clone(),
    span: include.span,
    filename: if cfg!(windows) {
      "books\\includes\\extra.bean".to_string()
    } else {
      "books/includes/extra.bean".to_string()
    },
  };

  assert_eq!(include, expected);
}
