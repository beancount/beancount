#[path = "core_common.rs"]
mod common;
use beancount_parser::core::Include;
use common::{lines, parse_as};

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

#[cfg(windows)]
#[test]
fn include_directive_handles_windows_escaped_backslashes() {
  let filename = "C:/ledger/main.bean";
  let input = lines(&[r#"include "C:\\Users\\alice\\books\\main.bean""#]);

  let include: Include = parse_as(&input, filename);

  let expected = Include {
    meta: include.meta.clone(),
    span: include.span,
    filename: "C:\\Users\\alice\\books\\main.bean".to_string(),
  };

  assert_eq!(include, expected);
}
