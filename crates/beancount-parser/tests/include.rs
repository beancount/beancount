mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};
use std::path::PathBuf;

#[test]
fn include_directive_resolves_path() {
  let filename = "books/main.bean";
  let expected = PathBuf::from("books").join("includes/extra.bean");
  let input = lines(&[r#"include "includes/extra.bean""#]);

  let directives = parse_core(&input, filename);
  let slice = directives.as_slice();
  let [CoreDirective::Include(include)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(include.filename, expected.to_string_lossy());
}
