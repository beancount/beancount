mod common;
use beancount_parser::core::Raw;
use common::{lines, parse_as_allow_raw};

#[test]
fn raw_directive_for_comment() {
  let input = lines(&[r#"; trailing raw text"#]);

  let raw: Raw = parse_as_allow_raw(&input, "book.bean");

  let expected = Raw {
    kind: "comment".into(),
    text: raw.text.clone(),
    ..raw.clone()
  };

  assert_eq!(raw, expected);
}
