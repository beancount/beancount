mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core_allow_all_raw};

#[test]
fn raw_directive_for_comment() {
  let input = lines(&[r#"; trailing raw text"#]);

  let directives = parse_core_allow_all_raw(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Raw(raw)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(raw.kind, "comment");
  assert!(raw.text.contains("trailing raw text"));
}
