mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};

#[test]
fn pushtag_directive() {
  let input = lines(&[r#"pushtag #project"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Pushtag(tag)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(tag.tag, "#project");
}

#[test]
fn poptag_directive() {
  let input = lines(&[r#"poptag #project"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Poptag(tag)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(tag.tag, "#project");
}
