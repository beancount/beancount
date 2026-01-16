mod common;
use beancount_parser::core::TagDirective;
use common::{PoptagDir, PushtagDir, lines, parse_as};

#[test]
fn pushtag_directive() {
  let input = lines(&[r#"pushtag #project"#]);

  let PushtagDir(tag) = parse_as(&input, "book.bean");

  let expected = TagDirective {
    meta: tag.meta.clone(),
    span: tag.span,
    tag: "project".into(),
  };

  assert_eq!(tag, expected);
}

#[test]
fn poptag_directive() {
  let input = lines(&[r#"poptag #project"#]);

  let PoptagDir(tag) = parse_as(&input, "book.bean");

  let expected = TagDirective {
    meta: tag.meta.clone(),
    span: tag.span,
    tag: "project".into(),
  };

  assert_eq!(tag, expected);
}
