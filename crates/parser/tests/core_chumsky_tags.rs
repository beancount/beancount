use beancount_parser::core::TagDirective;
use beancount_parser::parse_str as parse_chumsky;

fn parse_tag(input: &str, filename: &str) -> TagDirective {
  let ast = parse_chumsky(input, filename).expect("chumsky parse failed");
  let core = beancount_parser::normalize_directives(&ast, filename, input)
    .expect("chumsky normalize failed");
  assert_eq!(core.len(), 1, "expected a single directive");
  match core.into_iter().next().expect("directive") {
    beancount_parser::core::CoreDirective::PushTag(tag) => tag,
    beancount_parser::core::CoreDirective::PopTag(tag) => tag,
    other => panic!("expected tag directive, got {other:?}"),
  }
}

#[test]
fn pushtag_directive() {
  let input = "pushtag #project";

  let tag = parse_tag(input, "book.bean");

  let expected = TagDirective {
    meta: tag.meta.clone(),
    span: tag.span,
    tag: "project".into(),
  };

  assert_eq!(tag, expected);
}

#[test]
fn poptag_directive() {
  let input = "poptag #project";

  let tag = parse_tag(input, "book.bean");

  let expected = TagDirective {
    meta: tag.meta.clone(),
    span: tag.span,
    tag: "project".into(),
  };

  assert_eq!(tag, expected);
}
