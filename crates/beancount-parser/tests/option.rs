mod common;
use beancount_parser::core::OptionDirective;
use common::{lines, parse_as};

#[test]
fn option_directive() {
  let input = lines(&[r#"option "title" "Example Book""#]);

  let opt: OptionDirective = parse_as(&input, "books/main.bean");

  let expected = OptionDirective {
    meta: opt.meta.clone(),
    span: opt.span,
    key: "title".into(),
    value: "Example Book".into(),
  };

  assert_eq!(opt, expected);
}
