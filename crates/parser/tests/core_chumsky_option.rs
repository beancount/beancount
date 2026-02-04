use beancount_parser::core::OptionDirective;
use beancount_parser::parse_str as parse_chumsky;

fn parse_option(input: &str, filename: &str) -> OptionDirective {
  let ast = parse_chumsky(input);
  let core = beancount_parser::normalize_directives(&ast, filename, input)
    .expect("chumsky normalize failed");
  assert_eq!(core.len(), 1, "expected a single directive");
  match core.into_iter().next().expect("directive") {
    beancount_parser::core::CoreDirective::Option(opt) => opt,
    other => panic!("expected option directive, got {other:?}"),
  }
}

#[test]
fn option_directive() {
  let input = "option \"title\" \"Example Book\"";

  let opt = parse_option(input, "books/main.bean");

  let expected = OptionDirective {
    meta: opt.meta.clone(),
    span: opt.span,
    key: "title".into(),
    value: "Example Book".into(),
  };

  assert_eq!(opt, expected);
}
