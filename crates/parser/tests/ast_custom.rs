use beancount_parser::{ast::Directive, parse_lossy};

#[test]
fn parses_custom_with_inline_comment_after_values() {
  let input = [
    r#"2020-02-01 custom "fava-option" "collapse-pattern" "[^:]*:.*" ; 默认收起两层账户"#,
    "",
  ]
  .join("\n");

  let directives = parse_lossy(input.as_str());
  assert_eq!(directives.len(), 1);

  let custom = match &directives[0] {
    Directive::Custom(custom) => custom,
    other => panic!("expected custom directive, got {other:?}"),
  };

  assert_eq!(custom.name.content, "\"fava-option\"");
  assert_eq!(
    custom
      .values
      .iter()
      .map(|value| value.raw.content)
      .collect::<Vec<_>>(),
    vec!["\"collapse-pattern\"", "\"[^:]*:.*\""],
  );
  assert_eq!(
    custom.comment.as_ref().map(|comment| comment.content),
    Some(" 默认收起两层账户"),
  );
}
