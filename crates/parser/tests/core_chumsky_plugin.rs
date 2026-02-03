use beancount_parser::core::Plugin;
use beancount_parser::parse_str as parse_chumsky;

fn parse_plugin(input: &str, filename: &str) -> Plugin {
  let ast = parse_chumsky(input, filename).expect("chumsky parse failed");
  let core = beancount_parser::normalize_directives(&ast, filename, input)
    .expect("chumsky normalize failed");
  assert_eq!(core.len(), 1, "expected a single directive");
  match core.into_iter().next().expect("directive") {
    beancount_parser::core::CoreDirective::Plugin(plugin) => plugin,
    other => panic!("expected plugin directive, got {other:?}"),
  }
}

#[test]
fn plugin_directive() {
  let input = "plugin \"beancount.plugins.module\" \"{'k':1}\"";

  let plugin = parse_plugin(input, "book.bean");

  let expected = Plugin {
    meta: plugin.meta.clone(),
    span: plugin.span,
    name: "beancount.plugins.module".into(),
    config: Some("{'k':1}".into()),
  };

  assert_eq!(plugin, expected);
}
