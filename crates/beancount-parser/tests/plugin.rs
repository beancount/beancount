mod common;
use beancount_parser::core::Plugin;
use common::{lines, parse_as};

#[test]
fn plugin_directive() {
  let input = lines(&[r#"plugin "beancount.plugins.module" "{'k':1}""#]);

  let plugin: Plugin = parse_as(&input, "book.bean");

  let expected = Plugin {
    meta: plugin.meta.clone(),
    span: plugin.span,
    name: "beancount.plugins.module".into(),
    config: Some("{'k':1}".into()),
  };

  assert_eq!(plugin, expected);
}
