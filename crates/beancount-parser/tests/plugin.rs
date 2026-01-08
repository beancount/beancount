mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};

#[test]
fn plugin_directive() {
  let input = lines(&[r#"plugin "beancount.plugins.module" "{'k':1}""#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Plugin(plugin)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(plugin.name, "beancount.plugins.module");
  assert_eq!(plugin.config.as_deref(), Some("{'k':1}"));
}
