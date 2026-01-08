mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};

#[test]
fn option_directive() {
  let input = lines(&[r#"option "title" "Example Book""#]);

  let directives = parse_core(&input, "books/main.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Option(opt)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(opt.key, "title");
  assert_eq!(opt.value, "Example Book");
}
