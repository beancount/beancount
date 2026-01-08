mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};

#[test]
fn commodity_directive() {
  let input = lines(&[r#"2010-05-01 commodity USD"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Commodity(cmdty)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(cmdty.currency, "USD");
}

#[test]
fn price_directive() {
  let input = lines(&[r#"2010-06-01 price USD 1.25 CAD"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Price(price)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(price.currency, "USD");
  assert_eq!(price.amount.currency.as_deref(), Some("CAD"));
}
