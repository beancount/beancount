mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};

#[test]
fn balance_directive_no_tolerance() {
  let input = lines(&[r#"2010-02-01 balance Assets:Cash 100 USD"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Balance(balance)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(balance.amount.currency.as_deref(), Some("USD"));
  assert!(balance.tolerance.is_none());
}

#[test]
fn pad_directive() {
  let input = lines(&[r#"2010-03-01 pad Assets:Cash Equity:Pad"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Pad(pad)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(pad.account, "Assets:Cash");
  assert_eq!(pad.from_account, "Equity:Pad");
}
