mod common;
use beancount_parser::core::{CoreDirective, KeyValueValue};
use common::{lines, parse_core};

#[test]
fn open_directive_with_metadata() {
  let input = lines(&[
    r#"2010-01-01 open Assets:Cash USD "FIFO""#,
    r#"  active: TRUE"#,
    r#"  started: 2010-01-01"#,
    r#"  note: "opened account""#,
  ]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Open(open)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(open.account, "Assets:Cash");
  assert_eq!(open.currencies.as_slice(), &["USD"]);
  assert_eq!(open.opt_booking.as_deref(), Some("FIFO"));
  assert_eq!(open.key_values.len(), 3);
  assert!(matches!(
    open.key_values[0].value,
    Some(KeyValueValue::Bool(true))
  ));
  assert!(
    matches!(open.key_values[1].value, Some(KeyValueValue::Date(ref d)) if d == "2010-01-01")
  );
  assert!(
    matches!(open.key_values[2].value, Some(KeyValueValue::String(ref s)) if s == "opened account")
  );
}

#[test]
fn close_directive() {
  let input = lines(&[r#"2010-04-01 close Assets:Cash"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Close(close)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(close.account, "Assets:Cash");
}
