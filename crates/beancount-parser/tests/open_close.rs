mod common;
use beancount_parser::core::{Close, KeyValue, KeyValueValue, Open};
use common::{lines, parse_as};
use smallvec::smallvec;

#[test]
fn open_directive_with_metadata() {
  let input = lines(&[
    r#"2010-01-01 open Assets:Cash USD "FIFO""#,
    r#"  active: TRUE"#,
    r#"  started: 2010-01-01"#,
    r#"  note: "opened account""#,
  ]);

  let open: Open = parse_as(&input, "book.bean");

  let mut key_values = open.key_values.clone();
  key_values[0] = KeyValue {
    value: Some(KeyValueValue::Bool(true)),
    ..key_values[0].clone()
  };
  key_values[1] = KeyValue {
    value: Some(KeyValueValue::Date("2010-01-01".into())),
    ..key_values[1].clone()
  };
  key_values[2] = KeyValue {
    value: Some(KeyValueValue::String("opened account".into())),
    ..key_values[2].clone()
  };

  let expected = Open {
    date: "2010-01-01".into(),
    account: "Assets:Cash".into(),
    currencies: smallvec!["USD".into()],
    meta: open.meta.clone(),
    span: open.span,
    opt_booking: Some("FIFO".into()),
    comment: None,
    key_values,
  };

  assert_eq!(open, expected);
}

#[test]
fn close_directive() {
  let input = lines(&[r#"2010-04-01 close Assets:Cash"#]);

  let close: Close = parse_as(&input, "book.bean");

  let expected = Close {
    date: "2010-04-01".into(),
    account: "Assets:Cash".into(),
    meta: close.meta.clone(),
    span: close.span,
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(close, expected);
}
