mod common;
use beancount_parser::core::{Event, Query};
use common::{lines, parse_as};
use smallvec::smallvec;

#[test]
fn event_directive() {
  let input = lines(&[r#"2010-07-01 event "office" "moved desks""#]);

  let event: Event = parse_as(&input, "book.bean");

  let expected = Event {
    meta: event.meta.clone(),
    span: event.span,
    date: "2010-07-01".into(),
    event_type: "office".into(),
    desc: "moved desks".into(),
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(event, expected);
}

#[test]
fn query_directive() {
  let input = lines(&[r#"2010-08-01 query "balances" "SELECT * FROM balances""#]);

  let query: Query = parse_as(&input, "book.bean");

  let expected = Query {
    meta: query.meta.clone(),
    span: query.span,
    date: "2010-08-01".into(),
    name: "balances".into(),
    query: "SELECT * FROM balances".into(),
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(query, expected);
}
