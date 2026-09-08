#[path = "core_common.rs"]
mod common;
use beancount_core::{Event, Query};
use common::{lines, parse_as};
use jiff::civil::date;
use smallvec::smallvec;

#[test]
fn event_directive() {
  let input = lines(&[r#"2010-07-01 event "office" "moved desks""#]);

  let event: Event = parse_as(&input, "book.bean");

  let expected = Event {
    meta: event.meta.clone(),
    span: event.span,
    date: date(2010, 7, 1),
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
    date: date(2010, 8, 1),
    name: "balances".into(),
    query: "SELECT * FROM balances".into(),
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(query, expected);
}
