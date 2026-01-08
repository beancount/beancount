mod common;
use beancount_parser::core::CoreDirective;
use common::{lines, parse_core};

#[test]
fn event_directive() {
  let input = lines(&[r#"2010-07-01 event "office" "moved desks""#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Event(event)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(event.event_type, "office");
  assert_eq!(event.desc, "moved desks");
}

#[test]
fn query_directive() {
  let input = lines(&[r#"2010-08-01 query "balances" "SELECT * FROM balances""#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Query(query)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(query.name, "balances");
  assert_eq!(query.query, "SELECT * FROM balances");
}
