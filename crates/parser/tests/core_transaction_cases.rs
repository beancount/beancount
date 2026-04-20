#[path = "core_common.rs"]
mod common;
use beancount_parser::ast::{Directive, PriceOperator};
use beancount_parser::core::{KeyValueValue, NumberExpr, Transaction};
use common::{lines, parse_as};

#[test]
fn transaction_directive_with_postings() {
  let input = lines(&[
    r#"2011-01-01 * "Payee" "Narration""#,
    r#"  txn_meta: FALSE"#,
    r#"  Assets:Cash  -10 USD {100 USD, "label", *, 2011-02-02} @ EUR"#,
    r#"    raw: 123"#,
    r#"  Expenses:Food 10 USD {{1 # 2 USD}} @@ 2 EUR"#,
    r#"    status: pending"#,
  ]);

  let txn: Transaction = parse_as(&input, "book.bean");

  assert_eq!(txn.txn.as_deref(), Some("*"));
  assert_eq!(txn.payee.as_deref(), Some("Payee"));
  assert_eq!(txn.narration.as_deref(), Some("Narration"));
  assert_eq!(txn.key_values.len(), 1);
  assert!(matches!(
    txn.key_values[0].value,
    Some(KeyValueValue::Bool(false))
  ));
  assert_eq!(txn.postings.len(), 2);

  let p1 = &txn.postings[0];
  assert_eq!(p1.account, "Assets:Cash");
  assert!(matches!(p1.price_operator, Some(PriceOperator::PerUnit)));
  let cost1 = p1.cost_spec.as_ref().expect("p1 cost");
  assert!(cost1.raw.contains('*'));
  assert_eq!(cost1.date.as_deref(), Some("2011-02-02"));
  assert_eq!(cost1.label, None);
  let cost1_amount = cost1.amount.as_ref().expect("p1 cost amount");
  assert!(matches!(cost1_amount.per, Some(NumberExpr::Literal(ref n)) if n == "100"));
  assert_eq!(cost1_amount.currency.as_deref(), Some("USD"));
  let price1 = p1.price_annotation.as_ref().expect("p1 price");
  assert!(matches!(price1.number, NumberExpr::Missing));
  assert_eq!(price1.currency.as_deref(), Some("EUR"));
  assert!(matches!(
    p1.key_values[0].value,
    Some(KeyValueValue::UnquotedString(ref s)) if s == "123"
  ));

  let p2 = &txn.postings[1];
  assert_eq!(p2.account, "Expenses:Food");
  assert!(matches!(p2.price_operator, Some(PriceOperator::Total)));
  let p2_cost = p2.cost_spec.as_ref().expect("p2 cost");
  assert!(p2_cost.is_total);
  let p2_amount = p2_cost.amount.as_ref().expect("p2 cost amount");
  assert!(matches!(p2_amount.per, Some(NumberExpr::Literal(ref n)) if n == "1"));
  assert!(matches!(p2_amount.total, Some(NumberExpr::Literal(ref n)) if n == "2"));
  assert_eq!(p2_amount.currency.as_deref(), Some("USD"));
  let price2 = p2.price_annotation.as_ref().expect("p2 price");
  assert!(matches!(price2.number, NumberExpr::Literal(ref n) if n == "2"));
  assert_eq!(price2.currency.as_deref(), Some("EUR"));
  assert!(
    matches!(p2.key_values[0].value, Some(KeyValueValue::UnquotedString(ref s)) if s == "pending")
  );
}

#[test]
fn transaction_with_custom_flag() {
  let input = lines(&[
    r#"2015-01-01 P "Padding""#,
    r#"  Assets:Cash           1 USD"#,
    r#"  Equity:Other         -1 USD"#,
  ]);

  let txn: Transaction = parse_as(&input, "book.bean");

  assert_eq!(txn.txn.as_deref(), Some("P"));
  assert_eq!(txn.postings.len(), 2);
  assert_eq!(txn.postings[0].account, "Assets:Cash");
  assert_eq!(txn.postings[1].account, "Equity:Other");
}

#[test]
fn transaction_tags_and_links_content() {
  let input = lines(&[
    r#"2013-06-22 * "Payee" "Narr"  #b ^link1"#,
    r#"  Assets:Cash 1 USD"#,
    r#"  #c ^link2 #a"#,
  ]);

  let directives = beancount_parser::parse_lossy(&input);

  assert_eq!(directives.len(), 2, "{:?}", directives);
  assert!(matches!(
    directives[0],
    beancount_parser::ast::Directive::Transaction(_)
  ));
  match &directives[1] {
    beancount_parser::ast::Directive::Raw(raw) => {
      assert_eq!(raw.text, "  #c ^link2 #a");
    }
    other => panic!("expected trailing raw meta line, got {other:?}"),
  }
}

#[test]
fn transaction_body_comment_cannot_be_indented() {
  let input = lines(&[
    r#"2013-06-22 * "Payee" "Narr""#,
    r#"  ; body comment"#,
    r#"  Assets:Cash 1 USD"#,
  ]);

  let directives = beancount_parser::parse_lossy(&input);
  assert_eq!(directives.len(), 3, "{:?}", directives);
}

#[test]
fn transaction_stops_before_leaking_comment() {
  let input = lines(&[
    r#"2013-06-22 * "Payee" "Narr""#,
    r#"  Assets:Cash 1 USD"#,
    r#"; unrelated top-level comment"#,
    r#"2013-06-23 * "Next" "Narr2""#,
    r#"  Assets:Cash 2 USD"#,
  ]);

  let directives = beancount_parser::parse_lossy(&input);
  assert_eq!(directives.len(), 3);

  assert!(matches!(directives[0], Directive::Transaction(_)));
  assert!(matches!(directives[1], Directive::Comment(_)));
  assert!(matches!(directives[2], Directive::Transaction(_)));
}

#[test]
fn transaction_with_tags_and_inline_comments() {
  let input = lines(&[
    r#"2010-01-12 *   "Payee"  "Narration"   #tag1  ; headercmt"#,
    r#"    Assets:Cash   -10   USD    ;comment1"#,
    r#"        Expenses:Food  10   USD   ;comment2"#,
  ]);

  let txn: Transaction = parse_as(&input, "book.bean");

  assert_eq!(txn.date, "2010-01-12");
  assert_eq!(txn.txn.as_deref(), Some("*"));
  assert_eq!(txn.payee.as_deref(), Some("Payee"));
  assert_eq!(txn.narration.as_deref(), Some("Narration"));
  assert_eq!(txn.tags.len(), 1);
  assert_eq!(txn.tags[0], "tag1");
  assert_eq!(txn.links.len(), 0);

  assert_eq!(txn.postings.len(), 2);

  let posting1 = &txn.postings[0];
  assert_eq!(posting1.account, "Assets:Cash");
  let amount1 = posting1.amount.as_ref().expect("posting1 amount");
  assert!(matches!(amount1.number, NumberExpr::Literal(ref n) if n == "-10"));
  assert_eq!(amount1.currency.as_deref(), Some("USD"));
  assert_eq!(posting1.comment.as_deref(), Some("comment1"));

  let posting2 = &txn.postings[1];
  assert_eq!(posting2.account, "Expenses:Food");
  let amount2 = posting2.amount.as_ref().expect("posting2 amount");
  assert!(matches!(amount2.number, NumberExpr::Literal(ref n) if n == "10"));
  assert_eq!(amount2.currency.as_deref(), Some("USD"));
  assert_eq!(posting2.comment.as_deref(), Some("comment2"));
}
