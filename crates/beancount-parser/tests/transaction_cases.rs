mod common;
use beancount_parser::ast::PriceOperator;
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
  assert!(matches!(p1.key_values[0].value, Some(KeyValueValue::Raw(ref s)) if s == "123"));

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
