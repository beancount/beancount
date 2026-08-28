use beancount_parser::{ast::Directive, parse_lossy};

mod span_helpers;
use span_helpers::span_for;

#[test]
fn parses_transaction_with_inline_link_and_postings() {
  let input = [
    r#"2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39"#,
    r#"  Expenses:Coffee         5 USD"#,
    r#"  Assets:US:Cash"#,
    r#""#,
  ]
  .join("\n");

  let directives = parse_lossy(input.as_str());
  assert_eq!(directives.len(), 1);

  let txn = match &directives[0] {
    Directive::Transaction(txn) => txn,
    other => panic!("expected transaction, got {other:?}"),
  };

  let txn_span = span_for(
    &input,
    "2013-06-22 * \"La Colombe\" \"Buying coffee\"  ^ee89ada94a39\n  Expenses:Coffee         5 USD\n  Assets:US:Cash\n",
  );
  assert_eq!(txn.span, txn_span);

  assert_eq!(txn.date.content, "2013-06-22");
  assert_eq!(txn.txn.as_ref().map(|w| w.content), Some("*"));
  assert_eq!(
    txn.payee.as_ref().map(|w| w.content),
    Some("\"La Colombe\"")
  );
  assert_eq!(
    txn.narration.as_ref().map(|w| w.content),
    Some("\"Buying coffee\"")
  );
  assert_eq!(
    txn
      .tags_links
      .as_ref()
      .map(|vals| vals.iter().map(|v| v.content).collect::<Vec<_>>()),
    Some(vec!["^ee89ada94a39"])
  );
  assert!(txn.tags.is_empty());
  assert_eq!(
    txn.links.iter().map(|w| w.content).collect::<Vec<_>>(),
    vec!["ee89ada94a39"]
  );
  assert_eq!(txn.comment, None);
  assert!(txn.key_values.is_empty());

  assert_eq!(txn.postings.len(), 2);

  let p1_span = span_for(&input, "  Expenses:Coffee         5 USD\n");
  let p1 = &txn.postings[0];
  assert_eq!(p1.span, p1_span);
  let account_start = input.find("Expenses:Coffee").unwrap();
  assert_eq!(p1.account.span.start, account_start);
  assert_eq!(p1.opt_flag, None);
  assert_eq!(p1.account.content, "Expenses:Coffee");
  let p1_amount = p1.amount.as_ref().expect("p1 amount");
  assert_eq!(p1_amount.raw.content, "5 USD");
  assert_eq!(
    p1_amount.number,
    beancount_parser::ast::NumberExpr::Literal(beancount_parser::ast::WithSpan::new(
      p1_amount.number.span(),
      "5",
    ))
  );
  assert_eq!(p1_amount.currency.as_ref().map(|w| w.content), Some("USD"));
  assert_eq!(p1.cost_spec, None);
  assert_eq!(p1.price_operator, None);
  assert_eq!(p1.price_annotation, None);
  assert_eq!(p1.comment, None);

  let p2_span = span_for(&input, "  Assets:US:Cash\n");
  let p2 = &txn.postings[1];
  assert_eq!(p2.span, p2_span);
  assert_eq!(p2.opt_flag, None);
  assert_eq!(p2.account.content, "Assets:US:Cash");
  assert!(p2.amount.is_none());
  assert_eq!(p2.cost_spec, None);
  assert_eq!(p2.price_operator, None);
  assert_eq!(p2.price_annotation, None);
  assert_eq!(p2.comment, None);
}

#[test]
fn parses_and_sorts_tags_and_links() {
  let input = [
    "2013-06-22 * \"Payee\" \"Narr\"  #b ^z #a ^a #b",
    "  Assets:Cash 1 USD",
    "",
  ]
  .join("\n");

  let directives = parse_lossy(&input);
  assert_eq!(directives.len(), 1);

  let txn = match &directives[0] {
    Directive::Transaction(txn) => txn,
    other => panic!("expected transaction, got {other:?}"),
  };

  assert_eq!(txn.payee.as_ref().map(|w| w.content), Some("\"Payee\""));
  assert_eq!(txn.narration.as_ref().map(|w| w.content), Some("\"Narr\""));
  assert_eq!(
    txn.tags.iter().map(|w| w.content).collect::<Vec<_>>(),
    vec!["b", "a", "b"]
  );
  assert_eq!(
    txn.links.iter().map(|w| w.content).collect::<Vec<_>>(),
    vec!["z", "a"]
  );
  assert_eq!(
    txn
      .tags_links
      .as_ref()
      .map(|vals| vals.iter().map(|v| v.content).collect::<Vec<_>>()),
    Some(vec!["#b", "^z", "#a", "^a", "#b"])
  );
}
