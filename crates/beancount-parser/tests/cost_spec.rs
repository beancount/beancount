use beancount_parser::{ast::Directive, parse_str};

#[test]
fn parses_posting_cost_spec() {
  let input = [
    "2014-01-01 open Assets:Investing",
    "2014-01-01 open Assets:Other",
    "",
    "2014-06-05 *",
    "  Assets:Investing      30 HOOL {40 USD}",
    "  Assets:Other",
    "",
    "2014-06-05 *",
    "  Assets:Investing      -20 HOOL {40 USD}",
    "  Assets:Other",
    "",
  ]
  .join("\n");

  let directives = parse_str(&input, "input.bean").expect("parse failed");
  assert_eq!(directives.len(), 4);

  let Directive::Transaction(txn1) = directives[2].clone() else {
    panic!("expected transaction, got {:?}", directives[2]);
  };

  assert_eq!(txn1.postings.len(), 2);
  let p1 = &txn1.postings[0];
  assert_eq!(p1.account.content, "Assets:Investing");
  let amt1 = p1.amount.as_ref().expect("p1 amount");
  assert_eq!(amt1.raw.content, "30 HOOL");
  assert_eq!(amt1.currency.as_ref().unwrap().content, "HOOL");
  let cs1 = p1.cost_spec.as_ref().expect("p1 cost_spec");
  assert_eq!(cs1.raw.content, "{40 USD}");
  let ca1 = cs1.amount.as_ref().expect("p1 cost amount");
  assert!(ca1.total.is_none());
  assert_eq!(ca1.currency.as_ref().unwrap().content, "USD");

  let Directive::Transaction(txn2) = directives[3].clone() else {
    panic!("expected transaction, got {:?}", directives[3]);
  };

  assert_eq!(txn2.postings.len(), 2);
  let p2 = &txn2.postings[0];
  assert_eq!(p2.account.content, "Assets:Investing");
  let amt2 = p2.amount.as_ref().expect("p2 amount");
  assert_eq!(amt2.raw.content, "-20 HOOL");
  assert_eq!(amt2.currency.as_ref().unwrap().content, "HOOL");
  let cs2 = p2.cost_spec.as_ref().expect("p2 cost_spec");
  assert_eq!(cs2.raw.content, "{40 USD}");
  let ca2 = cs2.amount.as_ref().expect("p2 cost amount");
  assert!(ca2.total.is_none());
  assert_eq!(ca2.currency.as_ref().unwrap().content, "USD");
}
