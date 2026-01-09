use beancount_parser::{
  ast::{Amount, CostAmount, CostSpec, Directive, NumberExpr, Posting, Transaction},
  parse_str,
};

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

  let mut postings1 = txn1.postings.clone();
  postings1[0] = Posting {
    account: "Assets:Investing",
    amount: Some(Amount {
      raw: "30 HOOL",
      number: NumberExpr::Literal("30"),
      currency: Some("HOOL"),
    }),
    cost_spec: Some(CostSpec {
      raw: "{40 USD}",
      amount: Some(CostAmount {
        per: Some(NumberExpr::Literal("40")),
        total: None,
        currency: Some("USD"),
      }),
      date: None,
      label: None,
      merge: false,
      is_total: false,
    }),
    price_operator: None,
    price_annotation: None,
    ..postings1[0].clone()
  };
  postings1[1] = Posting {
    account: "Assets:Other",
    amount: None,
    cost_spec: None,
    ..postings1[1].clone()
  };

  let expected_txn1 = Directive::Transaction(Transaction {
    postings: postings1,
    ..txn1.clone()
  });

  assert_eq!(directives[2], expected_txn1);

  let Directive::Transaction(txn2) = directives[3].clone() else {
    panic!("expected transaction, got {:?}", directives[3]);
  };

  let mut postings2 = txn2.postings.clone();
  postings2[0] = Posting {
    account: "Assets:Investing",
    amount: Some(Amount {
      raw: "-20 HOOL",
      number: NumberExpr::Literal("-20"),
      currency: Some("HOOL"),
    }),
    cost_spec: Some(CostSpec {
      raw: "{40 USD}",
      amount: Some(CostAmount {
        per: Some(NumberExpr::Literal("40")),
        total: None,
        currency: Some("USD"),
      }),
      date: None,
      label: None,
      merge: false,
      is_total: false,
    }),
    price_operator: None,
    price_annotation: None,
    ..postings2[0].clone()
  };
  postings2[1] = Posting {
    account: "Assets:Other",
    amount: None,
    cost_spec: None,
    ..postings2[1].clone()
  };

  let expected_txn2 = Directive::Transaction(Transaction {
    postings: postings2,
    ..txn2.clone()
  });

  assert_eq!(directives[3], expected_txn2);
}
