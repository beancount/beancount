#[path = "core_chumsky_common.rs"]
mod chumsky_common;

use beancount_parser::core::{Amount, Balance, NumberExpr, Transaction};
use chumsky_common::{lines, parse_as};
use smallvec::smallvec;

#[test]
fn inline_comment_content_preserved_on_balance() {
  let input = lines(&[r#"2010-02-01 balance Assets:Cash 100 USD   ; inline comment here"#]);

  let balance: Balance = parse_as(&input, "book.bean");

  let expected = Balance {
    meta: balance.meta.clone(),
    span: balance.span,
    date: "2010-02-01".into(),
    account: "Assets:Cash".into(),
    amount: Amount {
      raw: "100 USD".into(),
      number: NumberExpr::Literal("100".into()),
      currency: Some("USD".into()),
    },
    tolerance: None,
    comment: Some(" inline comment here".into()),
    key_values: smallvec![],
  };

  assert_eq!(balance, expected);
}

#[test]
fn inline_comment_content_preserved_on_posting() {
  let input = lines(&[
    "2010-02-01 * \"Payee\" \"Narration\"",
    "  Assets:Cash 1 USD ; first posting comment",
    "  Equity:Opening-Balances",
  ]);

  let txn: Transaction = parse_as(&input, "book.bean");

  assert_eq!(txn.postings.len(), 2);
  assert_eq!(
    txn.postings[0].comment,
    Some(" first posting comment".into())
  );
  assert_eq!(txn.postings[1].comment, None);
}
