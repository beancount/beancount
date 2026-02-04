#[path = "core_chumsky_common.rs"]
mod chumsky_common;
use beancount_parser::core::{Amount, Balance, NumberExpr, Pad};
use chumsky_common::{lines, parse_as};
use smallvec::smallvec;

#[test]
fn balance_directive_no_tolerance() {
  let input = lines(&[r#"2010-02-01 balance Assets:Cash 100 USD"#]);

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
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(balance, expected);
}

#[test]
fn pad_directive() {
  let input = lines(&[r#"2010-03-01 pad Assets:Cash Equity:Pad"#]);

  let pad: Pad = parse_as(&input, "book.bean");

  let expected = Pad {
    meta: pad.meta.clone(),
    span: pad.span,
    date: "2010-03-01".into(),
    account: "Assets:Cash".into(),
    from_account: "Equity:Pad".into(),
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(pad, expected);
}

#[test]
fn balance_directive_tolerance_after_currency_with_comment() {
  let input = lines(&[r#"2026-02-03 balance Assets:Cash 0 USD ~1 USD ; tolerance"#]);

  let balance: Balance = parse_as(&input, "book.bean");

  let expected = Balance {
    meta: balance.meta.clone(),
    span: balance.span,
    date: "2026-02-03".into(),
    account: "Assets:Cash".into(),
    amount: Amount {
      raw: "0 USD ~1 USD".into(),
      number: NumberExpr::Literal("0".into()),
      currency: Some("USD".into()),
    },
    tolerance: Some("1".into()),
    comment: Some(" tolerance".into()),
    key_values: smallvec![],
  };

  assert_eq!(balance, expected);
}
