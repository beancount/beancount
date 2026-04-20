#[path = "core_common.rs"]
mod common;
use beancount_parser::core::{Amount, Balance, NumberExpr, Pad};
use common::{lines, parse_as};
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
fn balance_directive_with_tolerance() {
  let input = lines(&[r#"2015-05-02 balance Assets:Bank:Checking   23.022 ~ 0.001 USD"#]);

  let balance: Balance = parse_as(&input, "book.bean");

  assert_eq!(balance.account, "Assets:Bank:Checking");
  assert_eq!(balance.amount.currency.as_deref(), Some("USD"));
  assert_eq!(balance.tolerance.as_deref(), Some("0.001"));
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
