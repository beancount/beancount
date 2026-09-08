#[path = "core_common.rs"]
mod common;
use beancount_core::{Amount, Commodity, NumberExpr, Price};
use common::{lines, parse_as};
use jiff::civil::date;
use smallvec::smallvec;

#[test]
fn commodity_directive() {
  let input = lines(&[r#"2010-05-01 commodity USD"#]);

  let cmdty: Commodity = parse_as(&input, "book.bean");

  let expected = Commodity {
    meta: cmdty.meta.clone(),
    span: cmdty.span,
    date: date(2010, 5, 1),
    currency: "USD".into(),
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(cmdty, expected);
}

#[test]
fn price_directive() {
  let input = lines(&[r#"2010-06-01 price USD 1.25 CAD"#]);

  let price: Price = parse_as(&input, "book.bean");

  let expected = Price {
    meta: price.meta.clone(),
    span: price.span,
    date: date(2010, 6, 1),
    currency: "USD".into(),
    amount: Amount {
      raw: "1.25 CAD".into(),
      number: NumberExpr::Literal("1.25".into()),
      currency: Some("CAD".into()),
    },
    comment: None,
    key_values: smallvec![],
  };

  assert_eq!(price, expected);
}
