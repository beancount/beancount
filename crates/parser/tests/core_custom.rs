#[path = "core_common.rs"]
mod common;
use beancount_parser::core::{Custom, CustomValue, NumberExpr};
use common::{collect_ops, lines, parse_as};

#[test]
fn custom_directive_all_value_kinds() {
  let input = lines(&[
    r#"2010-11-01 custom "all_kinds" "string" 2010-12-01 FALSE 5.00 USD 1+2-3*4/5 Assets:Custom"#,
  ]);

  let custom: Custom = parse_as(&input, "book.bean");

  assert_eq!(custom.name, "all_kinds");
  assert_eq!(custom.values.len(), 6);
  assert!(matches!(custom.values[0], CustomValue::String(_)));
  assert!(matches!(custom.values[1], CustomValue::Date(_)));
  assert!(matches!(custom.values[2], CustomValue::Bool(_)));
  match &custom.values[3] {
    CustomValue::Amount(amount) => {
      assert!(matches!(amount.number, NumberExpr::Literal(ref n) if n == "5.00"));
      assert_eq!(amount.currency.as_deref(), Some("USD"));
    }
    other => panic!("expected amount, got {other:?}"),
  }
  match &custom.values[4] {
    CustomValue::Number(num) => {
      let ops = collect_ops(num);
      assert!(ops.iter().all(|v| *v));
    }
    other => panic!("expected number expression, got {other:?}"),
  }
  match &custom.values[5] {
    CustomValue::Account(acct) => assert_eq!(acct, "Assets:Custom"),
    other => panic!("expected account, got {other:?}"),
  }
}
