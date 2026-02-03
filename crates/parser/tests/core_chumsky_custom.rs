use beancount_parser::core::{Custom, CustomValue, NumberExpr};
use beancount_parser::parse_str as parse_chumsky;

fn parse_custom(input: &str, filename: &str) -> Custom {
  let ast = parse_chumsky(input, filename).expect("chumsky parse failed");
  let core = beancount_parser::normalize_directives(&ast, filename, input)
    .expect("chumsky normalize failed");
  assert_eq!(core.len(), 1, "expected a single directive");
  match core.into_iter().next().expect("directive") {
    beancount_parser::core::CoreDirective::Custom(custom) => custom,
    other => panic!("expected custom directive, got {other:?}"),
  }
}

fn collect_ops(expr: &NumberExpr) -> [bool; 4] {
  let mut seen = [false; 4];
  fn walk(expr: &NumberExpr, seen: &mut [bool; 4]) {
    if let NumberExpr::Binary { left, op, right } = expr {
      match op {
        beancount_parser::core::BinaryOp::Add => seen[0] = true,
        beancount_parser::core::BinaryOp::Sub => seen[1] = true,
        beancount_parser::core::BinaryOp::Mul => seen[2] = true,
        beancount_parser::core::BinaryOp::Div => seen[3] = true,
      }
      walk(left, seen);
      walk(right, seen);
    }
  }
  walk(expr, &mut seen);
  seen
}

#[test]
fn custom_directive_all_value_kinds() {
  let input =
    "2010-11-01 custom \"all_kinds\" \"string\" 2010-12-01 FALSE 5.00 USD 1+2-3*4/5 Assets:Custom";

  let custom = parse_custom(input, "book.bean");

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
