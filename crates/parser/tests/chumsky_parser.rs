use beancount_chumsky::parse_str as parse_chumsky;
use beancount_parser::parse_str as parse_tree_sitter;

fn assert_same_directives(source: &str) {
  let chumsky = parse_chumsky(source, "input.beancount").unwrap();
  let chumsky = beancount_parser::normalize_directives(&chumsky).unwrap();
  let tree_sitter = parse_tree_sitter(source, "input.beancount").unwrap();
  let tree_sitter = beancount_parser::normalize_directives(&tree_sitter).unwrap();
  assert_eq!(chumsky, tree_sitter);
}

#[test]
fn chumsky_parses_simple_directives() {
  assert_same_directives(
    "2020-01-01 open Assets:Cash\n\
2020-01-02 balance Assets:Cash 100 USD\n\
2020-01-03 close Assets:Cash\n",
  );
}

#[test]
fn chumsky_parses_transaction_postings() {
  let input = "2020-01-01 * \"Lunch\"\n  Assets:Cash  -10 USD\n  Expenses:Food 10 USD\n";
  let raw = parse_chumsky(input, "input.beancount").unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw).expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Transaction(txn) => {
      assert_eq!(txn.postings.len(), 2);
    }
    other => panic!("expected transaction, got {other:?}"),
  }
}

#[test]
fn chumsky_parses_posting_metadata() {
  let input =
    "2020-01-01 * \"Lunch\"\n  Assets:Cash  -10 USD\n    category: Food\n  Expenses:Food 10 USD\n";
  let raw = parse_chumsky(input, "input.beancount").unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw).expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Transaction(txn) => {
      assert_eq!(txn.postings.len(), 2);
      assert_eq!(txn.key_values.len(), 0);
    }
    other => panic!("expected transaction, got {other:?}"),
  }
}

#[test]
fn chumsky_parses_raw_directive() {
  let input = "2020-01-01 unknown Assets:Cash\n";
  let raw = parse_chumsky(input, "input.beancount").unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw).expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Raw(raw) => {
      assert_eq!(raw.text, "2020-01-01 unknown Assets:Cash");
      assert_eq!(raw.meta.line, 1);
    }
    other => panic!("expected raw, got {other:?}"),
  }
}

#[test]
fn chumsky_parses_multiline_with_raw() {
  let input = "2020-01-01 unknown Assets:Cash\n  meta: 1";
  let raw = parse_chumsky(input, "input.beancount").unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw).expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Raw(raw) => {
      assert_eq!(raw.text, "2020-01-01 unknown Assets:Cash\n  meta: 1");
      assert_eq!(raw.meta.line, 1);
    }
    other => panic!("expected raw, got {other:?}"),
  }
}

#[test]
fn chumsky_raw_after_blank_line() {
  let input = "2020-01-01 balance Assets:Cash 1 USD\n  meta: 1\n\n  meta: 2";
  let raw = parse_chumsky(input, "input.beancount").unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw).expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 2);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Balance(balance) => {
      assert_eq!(balance.key_values.len(), 1);
    }
    other => panic!("expected balance, got {other:?}"),
  }
  match &chumsky[1] {
    beancount_parser::core::CoreDirective::Raw(raw) => {
      assert_eq!(raw.text, "  meta: 2");
      assert_eq!(raw.meta.line, 4);
    }
    other => panic!("expected raw, got {other:?}"),
  }
}
