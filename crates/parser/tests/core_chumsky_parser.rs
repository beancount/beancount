use beancount_parser::parse_str;

fn assert_parses(source: &str) {
  let directives = parse_str(source).unwrap();
  let normalized =
    beancount_parser::normalize_directives(&directives, "input.beancount", source).unwrap();
  assert!(!normalized.is_empty());
}

#[test]
fn chumsky_parses_simple_directives() {
  assert_parses(
    "2020-01-01 open Assets:Cash\n\
2020-01-02 balance Assets:Cash 100 USD\n\
2020-01-03 close Assets:Cash\n",
  );
}

#[test]
fn chumsky_parses_transaction_postings() {
  let input = "2020-01-01 * \"Lunch\"\n  Assets:Cash  -10 USD\n  Expenses:Food 10 USD\n";
  let raw = parse_str(input).unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Transaction(txn) => {
      assert_eq!(txn.postings.len(), 2);
    }
    other => panic!("expected transaction, got {other:?}"),
  }
}

#[test]
fn chumsky_parses_txn_keyword() {
  let input = "2013-05-02 txn \"Testing!\"\n";
  let raw = parse_str(input).unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Transaction(txn) => {
      assert_eq!(txn.txn.as_deref(), Some("txn"));
      assert_eq!(txn.narration.as_deref(), Some("Testing!"));
    }
    other => panic!("expected transaction, got {other:?}"),
  }
}

#[test]
fn chumsky_parses_posting_metadata() {
  let input =
    "2020-01-01 * \"Lunch\"\n  Assets:Cash  -10 USD\n    category: Food\n  Expenses:Food 10 USD\n";
  let raw = parse_str(input).unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
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
  let raw = parse_str(input).unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
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
  let raw = parse_str(input).unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
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
  let raw = parse_str(input).unwrap();
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
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
