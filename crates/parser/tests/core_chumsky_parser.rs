use beancount_parser::{ast, parse_str};

fn assert_parses(source: &str) {
  let directives = parse_str(source);
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
  let raw = parse_str(input);
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
  let raw = parse_str(input);
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
  let raw = parse_str(input);
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
fn chumsky_parses_transaction_without_body() {
  let input = "2026-02-01 * \"Solo\"\n";
  let raw = parse_str(input);
  let chumsky = beancount_parser::normalize_directives(&raw, "input.beancount", input)
    .expect("chumsky normalize failed");
  assert_eq!(chumsky.len(), 1);
  match &chumsky[0] {
    beancount_parser::core::CoreDirective::Transaction(txn) => {
      assert_eq!(txn.postings.len(), 0);
      assert_eq!(txn.key_values.len(), 0);
    }
    other => panic!("expected transaction, got {other:?}"),
  }
}

#[test]
fn chumsky_parses_raw_directive() {
  let input = "2020-01-01 unknown Assets:Cash\n";
  let raw = parse_str(input);
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
  let raw = parse_str(input);
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
  let raw = parse_str(input);
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

#[test]
fn chumsky_failed_transaction_keeps_following_headline() {
  const INPUT: &str = r#"popmtea foo:
2026-02-12 * "Payee "Narration" #tag ^link
  Assets:Cash -10 USD { 1 # 2 USD } @ 5 EUR ; line comment
  Expenses:Food 10 USD @@ 20 USD
  key: FALSE
2026-02-13 ! "Payee2" "Narr2"
* Headline
"#;

  let directives = parse_str(INPUT);

  dbg!(&directives);

  assert!(matches!(directives[0], ast::Directive::Raw(_)));
  assert!(
    matches!(directives.get(1), Some(ast::Directive::Raw(raw)) if raw.text.contains("2026-02-12 * \"Payee"))
  );
  assert!(
    matches!(directives[2], ast::Directive::Transaction(_)),
    "{:?}",
    directives[2]
  );
  assert!(
    matches!(directives[3], ast::Directive::Headline(_)),
    "{:?}",
    directives[3]
  );
  assert_eq!(directives.len(), 4);
}

#[test]
fn chumsky_coerces_failed_transaction_to_raw() {
  const SAMPLE: &str = r#"; top level comment
2026-02-01 open Assets:Cash USD ; open comment
2026-02-02 close Assets:Cash
2026-02-03 balance Assets:Cash 0 USD ~1 USD ; tolerance
2026-02-04 pad Assets:Cash Equity:Opening-Balances
2026-02-05 note Assets:Cash "note text"
2026-02-06 commodity USD
2026-02-07 price USD 1 EUR
2026-02-08 event "type" "desc"
2026-02-09 query "q" "select 1"
2026-02-10 document Assets:Cash "file.pdf" #doc ^link
2026-02-11 custom "name" "val" TRUE 1*2+3/4 USD
option "title" "value"
include "other.bean"
plugin "mod" "config"
pushtag #tag2
poptag #tag3
pushmeta foo: TRUE
popmeta foo:
2026-02-12 * "Payee" broken narration #tag ^link
  Assets:Cash -10 USD { 1 # 2 USD } @ 5 EUR ; line comment
  Expenses:Food 10 USD @@ 20 USD
  key: FALSE
2026-02-13 ! "Payee2" "Narr2"
* Headline
"#;

  let directives = parse_str(SAMPLE);

  dbg!(&directives);

  let txn_raw = directives.iter().find_map(|directive| match directive {
    ast::Directive::Raw(raw) if raw.text.starts_with("2026-02-12 * \"Payee\"") => Some(raw),
    _ => None,
  });

  assert!(
    txn_raw.is_some(),
    "transaction with body errors should coerce to Raw"
  );
  assert!(directives.iter().all(|directive| {
    !matches!(directive, ast::Directive::Transaction(txn) if txn.date.content == "2026-02-12")
  }));
}
