use beancount_formatter::configuration::Configuration;
use beancount_formatter::format;

#[test]
fn formats_without_changes_returns_none() {
  let config = Configuration::default();

  let result = format(
    Some("example.beancount"),
    "2010-01-01 open Assets:Cash\n",
    &config,
  )
  .unwrap();

  assert_eq!(result, "2010-01-01 open Assets:Cash\n");
}
