use std::collections::HashMap;

use jiff::civil::Date;
use rust_decimal::Decimal;

use crate::core::{
  Amount, CostAmount, CostSpec, Directive, NumberExpr, Transaction, number_expr_to_decimal,
};
use beancount_parser::ast;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferredDirective {
  Transaction(InferredTransaction),
  Other(Directive),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferenceErrorKind {
  NumberEval { message: String },
  MissingCurrency,
  MissingPostingAmount,
  MissingPriceNumber,
  MultipleAutoPostings,
  MultipleMissingInCurrency { currency: String, count: usize },
  MissingCurrencyForAutoPosting,
  Unbalanced { currency: String, residual: Decimal },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceError {
  pub kind: InferenceErrorKind,
  pub span: ast::Span,
  pub line: usize,
  pub column: usize,
  pub message: String,
}

impl InferenceError {
  fn new(
    kind: InferenceErrorKind,
    meta: &ast::Meta,
    span: ast::Span,
    message: String,
  ) -> Self {
    Self {
      kind,
      span,
      line: meta.line,
      column: meta.column,
      message,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredTransaction {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: Date,
  pub txn: Option<String>,
  pub payee: Option<String>,
  pub narration: Option<String>,
  pub tags: crate::core::SmallStrVec,
  pub links: crate::core::SmallStrVec,
  pub key_values: crate::core::SmallKeyValues,
  pub postings: Vec<InferredPosting>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredPosting {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub opt_flag: Option<String>,
  pub account: String,
  pub amount: InferredAmount,
  pub cost_spec: Option<crate::core::CostSpec>,
  pub price_operator: Option<ast::PriceOperator>,
  pub price_annotation: Option<InferredAmount>,
  pub comment: Option<String>,
  pub key_values: crate::core::SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredAmount {
  pub raw: String,
  pub number: Decimal,
  pub currency: String,
}

#[derive(Default)]
struct CurrencyState {
  sum: Decimal,
  missing_indices: Vec<usize>,
}

fn resolve_amount(
  amount: &Amount,
  meta: &ast::Meta,
  span: ast::Span,
) -> Result<InferredAmount, InferenceError> {
  if matches!(amount.number, NumberExpr::Missing) {
    return Err(InferenceError::new(
      InferenceErrorKind::MissingPostingAmount,
      meta,
      span,
      "posting amount missing after inference".to_string(),
    ));
  }

  let dec = number_expr_to_decimal(&amount.number).map_err(|err| {
    InferenceError::new(
      InferenceErrorKind::NumberEval {
        message: err.message.clone(),
      },
      meta,
      span,
      err.message,
    )
  })?;
  let normalized = dec.normalize();
  let currency = amount.currency.clone().ok_or_else(|| {
    InferenceError::new(
      InferenceErrorKind::MissingCurrency,
      meta,
      span,
      "posting amount is missing a currency".to_string(),
    )
  })?;

  Ok(InferredAmount {
    raw: normalized.to_string(),
    number: normalized,
    currency,
  })
}

fn resolve_cost_amount(
  amount: &CostAmount,
  meta: &ast::Meta,
  span: ast::Span,
) -> Result<CostAmount, InferenceError> {
  let per = amount
    .per
    .as_ref()
    .map(|n| match n {
      NumberExpr::Missing => Ok(NumberExpr::Missing),
      _ => number_expr_to_decimal(n)
        .map(|d| NumberExpr::Literal(d.normalize().to_string()))
        .map_err(|err| {
          InferenceError::new(
            InferenceErrorKind::NumberEval {
              message: err.message.clone(),
            },
            meta,
            span,
            err.message,
          )
        }),
    })
    .transpose()?;
  let total = amount
    .total
    .as_ref()
    .map(|n| match n {
      NumberExpr::Missing => Ok(NumberExpr::Missing),
      _ => number_expr_to_decimal(n)
        .map(|d| NumberExpr::Literal(d.normalize().to_string()))
        .map_err(|err| {
          InferenceError::new(
            InferenceErrorKind::NumberEval {
              message: err.message.clone(),
            },
            meta,
            span,
            err.message,
          )
        }),
    })
    .transpose()?;

  Ok(CostAmount {
    per,
    total,
    currency: amount.currency.clone(),
  })
}

fn resolve_cost_spec(
  cost_spec: &CostSpec,
  meta: &ast::Meta,
  span: ast::Span,
) -> Result<CostSpec, InferenceError> {
  let amount = cost_spec
    .amount
    .as_ref()
    .map(|a| resolve_cost_amount(a, meta, span))
    .transpose()?;

  Ok(CostSpec {
    amount,
    raw: cost_spec.raw.clone(),
    date: cost_spec.date,
    label: cost_spec.label.clone(),
    merge: cost_spec.merge,
    is_total: cost_spec.is_total,
  })
}

fn resolve_price(
  price: &Option<Amount>,
  meta: &ast::Meta,
  span: ast::Span,
) -> Result<Option<InferredAmount>, InferenceError> {
  match price {
    None => Ok(None),
    Some(amount) => {
      if matches!(amount.number, NumberExpr::Missing) {
        Err(InferenceError::new(
          InferenceErrorKind::MissingPriceNumber,
          meta,
          span,
          "price number missing after inference".to_string(),
        ))
      } else {
        Ok(Some(resolve_amount(amount, meta, span)?))
      }
    }
  }
}

/// Infer missing posting amounts for each transaction while preserving other directives.
///
/// For each currency within a transaction, if exactly one posting amount is missing
/// we balance that currency by assigning the opposite of the running total. We continue
/// processing subsequent directives even if a transaction fails to infer, accumulating
/// all `InferenceError`s. Transactions that fail inference are returned as
/// `InferredDirective::Other` alongside the collected errors.
pub fn infer_directives(
  directives: Vec<Directive>,
) -> (Vec<InferredDirective>, Vec<InferenceError>) {
  let mut inferred = Vec::with_capacity(directives.len());
  let mut errors = Vec::new();

  for directive in directives {
    match directive {
      Directive::Transaction(txn) => {
        let original = Directive::Transaction(txn.clone());
        match infer_transaction_postings(txn) {
          Ok(txn) => inferred.push(InferredDirective::Transaction(txn)),
          Err(err) => {
            errors.push(err);
            inferred.push(InferredDirective::Other(original));
          }
        }
      }
      other => inferred.push(InferredDirective::Other(other)),
    }
  }

  (inferred, errors)
}

pub fn infer_transaction_postings(
  mut txn: Transaction,
) -> Result<InferredTransaction, InferenceError> {
  let mut currencies: HashMap<String, CurrencyState> = HashMap::new();
  let mut missing_without_amount: Vec<usize> = Vec::new();
  let mut currencyless_with_amount: Vec<(usize, Decimal)> = Vec::new();

  for (idx, posting) in txn.postings.iter().enumerate() {
    match &posting.amount {
      Some(amount) => match &amount.currency {
        Some(currency) => match &amount.number {
          NumberExpr::Missing => {
            currencies
              .entry(currency.clone())
              .or_default()
              .missing_indices
              .push(idx);
          }
          _ => {
            let value = number_expr_to_decimal(&amount.number).map_err(|err| {
              InferenceError::new(
                InferenceErrorKind::NumberEval {
                  message: err.message.clone(),
                },
                &posting.meta,
                posting.span,
                err.message,
              )
            })?;

            currencies.entry(currency.clone()).or_default().sum += value;
          }
        },
        None => match &amount.number {
          NumberExpr::Missing => {
            // Amount present but both number and currency missing:
            // treat as fully missing posting.
            missing_without_amount.push(idx);
          }
          _ => {
            // Amount present with a number but no currency:
            // currency will be inferred from other postings.
            let value = number_expr_to_decimal(&amount.number).map_err(|err| {
              InferenceError::new(
                InferenceErrorKind::NumberEval {
                  message: err.message.clone(),
                },
                &posting.meta,
                posting.span,
                err.message,
              )
            })?;
            currencyless_with_amount.push((idx, value));
          }
        },
      },
      None => {
        missing_without_amount.push(idx);
      }
    }
  }

  // Infer currency for postings that have amounts but no currency.
  if !currencyless_with_amount.is_empty() {
    let inferred_currency = match (currencies.len(), currencies.keys().next()) {
      (0, _) => {
        return Err(InferenceError::new(
          InferenceErrorKind::MissingCurrency,
          &txn.postings[currencyless_with_amount[0].0].meta,
          txn.postings[currencyless_with_amount[0].0].span,
          "posting amount is missing a currency; no other currency found in transaction"
            .to_string(),
        ));
      }
      (1, Some(currency)) => currency.clone(),
      _ => {
        return Err(InferenceError::new(
          InferenceErrorKind::MissingCurrency,
          &txn.postings[currencyless_with_amount[0].0].meta,
          txn.postings[currencyless_with_amount[0].0].span,
          "posting amount is missing a currency; multiple currencies in transaction"
            .to_string(),
        ));
      }
    };

    for (idx, value) in &currencyless_with_amount {
      currencies.entry(inferred_currency.clone()).or_default().sum += *value;
      // Update the posting to carry the inferred currency for later resolution.
      let target = &mut txn.postings[*idx];
      if let Some(ref mut amount) = target.amount {
        amount.currency = Some(inferred_currency.clone());
      }
    }
  }

  if missing_without_amount.len() > 1 {
    return Err(InferenceError::new(
      InferenceErrorKind::MultipleAutoPostings,
      &txn.meta,
      txn.span,
      "You may not have more than one auto-posting per currency".to_string(),
    ));
  }

  if let Some(&missing_idx) = missing_without_amount.first() {
    let currency = match currencies.keys().next() {
      Some(c) if currencies.len() == 1 => c.clone(),
      _ => {
        return Err(InferenceError::new(
          InferenceErrorKind::MissingCurrencyForAutoPosting,
          &txn.meta,
          txn.span,
          "posting is missing an amount; cannot infer without a currency".to_string(),
        ));
      }
    };

    let state = match currencies.get_mut(&currency) {
      Some(state) => state,
      None => unreachable!("inference invariant violated: currency state missing"),
    };
    if !state.missing_indices.is_empty() {
      return Err(InferenceError::new(
        InferenceErrorKind::MultipleMissingInCurrency {
          currency: currency.clone(),
          count: state.missing_indices.len() + 1,
        },
        &txn.meta,
        txn.span,
        "cannot infer amounts: multiple postings are missing".to_string(),
      ));
    }

    let inferred = (-state.sum).normalize();
    let amount_str = inferred.to_string();

    debug_assert!(
      missing_idx < txn.postings.len(),
      "inference invariant violated: missing posting index"
    );
    let target = &mut txn.postings[missing_idx];

    target.amount = Some(Amount {
      raw: amount_str.clone(),
      number: NumberExpr::Literal(amount_str),
      currency: Some(currency.clone()),
    });

    // Keep the currency residuals consistent so later balancing checks pass.
    state.sum += inferred;
  }

  for (currency, state) in currencies {
    if state.missing_indices.is_empty() {
      if !state.sum.is_zero() {
        return Err(InferenceError::new(
          InferenceErrorKind::Unbalanced {
            currency: currency.clone(),
            residual: state.sum,
          },
          &txn.meta,
          txn.span,
          format!("Transaction does not balance: ({} {})", state.sum, currency,),
        ));
      }
      continue;
    }

    if state.missing_indices.len() > 1 {
      return Err(InferenceError::new(
        InferenceErrorKind::MultipleMissingInCurrency {
          currency: currency.clone(),
          count: state.missing_indices.len(),
        },
        &txn.meta,
        txn.span,
        format!(
          "cannot infer amounts: {} postings are missing in currency {currency}",
          state.missing_indices.len()
        ),
      ));
    }

    let missing_idx = state.missing_indices[0];
    let inferred = (-state.sum).normalize();
    let amount_str = inferred.to_string();

    debug_assert!(
      missing_idx < txn.postings.len(),
      "inference invariant violated: missing posting index"
    );
    let target = &mut txn.postings[missing_idx];

    target.amount = Some(Amount {
      raw: amount_str.clone(),
      number: NumberExpr::Literal(amount_str),
      currency: Some(currency),
    });
  }

  let postings = txn
    .postings
    .into_iter()
    .map(|p| match p.amount {
      Some(amount) if !matches!(amount.number, NumberExpr::Missing) => {
        let amount = resolve_amount(&amount, &p.meta, p.span)?;
        let cost_spec = p
          .cost_spec
          .as_ref()
          .map(|c| resolve_cost_spec(c, &p.meta, p.span))
          .transpose()?;
        let price_annotation = resolve_price(&p.price_annotation, &p.meta, p.span)?;

        Ok(InferredPosting {
          meta: p.meta,
          span: p.span,
          opt_flag: p.opt_flag,
          account: p.account,
          amount,
          cost_spec,
          price_operator: p.price_operator,
          price_annotation,
          comment: p.comment,
          key_values: p.key_values,
        })
      }
      _ => Err(InferenceError::new(
        InferenceErrorKind::MissingPostingAmount,
        &p.meta,
        p.span,
        "posting amount missing after inference".to_string(),
      )),
    })
    .collect::<Result<Vec<_>, _>>()?;

  // Re-validate balance using the resolved, normalized amounts to catch any
  // residual drift introduced during inference or normalization steps.
  let mut final_sums: HashMap<String, Decimal> = HashMap::new();
  for posting in &postings {
    *final_sums
      .entry(posting.amount.currency.clone())
      .or_default() += posting.amount.number;
  }

  if let Some((currency, residual)) = final_sums.into_iter().find(|(_, sum)| !sum.is_zero())
  {
    return Err(InferenceError::new(
      InferenceErrorKind::Unbalanced {
        currency: currency.clone(),
        residual,
      },
      &txn.meta,
      txn.span,
      format!(
        "transaction is not balanced for currency {}: residual {}",
        currency, residual
      ),
    ));
  }

  Ok(InferredTransaction {
    meta: txn.meta,
    span: txn.span,
    date: txn.date,
    txn: txn.txn,
    payee: txn.payee,
    narration: txn.narration,
    tags: txn.tags,
    links: txn.links,
    key_values: txn.key_values,
    postings,
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::core::{Amount, CostAmount, CostSpec, Directive, NumberExpr, Posting};
  use std::sync::Arc;

  fn meta() -> ast::Meta {
    ast::Meta {
      filename: Arc::new("test.beancount".to_string()),
      ..Default::default()
    }
  }

  fn span() -> ast::Span {
    ast::Span::default()
  }

  fn posting(amount: Amount) -> Posting {
    Posting {
      meta: meta(),
      span: span(),
      account: "Assets:Cash".to_string(),
      amount: Some(amount),
      ..Default::default()
    }
  }

  fn posting_without_amount(account: &str) -> Posting {
    Posting {
      meta: meta(),
      span: span(),
      account: account.to_string(),
      amount: None,
      ..Default::default()
    }
  }

  fn literal_amount(raw: &str, currency: &str) -> Amount {
    Amount {
      raw: raw.to_string(),
      number: NumberExpr::Literal(raw.to_string()),
      currency: Some(currency.to_string()),
    }
  }

  fn txn_with_postings(postings: Vec<Posting>) -> Directive {
    let date = jiff::civil::date(2024, 1, 1);
    Directive::Transaction(Transaction {
      meta: meta(),
      span: span(),
      date,
      txn: Some("*".to_string()),
      tags: Default::default(),
      links: Default::default(),
      postings: postings.into(),
      ..Default::default()
    })
  }

  #[test]
  fn infers_single_missing_amount() {
    let missing = Amount {
      raw: "".to_string(),
      number: NumberExpr::Missing,
      currency: Some("USD".to_string()),
    };
    let present = Amount {
      raw: "10".to_string(),
      number: NumberExpr::Literal("10".to_string()),
      currency: Some("USD".to_string()),
    };

    let directives = vec![txn_with_postings(vec![posting(missing), posting(present)])];
    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty());

    match &inferred[0] {
      InferredDirective::Transaction(txn) => {
        assert_eq!(txn.postings.len(), 2);
        let inferred_amt = &txn.postings[0].amount;
        assert_eq!(inferred_amt.currency, "USD");
        assert_eq!(inferred_amt.number, Decimal::new(-10, 0));
        assert_eq!(inferred_amt.raw, "-10");
      }
      _ => panic!("expected transaction"),
    }
  }

  #[test]
  fn fails_on_multiple_missing_same_currency() {
    let missing_usd = Amount {
      raw: "".to_string(),
      number: NumberExpr::Missing,
      currency: Some("USD".to_string()),
    };

    let directives = vec![txn_with_postings(vec![
      posting(missing_usd.clone()),
      posting(missing_usd),
    ])];
    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "expected one inference error");
    assert!(errors[0].message.contains("cannot infer amounts"));

    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn infers_single_missing_without_amount_when_currency_unique() {
    let mut cash = posting(literal_amount("1", "CNY"));
    cash.account = "Assets:Cash".to_string();

    let food = posting_without_amount("Expenses:Food");

    let directives = vec![txn_with_postings(vec![cash, food])];
    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty());

    let InferredDirective::Transaction(txn) = &inferred[0] else {
      panic!("expected transaction");
    };

    assert_eq!(txn.postings.len(), 2);
    let inferred_amt = &txn.postings[1].amount;
    assert_eq!(inferred_amt.currency, "CNY");
    assert_eq!(inferred_amt.number, Decimal::new(-1, 0));
    assert_eq!(inferred_amt.raw, "-1");
  }

  #[test]
  fn fails_when_all_amounts_present_but_unbalanced() {
    let cash = posting(literal_amount("1", "CNY"));
    let mut food = posting(literal_amount("-10", "CNY"));
    food.account = "Expenses:Food".to_string();

    let directives = vec![txn_with_postings(vec![cash, food])];
    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "should report unbalanced txn");
    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::Unbalanced { ref currency, .. } if currency == "CNY"
    ));
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn fails_with_multiple_missing_without_amount() {
    let p1 = posting_without_amount("Expenses:Food");
    let p2 = posting_without_amount("Assets:Cash");

    let directives = vec![txn_with_postings(vec![p1, p2])];
    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "should fail with two missing amounts");

    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::MultipleAutoPostings
    ));
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn infers_missing_per_currency_independently() {
    let usd_missing = Amount {
      raw: "".to_string(),
      number: NumberExpr::Missing,
      currency: Some("USD".to_string()),
    };
    let usd_present = literal_amount("5", "USD");
    let eur_present = literal_amount("7", "EUR");
    let eur_missing = Amount {
      raw: "".to_string(),
      number: NumberExpr::Missing,
      currency: Some("EUR".to_string()),
    };

    let directives = vec![txn_with_postings(vec![
      posting(usd_missing),
      posting(usd_present),
      posting(eur_present),
      posting(eur_missing),
    ])];

    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty());
    let InferredDirective::Transaction(txn) = &inferred[0] else {
      panic!("expected transaction");
    };

    assert_eq!(txn.postings.len(), 4);
    let usd_inferred = &txn.postings[0].amount;
    assert_eq!(usd_inferred.currency, "USD");
    assert_eq!(usd_inferred.number, Decimal::new(-5, 0));

    let eur_inferred = &txn.postings[3].amount;
    assert_eq!(eur_inferred.currency, "EUR");
    assert_eq!(eur_inferred.number, Decimal::new(-7, 0));
  }

  #[test]
  fn normalizes_cost_spec_numbers() {
    let amt = literal_amount("1", "USD");
    let cost_amount = CostAmount {
      per: Some(NumberExpr::Literal("01.2300".to_string())),
      total: Some(NumberExpr::Literal("2_0".to_string())),
      currency: Some("USD".to_string()),
    };
    let cost_spec = CostSpec {
      amount: Some(cost_amount),
      raw: String::new(),
      ..Default::default()
    };

    let mut p = posting(amt);
    p.cost_spec = Some(cost_spec);

    let balancing = posting(literal_amount("-1", "USD"));
    let directives = vec![txn_with_postings(vec![p, balancing])];
    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty());
    let InferredDirective::Transaction(txn) = &inferred[0] else {
      panic!("expected transaction");
    };
    let cost_spec = txn.postings[0]
      .cost_spec
      .as_ref()
      .expect("cost spec present");
    let amount = cost_spec.amount.as_ref().expect("cost amount present");

    assert!(matches!(amount.per, Some(NumberExpr::Literal(ref n)) if n == "1.23"));
    assert!(matches!(amount.total, Some(NumberExpr::Literal(ref n)) if n == "20"));
  }

  #[test]
  fn normalizes_price_annotation() {
    let amt = literal_amount("10", "USD");
    let price = literal_amount("2.500", "CAD");

    let mut p = posting(amt);
    p.price_annotation = Some(price);

    let balancing = posting(literal_amount("-10", "USD"));
    let directives = vec![txn_with_postings(vec![p, balancing])];
    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty());
    let InferredDirective::Transaction(txn) = &inferred[0] else {
      panic!("expected transaction");
    };
    let price = txn.postings[0]
      .price_annotation
      .as_ref()
      .expect("price annotation present");

    assert_eq!(price.currency, "CAD");
    assert_eq!(price.number, Decimal::new(25, 1));
    assert_eq!(price.raw, "2.5");
  }

  #[test]
  fn fails_on_unbalanced_parsed_transaction() {
    let source = r#"
2020-05-05 * "馄饨"
  Assets:Cash                                         1 CNY
  Expenses:Food  -10 CNY
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "unbalanced transaction should be rejected");

    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::Unbalanced { ref currency, .. } if currency == "CNY"
    ));
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn infers_currency_for_amounts_without_currency() {
    // Postings with amounts but no currency should inherit the currency
    // from other postings in the same transaction that do have a currency.
    let source = r#"
2022-01-01 * "Payee" "Narration"
  Assets:Cash1 1
  Assets:Cash2 2 CNY
  Assets:Cash3 -3
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);

    let InferredDirective::Transaction(txn) = &inferred[0] else {
      panic!("expected transaction");
    };

    assert_eq!(txn.postings.len(), 3);
    // All postings should have CNY as currency
    for posting in &txn.postings {
      assert_eq!(posting.amount.currency, "CNY");
    }
    assert_eq!(txn.postings[0].amount.number, Decimal::new(1, 0));
    assert_eq!(txn.postings[1].amount.number, Decimal::new(2, 0));
    assert_eq!(txn.postings[2].amount.number, Decimal::new(-3, 0));
  }

  #[test]
  fn infers_currency_with_auto_posting() {
    // Currencyless posting + fully missing posting: currency should be
    // inferred, and the missing posting auto-balanced.
    let source = r#"
2022-01-01 * "Payee" "Narration"
  Assets:Cash1 10
  Assets:Cash2
  Assets:Cash3 2 CNY
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);

    let InferredDirective::Transaction(txn) = &inferred[0] else {
      panic!("expected transaction");
    };

    assert_eq!(txn.postings.len(), 3);
    assert_eq!(txn.postings[0].amount.currency, "CNY");
    assert_eq!(txn.postings[0].amount.number, Decimal::new(10, 0));
    assert_eq!(txn.postings[1].amount.currency, "CNY");
    assert_eq!(txn.postings[1].amount.number, Decimal::new(-12, 0));
    assert_eq!(txn.postings[2].amount.currency, "CNY");
    assert_eq!(txn.postings[2].amount.number, Decimal::new(2, 0));
  }

  #[test]
  fn fails_currencyless_when_no_explicit_currency() {
    // All postings have amounts but none have a currency → error.
    let source = r#"
2022-01-01 * "Payee" "Narration"
  Assets:Cash1 1
  Assets:Cash2 -1
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "expected one inference error");
    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::MissingCurrency
    ));
    assert!(
      errors[0]
        .message
        .contains("no other currency found in transaction")
    );
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn fails_currencyless_when_multiple_explicit_currencies() {
    // Posting without currency when multiple explicit currencies exist → ambiguous.
    let source = r#"
2022-01-01 * "Payee" "Narration"
  Assets:Cash1 1
  Assets:Cash2 2 USD
  Assets:Cash3 3 CNY
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "expected one inference error");
    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::MissingCurrency
    ));
    assert!(
      errors[0]
        .message
        .contains("multiple currencies in transaction")
    );
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn fails_currencyless_when_unbalanced() {
    // Currencyless postings that don't balance after inference → error.
    let source = r#"
2022-01-01 * "Payee" "Narration"
  Assets:Cash1 1
  Assets:Cash2 2 CNY
  Assets:Cash3 -5
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "expected one inference error");
    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::Unbalanced { ref currency, .. } if currency == "CNY"
    ));
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }

  #[test]
  fn fails_when_all_postings_missing_amounts() {
    let source = r#"
2020-05-05 * "馄饨"
  Assets:Cash
  Expenses:Food
"#;

    let parsed = beancount_parser::parse_strict(source).unwrap();
    let directives = crate::core::normalize_directives(&parsed, "test.beancount", source)
      .expect("normalize directives");

    let (inferred, errors) = infer_directives(directives);
    assert_eq!(errors.len(), 1, "missing amounts should be rejected");

    assert!(matches!(
      errors[0].kind,
      InferenceErrorKind::MultipleAutoPostings
    ));
    assert!(matches!(inferred[0], InferredDirective::Other(_)));
  }
}
