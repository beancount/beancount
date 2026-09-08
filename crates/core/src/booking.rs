use crate::core::{NumberExpr, number_expr_to_decimal};
use crate::{Amount, Cost, Directive, KeyValue, KeyValueValue, Open, Posting, Transaction};
use beancount_parser::ast;
use rust_decimal::Decimal;
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BookingErrorKind {
  AmountZero,
  CostNegative,
  ReductionNoMatch,
  CannotInfer,
  NotImplemented,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BookingError {
  pub meta: ast::Meta,
  pub kind: BookingErrorKind,
  pub message: String,
  pub entry: Option<Transaction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BookingConfig {
  /// Equivalent to options_map["infer_tolerance_from_cost"].
  pub infer_tolerance_from_cost: bool,
  /// Equivalent to options_map["tolerance_multiplier"]. Default: 0.5
  pub tolerance_multiplier: Decimal,
  /// Equivalent to options_map["inferred_tolerance_default"].
  pub inferred_tolerance_default: BTreeMap<String, Decimal>,
}

impl Default for BookingConfig {
  fn default() -> Self {
    Self {
      infer_tolerance_from_cost: false,
      tolerance_multiplier: Decimal::new(5, 1),
      inferred_tolerance_default: BTreeMap::new(),
    }
  }
}

fn maximum_tolerance() -> Decimal {
  Decimal::new(5, 1) // 0.5
}

#[derive(Debug, Clone)]
struct Lot {
  units_currency: String,
  cost: Cost,
  cost_number: Decimal,
  remaining: Decimal,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct BookingState {
  lots_by_account: HashMap<String, Vec<Lot>>,
  booking_none_by_account: HashMap<String, bool>,
}

#[derive(Debug, Clone)]
pub(crate) struct BookingCheckpoint {
  pub start_index: usize,
  pub state: BookingState,
  pub booked_len: usize,
  pub error_len: usize,
}

fn posting_bucket_currency(posting: &Posting) -> Option<&str> {
  if let Some(cost) = posting.cost.as_ref() {
    return Some(cost.currency.as_str());
  }
  if let Some(cost_spec) = posting.cost_spec.as_ref()
    && let Some(cost_amount) = cost_spec.amount.as_ref()
    && let Some(currency) = cost_amount.currency.as_deref()
  {
    return Some(currency);
  }
  if let Some(price) = posting.price_annotation.as_ref()
    && let Some(currency) = price.currency.as_deref()
  {
    return Some(currency);
  }
  posting.amount.as_ref().and_then(|a| a.currency.as_deref())
}

fn posting_weight(posting: &Posting) -> Option<(Decimal, &str)> {
  let units = posting.amount.as_ref()?;
  if matches!(units.number, NumberExpr::Missing) {
    return None;
  }
  let units_currency = units.currency.as_deref()?;
  let units_number = number_expr_to_decimal(&units.number).ok()?;

  if let Some(cost) = posting.cost.as_ref() {
    let cost_number = number_expr_to_decimal(&cost.number).ok()?;
    return Some((units_number * cost_number, cost.currency.as_str()));
  }

  if let Some(price) = posting.price_annotation.as_ref() {
    let price_currency = price.currency.as_deref()?;
    if matches!(price.number, NumberExpr::Missing) {
      return None;
    }
    let price_number = number_expr_to_decimal(&price.number).ok()?;
    if posting.price_operator == Some(ast::PriceOperator::Total) {
      return Some((price_number, price_currency));
    }
    return Some((units_number * price_number, price_currency));
  }

  Some((units_number, units_currency))
}

fn reorder_postings_by_currency_groups(txn: &mut Transaction) {
  // Mirror booking_full's grouping effect on posting order.
  let mut currency_order: Vec<String> = Vec::new();
  let mut seen: HashMap<String, ()> = HashMap::new();

  for posting in txn.postings.iter() {
    let Some(currency) = posting_bucket_currency(posting) else {
      continue;
    };
    if seen.insert(currency.to_string(), ()).is_none() {
      currency_order.push(currency.to_string());
    }
  }

  if currency_order.len() <= 1 {
    return;
  }

  let original = txn.postings.clone();
  let mut grouped = crate::SmallPostings::new();

  for currency in &currency_order {
    for posting in original.iter() {
      if posting_bucket_currency(posting) == Some(currency.as_str()) {
        grouped.push(posting.clone());
      }
    }
  }

  for posting in original.iter() {
    if posting_bucket_currency(posting).is_none() {
      grouped.push(posting.clone());
    }
  }

  txn.postings = grouped;
}

fn infer_tolerances(txn: &Transaction, config: &BookingConfig) -> Vec<(String, Decimal)> {
  // Port of beancount.core.interpolate.infer_tolerances(), returning an ordered
  // list of (currency, tolerance) pairs to preserve insertion order.
  let mut seen_currencies: BTreeSet<String> = BTreeSet::new();
  for posting in &txn.postings {
    if let Some(units) = posting.amount.as_ref()
      && !matches!(units.number, NumberExpr::Missing)
      && let Some(curr) = units.currency.as_deref()
    {
      seen_currencies.insert(curr.to_string());
    }
    if let Some(cost) = posting.cost.as_ref() {
      seen_currencies.insert(cost.currency.clone());
    }
    if let Some(price) = posting.price_annotation.as_ref()
      && let Some(curr) = price.currency.as_deref()
    {
      seen_currencies.insert(curr.to_string());
    }
  }

  let mut order: Vec<String> = Vec::new();
  let mut tolerances: HashMap<String, Decimal> = HashMap::new();

  // Seed from defaults (include "*" and currencies seen).
  for (currency, tol) in &config.inferred_tolerance_default {
    if currency == "*" || seen_currencies.contains(currency) {
      tolerances.insert(currency.clone(), *tol);
      order.push(currency.clone());
    }
  }

  let mut cost_order: Vec<String> = Vec::new();
  let mut cost_tolerances: HashMap<String, Decimal> = HashMap::new();

  for posting in &txn.postings {
    // Skip automatically inferred postings.
    if posting
      .key_values
      .iter()
      .any(|kv| kv.key == "__automatic__")
    {
      continue;
    }

    let Some(units) = posting.amount.as_ref() else {
      continue;
    };
    if matches!(units.number, NumberExpr::Missing) {
      continue;
    }
    let Some(currency) = units.currency.as_deref() else {
      continue;
    };
    let Ok(units_number) = number_expr_to_decimal(&units.number) else {
      continue;
    };

    let scale = units_number.scale();
    if scale == 0 {
      continue;
    }

    let tolerance = Decimal::new(1, scale) * config.tolerance_multiplier;
    let existing = tolerances
      .get(currency)
      .copied()
      .unwrap_or(Decimal::new(-1024, 0));
    if !tolerances.contains_key(currency) {
      tolerances.insert(currency.to_string(), tolerance);
      order.push(currency.to_string());
    } else if tolerance > existing {
      tolerances.insert(currency.to_string(), tolerance);
    }

    if !config.infer_tolerance_from_cost {
      continue;
    }

    // Cost contribution.
    if let Some(cost) = posting.cost.as_ref() {
      if let Ok(cost_number) = number_expr_to_decimal(&cost.number) {
        let cost_tol = (tolerance * cost_number).min(maximum_tolerance());
        let entry = cost_tolerances
          .entry(cost.currency.clone())
          .or_insert_with(|| {
            cost_order.push(cost.currency.clone());
            Decimal::ZERO
          });
        *entry += cost_tol;
      }
    } else if let Some(cost_spec) = posting.cost_spec.as_ref()
      && let Some(cost_amount) = cost_spec.amount.as_ref()
      && let Some(cost_currency) = cost_amount.currency.as_deref()
    {
      let mut cost_tol = maximum_tolerance();
      for maybe_number in [&cost_amount.total, &cost_amount.per] {
        let Some(expr) = maybe_number.as_ref() else {
          continue;
        };
        if let Ok(cost_number) = number_expr_to_decimal(expr) {
          cost_tol = (tolerance * cost_number).min(cost_tol);
        }
      }
      let key = cost_currency.to_string();
      let entry = cost_tolerances.entry(key.clone()).or_insert_with(|| {
        cost_order.push(key.clone());
        Decimal::ZERO
      });
      *entry += cost_tol;
    }

    // Price contribution.
    if let Some(price) = posting.price_annotation.as_ref()
      && let (Some(price_currency), Ok(price_number)) = (
        price.currency.as_deref(),
        number_expr_to_decimal(&price.number),
      )
    {
      let price_tol = (tolerance * price_number).min(maximum_tolerance());
      let key = price_currency.to_string();
      let entry = cost_tolerances.entry(key.clone()).or_insert_with(|| {
        cost_order.push(key.clone());
        Decimal::ZERO
      });
      *entry += price_tol;
    }
  }

  for currency in cost_order {
    let Some(tol) = cost_tolerances.get(&currency).copied() else {
      continue;
    };
    let existing = tolerances
      .get(&currency)
      .copied()
      .unwrap_or(Decimal::new(-1024, 0));

    if !tolerances.contains_key(&currency) {
      tolerances.insert(currency.clone(), tol);
      order.push(currency);
    } else if tol > existing {
      tolerances.insert(currency, tol);
    }
  }

  order
    .into_iter()
    .filter_map(|currency| {
      tolerances
        .get(&currency)
        .copied()
        .map(|tol| (currency, tol))
    })
    .collect()
}

fn is_booking_none(open: &Open) -> bool {
  open
    .opt_booking
    .as_deref()
    .is_some_and(|s| s.eq_ignore_ascii_case("NONE"))
}

pub fn book_directives(
  directives: Vec<Directive>,
  config: &BookingConfig,
) -> (Vec<Directive>, Vec<BookingError>) {
  let (booked, errors, _) =
    book_directives_with_state(&directives, config, BookingState::default());
  (booked, errors)
}

pub(crate) fn book_directives_with_state_and_checkpoints(
  directives: &[Directive],
  config: &BookingConfig,
  state: BookingState,
  checkpoint_interval: usize,
  start_index: usize,
) -> (
  Vec<Directive>,
  Vec<BookingError>,
  BookingState,
  Vec<BookingCheckpoint>,
) {
  let mut errors: Vec<BookingError> = Vec::new();
  let mut out: Vec<Directive> = Vec::with_capacity(directives.len());
  let mut checkpoints: Vec<BookingCheckpoint> = Vec::new();

  let mut state = state;

  if checkpoint_interval > 0 {
    checkpoints.push(BookingCheckpoint {
      start_index,
      state: state.clone(),
      booked_len: out.len(),
      error_len: errors.len(),
    });
  }

  for (offset, directive) in directives.iter().cloned().enumerate() {
    match directive {
      Directive::Open(open) => {
        if is_booking_none(&open) {
          state
            .booking_none_by_account
            .insert(open.account.clone(), true);
        }
        out.push(Directive::Open(open));
      }
      Directive::Transaction(mut txn) => {
        // If we can't categorize at all, emit an error and skip (matches booking_full behavior).
        if !txn
          .postings
          .iter()
          .any(|posting| posting_bucket_currency(posting).is_some())
        {
          errors.push(BookingError {
            meta: txn.meta.clone(),
            kind: BookingErrorKind::CannotInfer,
            message: "Failed to categorize transaction postings".to_string(),
            entry: Some(txn.clone()),
          });
          continue;
        }

        let txn_for_errors = txn.clone();

        // Minimal reduction booking by matching tracked lots.
        for posting in &mut txn.postings {
          if *state
            .booking_none_by_account
            .get(&posting.account)
            .unwrap_or(&false)
          {
            continue;
          }

          let Some(cost_spec) = posting.cost_spec.as_ref() else {
            continue;
          };
          let Some(units) = posting.amount.as_ref() else {
            continue;
          };
          if matches!(units.number, NumberExpr::Missing) {
            continue;
          }
          let Some(units_currency) = units.currency.as_deref() else {
            continue;
          };
          let Ok(units_number) = number_expr_to_decimal(&units.number) else {
            continue;
          };
          if units_number >= Decimal::ZERO {
            continue;
          }

          let Some(cost_amount) = cost_spec.amount.as_ref() else {
            continue;
          };
          let Some(cost_currency) = cost_amount.currency.as_deref() else {
            continue;
          };

          let per = cost_amount
            .per
            .as_ref()
            .and_then(|n| number_expr_to_decimal(n).ok());
          let total = cost_amount
            .total
            .as_ref()
            .and_then(|n| number_expr_to_decimal(n).ok());

          // Compute per-unit cost number.
          let per_unit: Option<Decimal> = if cost_spec.is_total {
            total.or(per).map(|t| t / units_number.abs())
          } else {
            match (per, total) {
              (Some(per_cost), None) => Some(per_cost),
              (None, Some(total_cost)) => Some(total_cost / units_number.abs()),
              (Some(per_cost), Some(total_cost)) => {
                let cost_total = total_cost + (per_cost * units_number);
                Some(cost_total / units_number.abs())
              }
              (None, None) => None,
            }
          };

          let Some(cost_number) = per_unit else {
            continue;
          };

          let lots = state
            .lots_by_account
            .entry(posting.account.clone())
            .or_default();

          let mut matched_index: Option<usize> = None;
          for (idx, lot) in lots.iter().enumerate() {
            if lot.units_currency != units_currency {
              continue;
            }
            if lot.cost.currency != cost_currency {
              continue;
            }
            if lot.cost_number == cost_number {
              matched_index = Some(idx);
              break;
            }
          }

          let Some(idx) = matched_index else {
            if lots.iter().any(|lot| lot.units_currency == units_currency) {
              errors.push(BookingError {
                meta: txn.meta.clone(),
                kind: BookingErrorKind::ReductionNoMatch,
                message: format!(
                  "No position matches reduction posting (account={}, units={} {}, cost={} {})",
                  posting.account,
                  units_number,
                  units_currency,
                  cost_number,
                  cost_currency
                ),
                entry: Some(txn_for_errors.clone()),
              });
            }
            continue;
          };

          let lot_cost = lots[idx].cost.clone();
          lots[idx].remaining += units_number; // negative
          if lots[idx].remaining.is_zero() {
            lots.remove(idx);
          }

          posting.cost = Some(lot_cost);
          posting.cost_spec = None;
        }

        // Convert any remaining CostSpec to concrete Cost (including those with price annotations).
        for posting in &mut txn.postings {
          let Some(cost_spec) = posting.cost_spec.as_ref() else {
            continue;
          };
          let Some(units) = posting.amount.as_ref() else {
            continue;
          };
          let Ok(units_number) = number_expr_to_decimal(&units.number) else {
            continue;
          };
          if units_number.is_zero() {
            continue;
          }

          let Some(cost_amount) = cost_spec.amount.as_ref() else {
            continue;
          };
          let Some(cost_currency) = cost_amount.currency.as_deref() else {
            continue;
          };

          let per = cost_amount
            .per
            .as_ref()
            .and_then(|n| number_expr_to_decimal(n).ok());
          let total = cost_amount
            .total
            .as_ref()
            .and_then(|n| number_expr_to_decimal(n).ok());

          let per_unit: Option<Decimal> = if cost_spec.is_total {
            total.or(per).map(|t| t / units_number.abs())
          } else {
            match (per, total) {
              (Some(per_cost), None) => Some(per_cost),
              (None, Some(total_cost)) => Some(total_cost / units_number.abs()),
              (Some(per_cost), Some(total_cost)) => {
                let cost_total = total_cost + (per_cost * units_number);
                Some(cost_total / units_number.abs())
              }
              (None, None) => None,
            }
          };

          let Some(cost_number) = per_unit else {
            continue;
          };

          let cost_date = cost_spec.date.unwrap_or(txn.date);

          posting.cost = Some(Cost {
            number: NumberExpr::Literal(cost_number.to_string()),
            currency: cost_currency.to_string(),
            date: cost_date,
            label: cost_spec.label.clone(),
          });
          posting.cost_spec = None;
        }

        // Track augmenting lots.
        for posting in &txn.postings {
          if *state
            .booking_none_by_account
            .get(&posting.account)
            .unwrap_or(&false)
          {
            continue;
          }

          let Some(units) = posting.amount.as_ref() else {
            continue;
          };
          if matches!(units.number, NumberExpr::Missing) {
            continue;
          }
          let Some(units_currency) = units.currency.as_deref() else {
            continue;
          };
          let Some(cost) = posting.cost.as_ref() else {
            continue;
          };

          let Ok(units_number) = number_expr_to_decimal(&units.number) else {
            continue;
          };
          if units_number <= Decimal::ZERO {
            continue;
          }
          let Ok(cost_number) = number_expr_to_decimal(&cost.number) else {
            continue;
          };

          state
            .lots_by_account
            .entry(posting.account.clone())
            .or_default()
            .push(Lot {
              units_currency: units_currency.to_string(),
              cost: cost.clone(),
              cost_number,
              remaining: units_number,
            });
        }

        // Infer exactly one missing CostSpec amount `{}` by balancing costs.
        let mut missing_cost_index: Option<usize> = None;
        let mut inferred_cost_currency: Option<String> = None;
        let mut sum_weights: Decimal = Decimal::ZERO;
        let mut any_known_weight = false;

        for (idx, posting) in txn.postings.iter().enumerate() {
          if posting.cost.is_none()
            && let Some(cost_spec) = posting.cost_spec.as_ref()
            && cost_spec.amount.is_none()
          {
            if missing_cost_index.is_some() {
              missing_cost_index = None;
              inferred_cost_currency = None;
              break;
            }
            missing_cost_index = Some(idx);
            continue;
          }

          let Some(cost) = posting.cost.as_ref() else {
            continue;
          };
          let Some(units) = posting.amount.as_ref() else {
            continue;
          };
          if matches!(units.number, NumberExpr::Missing) {
            continue;
          }
          let Ok(units_number) = number_expr_to_decimal(&units.number) else {
            continue;
          };
          let Ok(cost_number) = number_expr_to_decimal(&cost.number) else {
            continue;
          };

          any_known_weight = true;
          match &inferred_cost_currency {
            None => inferred_cost_currency = Some(cost.currency.clone()),
            Some(existing) if existing == &cost.currency => {}
            Some(_) => {
              missing_cost_index = None;
              inferred_cost_currency = None;
              break;
            }
          }
          sum_weights += units_number * cost_number;
        }

        if let (Some(midx), Some(curr)) =
          (missing_cost_index, inferred_cost_currency.clone())
          && any_known_weight
        {
          let posting = &mut txn.postings[midx];
          if let Some(units) = posting.amount.as_ref()
            && !matches!(units.number, NumberExpr::Missing)
            && let Ok(units_number) = number_expr_to_decimal(&units.number)
            && !units_number.is_zero()
          {
            let per_unit = (-sum_weights) / units_number;
            posting.cost = Some(Cost {
              number: NumberExpr::Literal(per_unit.to_string()),
              currency: curr,
              date: txn.date,
              label: None,
            });
            posting.cost_spec = None;
            posting.key_values.push(KeyValue {
              span: posting.span,
              key: "__automatic__".to_string(),
              value: Some(KeyValueValue::Bool(true)),
            });
          }
        }

        // Infer exactly one missing price number (e.g. `@ CAD`) by balancing
        // weights in that price currency, mirroring booking_full.
        {
          let mut missing_price_index: Option<usize> = None;
          for (idx, posting) in txn.postings.iter().enumerate() {
            let Some(price) = posting.price_annotation.as_ref() else {
              continue;
            };
            if price.currency.is_none() {
              continue;
            }
            if matches!(price.number, NumberExpr::Missing)
              || number_expr_to_decimal(&price.number).is_err()
            {
              if missing_price_index.is_some() {
                missing_price_index = None;
                break;
              }
              missing_price_index = Some(idx);
            }
          }

          if let Some(midx) = missing_price_index {
            let posting = &txn.postings[midx];
            if posting.cost.is_some() {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Cannot infer price for postings held at cost".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            }

            let Some(units) = posting.amount.as_ref() else {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Cannot infer price without units".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            };
            if matches!(units.number, NumberExpr::Missing) {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Cannot infer price with missing units".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            }
            let Ok(units_number) = number_expr_to_decimal(&units.number) else {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Cannot infer price with invalid units".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            };
            if units_number.is_zero() {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Cannot infer price for zero units".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            }

            let Some(price) = posting.price_annotation.as_ref() else {
              continue;
            };
            let Some(price_currency) = price.currency.as_deref() else {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Cannot infer price with missing currency".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            };

            let mut residual: Decimal = Decimal::ZERO;
            let mut any_other = false;
            let mut blocked = false;
            for (idx, other) in txn.postings.iter().enumerate() {
              if idx == midx {
                continue;
              }
              match posting_weight(other) {
                Some((weight, currency)) if currency == price_currency => {
                  residual += weight;
                  any_other = true;
                }
                Some((_weight, _currency)) => {}
                None => {
                  // If another posting might contribute to this currency group but
                  // can't be evaluated, don't attempt inference.
                  if posting_bucket_currency(other) == Some(price_currency) {
                    blocked = true;
                    break;
                  }
                }
              }
            }

            if blocked {
              errors.push(BookingError {
                meta: posting.meta.clone(),
                kind: BookingErrorKind::CannotInfer,
                message: "Could not infer missing price number".to_string(),
                entry: Some(txn_for_errors.clone()),
              });
              continue;
            }

            // If there are no other postings in this currency group, fall back
            // to assuming a zero residual.
            let desired_weight = if any_other { -residual } else { Decimal::ZERO };

            let new_price_number =
              if posting.price_operator == Some(ast::PriceOperator::Total) {
                desired_weight.abs()
              } else {
                (desired_weight / units_number).abs()
              };

            let mut posting = txn.postings[midx].clone();
            if let Some(mut price) = posting.price_annotation.clone() {
              price.number = NumberExpr::Literal(new_price_number.to_string());
              posting.price_annotation = Some(price);
              posting.key_values.push(KeyValue {
                span: posting.span,
                key: "__automatic__".to_string(),
                value: Some(KeyValueValue::Bool(true)),
              });
              txn.postings[midx] = posting;
            }
          }
        }

        // Infer exactly one missing posting amount by balancing weights.
        let mut missing_index: Option<usize> = None;
        let mut sums: BTreeMap<String, Decimal> = BTreeMap::new();
        let mut currencyless_with_amount: Vec<(usize, Decimal)> = Vec::new();
        let mut any_explicit = false;

        for (idx, posting) in txn.postings.iter().enumerate() {
          let Some(amount) = posting.amount.as_ref() else {
            if missing_index.is_some() {
              missing_index = None;
              break;
            }
            missing_index = Some(idx);
            continue;
          };

          if matches!(amount.number, NumberExpr::Missing) {
            if missing_index.is_some() {
              missing_index = None;
              break;
            }
            missing_index = Some(idx);
            continue;
          }

          let Ok(units_number) = number_expr_to_decimal(&amount.number) else {
            continue;
          };

          let Some(units_currency) = amount.currency.as_deref() else {
            // Posting has a number but no currency: infer currency from other postings.
            if posting.cost.is_some() || posting.price_annotation.is_some() {
              // Cost/price provides currency context; keep existing "missing" behavior
              // until cost-basis inference is fully implemented.
              if missing_index.is_some() {
                missing_index = None;
                break;
              }
              missing_index = Some(idx);
              continue;
            }
            // No cost, no price: currency will be inferred from explicit postings.
            currencyless_with_amount.push((idx, units_number));
            continue;
          };

          let (weight_number, weight_currency) = if let Some(cost) = posting.cost.as_ref() {
            let Ok(cost_number) = number_expr_to_decimal(&cost.number) else {
              continue;
            };
            (units_number * cost_number, cost.currency.as_str())
          } else if let Some(price) = posting.price_annotation.as_ref() {
            let Some(price_currency) = price.currency.as_deref() else {
              continue;
            };
            let Ok(price_number) = number_expr_to_decimal(&price.number) else {
              continue;
            };

            if posting.price_operator == Some(ast::PriceOperator::Total) {
              (price_number, price_currency)
            } else {
              (units_number * price_number, price_currency)
            }
          } else {
            (units_number, units_currency)
          };

          any_explicit = true;
          *sums
            .entry(weight_currency.to_string())
            .or_insert(Decimal::ZERO) += weight_number;
        }

        // Infer currency for postings that have amounts but no currency.
        if !currencyless_with_amount.is_empty() && sums.len() <= 1 {
          // If there's exactly one currency in explicit postings, use it.
          // If no explicit currency, sums is empty and we can't infer (error later).
          if sums.len() == 1
            && let Some(inferred_currency) = sums.keys().next().cloned()
          {
            for (idx, units_number) in &currencyless_with_amount {
              *sums.entry(inferred_currency.clone()).or_default() += units_number;
              let posting = &mut txn.postings[*idx];
              if let Some(ref mut amount) = posting.amount {
                amount.currency = Some(inferred_currency.clone());
              }
            }
            any_explicit = true;
          }
          // If sums.len() == 0 (no explicit postings) or > 1 (ambiguous),
          // currencyless postings remain unresolved → error caught below.
        }

        if let Some(midx) = missing_index
          && any_explicit
          && !sums.is_empty()
        {
          if sums.len() == 1 {
            let Some((curr, sum)) = sums.into_iter().next() else {
              debug_assert!(false, "sums.len()==1 but no element");
              continue;
            };
            let inferred = -sum;
            let posting = &mut txn.postings[midx];

            posting.amount = Some(Amount {
              raw: inferred.to_string(),
              number: NumberExpr::Literal(inferred.to_string()),
              currency: Some(curr),
            });
            posting.key_values.push(KeyValue {
              span: posting.span,
              key: "__automatic__".to_string(),
              value: Some(KeyValueValue::Bool(true)),
            });
          } else {
            let template = txn.postings.remove(midx);
            let mut inferred_postings: Vec<Posting> = Vec::new();
            for (curr, sum) in sums {
              let inferred = -sum;
              let mut posting = template.clone();
              posting.amount = Some(Amount {
                raw: inferred.to_string(),
                number: NumberExpr::Literal(inferred.to_string()),
                currency: Some(curr),
              });
              posting.key_values.push(KeyValue {
                span: posting.span,
                key: "__automatic__".to_string(),
                value: Some(KeyValueValue::Bool(true)),
              });
              inferred_postings.push(posting);
            }

            for (offset, posting) in inferred_postings.into_iter().enumerate() {
              txn.postings.insert(midx + offset, posting);
            }
          }
        }

        // Skip transactions that still have unresolved units.
        if txn
          .postings
          .iter()
          .any(|posting| match posting.amount.as_ref() {
            None => true,
            Some(amount) => {
              matches!(amount.number, NumberExpr::Missing) || amount.currency.is_none()
            }
          })
        {
          errors.push(BookingError {
            meta: txn.meta.clone(),
            kind: BookingErrorKind::CannotInfer,
            message: "Could not infer missing posting units".to_string(),
            entry: Some(txn_for_errors.clone()),
          });
          continue;
        }

        // Skip transactions that still have unresolved prices; Python validation
        // cannot handle MISSING weights.
        if txn.postings.iter().any(|posting| {
          posting.price_annotation.as_ref().is_some_and(|price| {
            price.currency.is_none()
              || matches!(price.number, NumberExpr::Missing)
              || number_expr_to_decimal(&price.number).is_err()
          })
        }) {
          errors.push(BookingError {
            meta: txn.meta.clone(),
            kind: BookingErrorKind::CannotInfer,
            message: "Could not infer missing price".to_string(),
            entry: Some(txn_for_errors.clone()),
          });
          continue;
        }

        txn.tolerances = Some(infer_tolerances(&txn, config));
        reorder_postings_by_currency_groups(&mut txn);

        // Basic cost validations (ported from Python booking_full).
        for posting in &txn.postings {
          let Some(cost_spec) = &posting.cost_spec else {
            continue;
          };
          let Some(units) = &posting.amount else {
            continue;
          };
          let units_number = match number_expr_to_decimal(&units.number) {
            Ok(n) => n,
            Err(_) => continue,
          };
          if units_number.is_zero() {
            errors.push(BookingError {
              meta: posting.meta.clone(),
              kind: BookingErrorKind::AmountZero,
              message: "Amount is zero".to_string(),
              entry: Some(txn_for_errors.clone()),
            });
            continue;
          }

          let Some(cost_amount) = &cost_spec.amount else {
            continue;
          };
          let per = cost_amount
            .per
            .as_ref()
            .and_then(|n| number_expr_to_decimal(n).ok());
          let total = cost_amount
            .total
            .as_ref()
            .and_then(|n| number_expr_to_decimal(n).ok());

          let per_unit: Option<Decimal> = if cost_spec.is_total {
            total.or(per).map(|t| t / units_number.abs())
          } else {
            match (per, total) {
              (Some(per_cost), Some(total_cost)) => {
                let cost_total = total_cost + (per_cost * units_number);
                Some(cost_total / units_number.abs())
              }
              (Some(per_cost), None) => Some(per_cost),
              (None, Some(total_cost)) => Some(total_cost / units_number.abs()),
              (None, None) => None,
            }
          };

          if let Some(cost_number) = per_unit
            && cost_number < Decimal::ZERO
          {
            errors.push(BookingError {
              meta: posting.meta.clone(),
              kind: BookingErrorKind::CostNegative,
              message: "Cost is negative".to_string(),
              entry: Some(txn_for_errors.clone()),
            });
          }
        }

        out.push(Directive::Transaction(txn));
      }
      other => out.push(other),
    }

    let absolute_index = start_index + offset + 1;
    if checkpoint_interval > 0 && absolute_index.is_multiple_of(checkpoint_interval) {
      checkpoints.push(BookingCheckpoint {
        start_index: absolute_index,
        state: state.clone(),
        booked_len: out.len(),
        error_len: errors.len(),
      });
    }
  }

  (out, errors, state, checkpoints)
}

pub(crate) fn book_directives_with_state(
  directives: &[Directive],
  config: &BookingConfig,
  state: BookingState,
) -> (Vec<Directive>, Vec<BookingError>, BookingState) {
  let (booked, errors, state, _) =
    book_directives_with_state_and_checkpoints(directives, config, state, 0, 0);
  (booked, errors, state)
}
