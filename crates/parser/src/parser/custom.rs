use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::{ast, Error};
use crate::utils::{looks_like_currency, looks_like_date};

use super::common::{bare_string_parser, date_parser, keyword_span_parser, key_value_block_parser, quoted_string_parser, spanned_token_parser, ws0_parser, ws1_parser};
use super::number::{number_expr_parser, number_literal_parser};

pub(super) fn custom_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("custom"))
    .then_ignore(ws1_parser())
    .then(choice((quoted_string_parser(), spanned_token_parser())))
    .then(
      ws1_parser()
        .ignore_then(custom_value_parser())
        .repeated()
        .collect::<Vec<_>>(),
    )
    .then_ignore(ws0_parser());

  header
    .then_ignore(super::common::line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), name), values), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Custom(ast::Custom {
        span,
        keyword,
        date,
        name,
        values: values.into_iter().collect(),
        comment: None,
        key_values,
      })
    })
}

fn custom_value_parser<'src>() -> impl Parser<'src, &'src str, ast::CustomValue<'src>, Error<'src>> {
  let bool_value = bare_string_parser().filter(|value| {
    value.content.eq_ignore_ascii_case("true") || value.content.eq_ignore_ascii_case("false")
  });

  let date_value = bare_string_parser().filter(|value| looks_like_date(value.content));

  let currency_value = bare_string_parser().filter(|value| looks_like_currency(value.content));

  let amount_value = number_literal_parser()
    .then_ignore(ws1_parser())
    .then(currency_value)
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
      ast::CustomValue {
        raw: raw.clone(),
        kind: ast::CustomValueKind::Amount,
        number: None,
        amount: Some(ast::Amount {
          raw,
          number: ast::NumberExpr::Literal(number),
          currency: Some(currency),
        }),
      }
    });

  let number_expr_value = number_expr_parser().map_with(|expr, e| {
    let span: SimpleSpan = e.span();
    let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
    ast::CustomValue {
      raw,
      kind: ast::CustomValueKind::Number,
      number: Some(expr),
      amount: None,
    }
  });

  let string_value = quoted_string_parser().map(|raw| ast::CustomValue {
    raw,
    kind: ast::CustomValueKind::String,
    number: None,
    amount: None,
  });

  let date_value = date_value.map(|raw| ast::CustomValue {
    raw,
    kind: ast::CustomValueKind::Date,
    number: None,
    amount: None,
  });

  let bool_value = bool_value.map(|raw| ast::CustomValue {
    raw: raw.clone(),
    kind: ast::CustomValueKind::Bool,
    number: None,
    amount: None,
  });

  let account_value = bare_string_parser().map(|raw| ast::CustomValue {
    raw,
    kind: ast::CustomValueKind::Account,
    number: None,
    amount: None,
  });

  choice((
    amount_value,
    string_value,
    date_value,
    bool_value,
    number_expr_value,
    account_value,
  ))
}
