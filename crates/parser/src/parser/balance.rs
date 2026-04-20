use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::parser::common;
use crate::utils::looks_like_currency;
use crate::{Error, ast};

use super::common::{
  inline_comment_parser, key_value_block_parser, spanned_token_parser, ws0_parser,
  ws1_parser,
};
use super::number::{number_expr_parser, number_literal_parser};

pub(super) fn balance_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = super::common::date_parser();

  let currency =
    || spanned_token_parser().filter(|value| looks_like_currency(value.content));

  let currency_after = || ws1_parser().ignore_then(currency());

  let tolerance = || {
    ws0_parser()
      .ignore_then(just('~'))
      .ignore_then(ws0_parser())
      .ignore_then(number_literal_parser())
  };

  let amount = number_expr_parser()
    .then(choice((
      tolerance()
        .then(currency_after())
        .map(|(tolerance, currency)| (Some(tolerance), currency)),
      currency_after()
        .then(
          tolerance()
            .then(currency_after().or_not())
            .map(|(tolerance, _)| tolerance)
            .or_not(),
        )
        .map(|(currency, tolerance)| (tolerance, currency)),
    )))
    .map_with(|(number, (tolerance, currency)), e| {
      let span: SimpleSpan = e.span();
      let raw_span = ast::Span::from_range(span.start, span.end);
      let raw: &str = e.slice();
      let trimmed = raw.trim();
      let offset = raw.find(trimmed).unwrap_or(0);
      let raw_span = ast::Span::from_range(
        raw_span.start + offset,
        raw_span.start + offset + trimmed.len(),
      );
      (
        ast::Amount {
          raw: ast::WithSpan::new(raw_span, trimmed),
          number,
          currency: Some(currency),
        },
        tolerance,
      )
    });

  let header = date
    .then_ignore(ws1_parser())
    .then(common::keyword_span_parser("balance"))
    .then_ignore(ws1_parser())
    .then(common::account_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .then_ignore(ws0_parser());

  header
    .then(inline_comment_parser().or_not())
    .then_ignore(super::common::line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|(header_and_comment, key_values), e| {
      let (header_parts, comment) = header_and_comment;
      let (((date, keyword), account), (amount, tolerance)) = header_parts;
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Balance(ast::Balance {
        span,
        keyword,
        date,
        account,
        amount,
        tolerance,
        comment,
        key_values,
      })
    })
}

#[cfg(test)]
mod tests {
  use super::*;
  use chumsky::Parser;

  #[test]
  fn parses_balance_with_expression_amount() {
    let src = "2026-01-25 balance Assets:Cash 50139.85-600000 CNY";

    let directive = balance_directive_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .expect("should parse balance");

    let balance = match directive {
      ast::Directive::Balance(val) => val,
      other => panic!("expected balance, got {other:?}"),
    };

    assert_eq!(balance.account.content, "Assets:Cash");
    assert_eq!(balance.amount.currency.unwrap().content, "CNY");

    let number = balance.amount.number;
    match number {
      ast::NumberExpr::Binary {
        left, op, right, ..
      } => {
        assert_eq!(op.content, ast::BinaryOp::Sub);
        let left_val = match *left {
          ast::NumberExpr::Literal(ref lit) => lit.content,
          _ => panic!("expected left literal"),
        };
        let right_val = match *right {
          ast::NumberExpr::Literal(ref lit) => lit.content,
          _ => panic!("expected right literal"),
        };
        assert_eq!(left_val, "50139.85");
        assert_eq!(right_val, "600000");
      }
      _ => panic!("expected binary number expression"),
    }

    assert!(balance.tolerance.is_none());
    assert!(balance.comment.is_none());
  }
}
