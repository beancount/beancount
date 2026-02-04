use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::utils::looks_like_currency;
use crate::{Error, ast};

use super::common::{
  inline_comment_parser, key_value_block_parser, spanned_token_parser, ws0_parser, ws1_parser,
};
use super::number::number_literal_parser;

pub(super) fn balance_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = super::common::date_parser();

  let currency = || spanned_token_parser().filter(|value| looks_like_currency(value.content));

  let currency_after = || ws1_parser().ignore_then(currency());

  let tolerance = || {
    ws0_parser()
      .ignore_then(just('~'))
      .ignore_then(ws0_parser())
      .ignore_then(number_literal_parser())
  };

  let amount = number_literal_parser()
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
          number: ast::NumberExpr::Literal(number),
          currency: Some(currency),
        },
        tolerance,
      )
    });

  let header = date
    .then_ignore(ws1_parser())
    .then(super::common::keyword_span_parser("balance"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
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
    .boxed()
}
