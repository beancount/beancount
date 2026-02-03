use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::{Error, ast};

use super::common::{
  date_parser, key_value_block_parser, keyword_span_parser, spanned_token_parser, ws0_parser,
  ws1_parser,
};

pub(super) fn price_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();

  let amount = spanned_token_parser()
    .then(ws1_parser().ignore_then(spanned_token_parser()).or_not())
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw_span = ast::Span::from_range(span.start, span.end);
      let raw: &str = e.slice();
      ast::Amount {
        raw: ast::WithSpan::new(raw_span, raw.trim()),
        number: ast::NumberExpr::Literal(number),
        currency,
      }
    });

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("price"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .then_ignore(ws0_parser());

  let comment = ws0_parser()
    .ignore_then(just(';'))
    .ignore_then(any().filter(|c: &char| *c != '\n').repeated().to_slice())
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text)
    })
    .or_not();

  header
    .then(comment)
    .then_ignore(super::common::line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |(((((date, keyword), currency), amount), comment), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let key_values = key_values.unwrap_or_else(SmallVec::new);

        ast::Directive::Price(ast::Price {
          span,
          keyword,
          date,
          currency,
          amount,
          comment,
          key_values,
        })
      },
    )
}
