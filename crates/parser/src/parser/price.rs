use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::{Error, ast};

use super::common::{
  date_parser, key_value_block_parser, keyword_span_parser, not_eol_parser,
  spanned_token_parser, ws0_parser, ws1_parser,
};
use super::number::number_literal_parser;

pub(super) fn price_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();

  let amount = number_literal_parser()
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
    .ignore_then(not_eol_parser().repeated().to_slice())
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast;
  use chumsky::{Parser, prelude::end};

  #[test]
  fn parses_price_with_spaced_negative_amount() {
    let src = "2026-02-06 price BTC - 227000 USD";

    let directive = price_directive_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let price = match directive {
      ast::Directive::Price(price) => price,
      other => panic!("expected price directive, got {other:?}"),
    };

    let number = match price.amount.number {
      ast::NumberExpr::Literal(ref literal) => literal,
      ref other => panic!("expected literal amount, got {other:?}"),
    };

    assert_eq!(price.currency.content, "BTC");
    assert_eq!(number.content, "- 227000");
    assert_eq!(
      price.amount.currency.as_ref().map(|c| c.content),
      Some("USD")
    );
  }
}
