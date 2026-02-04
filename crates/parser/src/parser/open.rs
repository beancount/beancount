use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::utils::split_currencies;
use crate::{Error, ast};

use super::common::{
  bare_string_parser, date_parser, inline_comment_parser, key_value_block_parser,
  keyword_span_parser, quoted_string_parser, spanned_token_parser, ws0_parser, ws1_parser,
};

pub(super) fn open_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();
  let open_currency = bare_string_parser()
    .filter(|value| !value.content.starts_with('"') && !value.content.starts_with(';'));

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("open"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then(
      ws1_parser()
        .ignore_then(open_currency)
        .repeated()
        .collect::<Vec<_>>(),
    )
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .then_ignore(ws0_parser());

  let key_values_block = choice((
    key_value_block_parser(),
    ws1_parser().not().to(SmallVec::new()),
  ));

  header
    .then(inline_comment_parser().or_not())
    .then_ignore(super::common::line_end())
    .then(key_values_block)
    .map_with(|(header_and_comment, key_values), e| {
      let (header_parts, comment) = header_and_comment;
      let ((((date, keyword), account), currencies), opt_booking) = header_parts;
      let span = ast::Span::from_simple_span(e.span());
      let currencies = currencies.into_iter().flat_map(split_currencies).collect();

      ast::Directive::Open(ast::Open {
        span,
        keyword,
        date,
        account,
        currencies,
        opt_booking,
        comment,
        key_values,
      })
    })
}
