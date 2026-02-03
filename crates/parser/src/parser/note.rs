use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::{ast, Error};

use super::common::{date_parser, keyword_span_parser, key_value_block_parser, rest_trimmed_parser, spanned_token_parser, ws0_parser, ws1_parser};

pub(super) fn note_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();
  let note = rest_trimmed_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("note"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(note)
    .then_ignore(ws0_parser());

  header
    .then_ignore(super::common::line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), account), note), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Note(ast::Note {
        span,
        keyword,
        date,
        account,
        note,
        comment: None,
        key_values,
      })
    })
}
