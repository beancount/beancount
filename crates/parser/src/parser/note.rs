use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::{Error, ast};

use super::common::{
  date_parser, inline_comment_parser, key_value_block_parser, keyword_span_parser,
  spanned_token_parser, ws0_parser, ws1_parser,
};

pub(super) fn note_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();
  let note = any()
    .filter(|c: &char| *c != '\n' && *c != ';')
    .repeated()
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      let start = span.start;
      let trimmed = value.trim();
      if trimmed.is_empty() {
        return ast::WithSpan::new(ast::Span::from_range(start, start), trimmed);
      }
      let offset = value.find(trimmed).unwrap_or(0);
      let span = ast::Span::from_range(start + offset, start + offset + trimmed.len());
      ast::WithSpan::new(span, trimmed)
    });

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("note"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(note)
    .then_ignore(ws0_parser());

  header
    .then(inline_comment_parser().or_not())
    .then_ignore(super::common::line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |(((((date, keyword), account), note), comment), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let key_values = key_values.unwrap_or_else(SmallVec::new);

        ast::Directive::Note(ast::Note {
          span,
          keyword,
          date,
          account,
          note,
          comment,
          key_values,
        })
      },
    )
}
