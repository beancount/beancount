use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::utils::parse_tags_links;
use crate::{Error, ast};

use super::common::{
  date_parser, inline_comment_parser, key_value_block_parser, keyword_span_parser,
  quoted_string_parser, rest_trimmed_parser, spanned_token_parser, ws0_parser, ws1_parser,
};

pub(super) fn document_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();
  let document_filename = choice((quoted_string_parser(), spanned_token_parser()));
  let header_rest = rest_trimmed_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("document"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(document_filename)
    .then(ws1_parser().ignore_then(header_rest).or_not())
    .then_ignore(ws0_parser());

  header
    .then(inline_comment_parser().or_not())
    .then_ignore(super::common::line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |((((((date, keyword), account), filename), tags_links), comment), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let key_values = key_values.unwrap_or_else(SmallVec::new);
        let (tags, links) = match tags_links.clone() {
          Some(value) => parse_tags_links([value]),
          None => (SmallVec::new(), SmallVec::new()),
        };

        ast::Directive::Document(ast::Document {
          span,
          keyword,
          date,
          account,
          filename,
          tags_links,
          tags,
          links,
          comment,
          key_values,
        })
      },
    )
}
