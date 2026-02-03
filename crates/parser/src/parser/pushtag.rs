use chumsky::prelude::*;

use crate::{ast, Error};

use super::common::{keyword_span_parser, ws0_parser, ws1_parser};

pub(super) fn pushtag_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let token = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice();

  let spanned_token = token.map_with(|value: &str, e| {
    let span: SimpleSpan = e.span();
    ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
  });

  let tag_token = just('#').or_not().ignore_then(spanned_token);

  keyword_span_parser("pushtag")
    .then_ignore(ws1_parser())
    .then(tag_token)
    .map_with(|(keyword, tag), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::PushTag(ast::TagDirective {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        tag,
        comment: None,
      })
    })
    .then_ignore(ws0_parser())
}
