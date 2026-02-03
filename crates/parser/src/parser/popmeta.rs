use chumsky::prelude::*;

use crate::{ast, Error};

use super::common::{keyword_span_parser, ws0_parser, ws1_parser};

pub(super) fn popmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let popmeta_key = any()
    .filter(|c: &char| !c.is_whitespace() && *c != ':')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  keyword_span_parser("popmeta")
    .then_ignore(ws1_parser())
    .then(popmeta_key)
    .then_ignore(just(':').or_not())
    .map_with(|(keyword, key), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::PopMeta(ast::PopMeta {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        key,
        comment: None,
      })
    })
    .then_ignore(ws0_parser())
}
