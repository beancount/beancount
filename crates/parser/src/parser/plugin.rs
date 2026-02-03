use chumsky::prelude::*;

use crate::{ast, Error};

use super::common::{keyword_span_parser, quoted_string_parser, ws0_parser, ws1_parser};

pub(super) fn plugin_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  keyword_span_parser("plugin")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .map_with(|((keyword, name), config), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::Plugin(ast::Plugin {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        name,
        config,
        comment: None,
      })
    })
    .then_ignore(ws0_parser())
}
