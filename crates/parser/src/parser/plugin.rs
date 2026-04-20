use chumsky::prelude::*;

use crate::{Error, ast};

use super::common::{
  inline_comment_parser, keyword_span_parser, quoted_string_parser, ws0_parser, ws1_parser,
};

pub(super) fn plugin_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let header = keyword_span_parser("plugin")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .then_ignore(ws0_parser());

  header.then(inline_comment_parser().or_not()).map_with(
    |(((keyword, name), config), comment), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::Plugin(ast::Plugin {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        name,
        config,
        comment,
      })
    },
  )
}
