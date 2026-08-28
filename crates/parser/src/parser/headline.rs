use chumsky::prelude::*;

use crate::{Error, ast};

use super::common::{not_eol_parser, ws0_parser};

pub(super) fn headline_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  ws0_parser()
    .then(just('*'))
    // Limit headlines to a single line so following directives still parse.
    .then(not_eol_parser().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      let text = text.trim_start();
      ast::Directive::Headline(ast::Headline {
        span: ast::Span::from_range(span.start, span.end),
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      })
    })
}
