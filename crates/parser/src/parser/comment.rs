use chumsky::prelude::*;

use crate::{ast, Error};

use super::common::ws0_parser;

pub(super) fn comment_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  ws0_parser()
    .then(just(';'))
    .then(any().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      ast::Directive::Comment(ast::Comment {
        span: ast::Span::from_range(span.start, span.end),
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      })
    })
}
