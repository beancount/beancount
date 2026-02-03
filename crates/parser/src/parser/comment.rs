use chumsky::prelude::*;

use crate::{Error, ast};

use super::common::ws0_parser;

pub(super) fn comment_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let semicolon_comment = ws0_parser()
    .ignore_then(just(';'))
    .ignore_then(any().filter(|c: &char| *c != '\n').repeated())
    .to_slice();

  let hash_comment = just('#')
    .ignore_then(any().filter(|c: &char| *c != '\n').repeated())
    .to_slice();

  choice((semicolon_comment, hash_comment)).map_with(|text: &str, e| {
    let span: SimpleSpan = e.span();
    ast::Directive::Comment(ast::Comment {
      span: ast::Span::from_range(span.start, span.end),
      text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
    })
  })
}
