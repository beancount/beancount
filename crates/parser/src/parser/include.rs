use chumsky::prelude::*;

use crate::{ast, Error};

use super::common::{keyword_span_parser, quoted_string_parser, ws0_parser, ws1_parser};

pub(super) fn include_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  keyword_span_parser("include")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .map_with(|(keyword, filename), e| {
      ast::Directive::Include(ast::Include {
        span: e.span().into(),
        keyword,
        filename,
        comment: None,
      })
    })
    .then_ignore(ws0_parser())
}
