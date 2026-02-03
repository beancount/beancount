use chumsky::prelude::*;

use crate::{ast, Error};
use crate::utils::looks_like_date;

use super::common::{bare_string_parser, keyword_span_parser, quoted_string_parser, ws0_parser, ws1_parser};

pub(super) fn pushmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let pushmeta_key = any()
    .filter(|c: &char| !c.is_whitespace() && *c != ':')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let bool_value = bare_string_parser().filter(|value| {
    value.content.eq_ignore_ascii_case("true") || value.content.eq_ignore_ascii_case("false")
  });

  let date_value = bare_string_parser().filter(|value| looks_like_date(value.content));

  let pushmeta_value = choice((
    quoted_string_parser().map(|value| value.map(ast::KeyValueValue::String)),
    bool_value
      .map(|value| value.map(|raw| ast::KeyValueValue::Bool(raw.eq_ignore_ascii_case("true")))),
    date_value.map(|value| value.map(ast::KeyValueValue::Date)),
    bare_string_parser().map(|value| value.map(ast::KeyValueValue::UnquotedString)),
  ))
  .or_not();

  keyword_span_parser("pushmeta")
    .then_ignore(ws1_parser())
    .then(pushmeta_key)
    .then_ignore(ws0_parser())
    .then_ignore(just(':'))
    .then_ignore(ws0_parser())
    .then(pushmeta_value)
    .map_with(|((keyword, key), value), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::PushMeta(ast::PushMeta {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        key,
        value,
        comment: None,
      })
    })
    .then_ignore(ws0_parser())
}
