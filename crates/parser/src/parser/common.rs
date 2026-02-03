use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::Error;
use crate::ast;
use crate::utils::{looks_like_currency, looks_like_date};

pub(super) fn ws0_parser<'src>() -> impl Parser<'src, &'src str, (), Error<'src>> {
  choice((just(' '), just('\t'))).repeated().ignored()
}

pub(super) fn ws1_parser<'src>() -> impl Parser<'src, &'src str, (), Error<'src>> {
  choice((just(' '), just('\t')))
    .repeated()
    .at_least(1)
    .ignored()
}

pub(super) fn line_end<'src>() -> impl Parser<'src, &'src str, (), Error<'src>> {
  choice((just('\n').ignored(), end()))
}

pub(super) fn keyword_span_parser<'src>(
  keyword: &'static str,
) -> impl Parser<'src, &'src str, ast::Span, Error<'src>> {
  just(keyword).map_with(|_, e| {
    let span: SimpleSpan = e.span();
    ast::Span::from_range(span.start, span.end)
  })
}

pub(super) fn quoted_string_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    })
}

pub(super) fn bare_string_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    })
}

pub(super) fn spanned_token_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    })
}

pub(super) fn date_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  let digit = any().filter(|c: &char| c.is_ascii_digit());
  digit
    .repeated()
    .exactly(4)
    .then_ignore(just('-'))
    .then(digit.repeated().exactly(2))
    .then_ignore(just('-'))
    .then(digit.repeated().exactly(2))
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    })
}

pub(super) fn rest_trimmed_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  any()
    .filter(|c: &char| *c != '\n' && *c != ';')
    .repeated()
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      let start = span.start;
      let trimmed = value.trim();
      if trimmed.is_empty() {
        return ast::WithSpan::new(ast::Span::from_range(start, start), trimmed);
      }
      let offset = value.find(trimmed).unwrap_or(0);
      let span = ast::Span::from_range(start + offset, start + offset + trimmed.len());
      ast::WithSpan::new(span, trimmed)
    })
}

pub(super) fn tags_links_line_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  any()
    .filter(|c: &char| *c != '\n')
    .repeated()
    .at_least(1)
    .to_slice()
    .filter(|line: &&str| {
      let trimmed = line.trim();
      !trimmed.is_empty() && (trimmed.starts_with('#') || trimmed.starts_with('^'))
    })
    .map_with(|line, e| {
      let trimmed = line.trim();
      let span: SimpleSpan = e.span();
      let offset = line.find(trimmed).unwrap_or(0);
      let start = span.start + offset;
      let end = start + trimmed.len();
      ast::WithSpan::new(ast::Span::from_range(start, end), trimmed)
    })
}

pub(super) fn inline_comment_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  ws0_parser()
    .ignore_then(just(';'))
    .ignore_then(any().filter(|c: &char| *c != '\n').repeated().to_slice())
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text)
    })
}

pub(super) fn key_value_parser<'src>()
-> impl Parser<'src, &'src str, ast::KeyValue<'src>, Error<'src>> {
  let key = any()
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

  let value = choice((
    quoted_string_parser().map(|value| value.map(ast::KeyValueValue::String)),
    bool_value
      .map(|value| value.map(|raw| ast::KeyValueValue::Bool(raw.eq_ignore_ascii_case("true")))),
    date_value.map(|value| value.map(ast::KeyValueValue::Date)),
    bare_string_parser().map(|value| value.map(ast::KeyValueValue::UnquotedString)),
  ));

  ws0_parser()
    .ignore_then(key)
    .then_ignore(ws0_parser())
    .then_ignore(just(':'))
    .then_ignore(ws0_parser())
    .then(value.or_not())
    .then_ignore(ws0_parser())
    .map_with(|(key, value), e| {
      let span: SimpleSpan = e.span();
      ast::KeyValue {
        span: ast::Span::from_range(span.start, span.end),
        key,
        value,
      }
    })
}

pub(super) fn indented_key_value_parser<'src>()
-> impl Parser<'src, &'src str, ast::KeyValue<'src>, Error<'src>> {
  let ws1 = ws1_parser();
  ws1.ignore_then(key_value_parser().filter(|kv| is_key_token(kv.key.content)))
}

pub(super) fn key_value_block_parser<'src>()
-> impl Parser<'src, &'src str, SmallVec<[ast::KeyValue<'src>; 4]>, Error<'src>> + 'src {
  indented_key_value_parser()
    .then_ignore(line_end())
    .map_with(move |mut kv, e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      kv.span = span;
      kv
    })
    .repeated()
    .collect::<Vec<_>>()
    .map(SmallVec::from_vec)
}

pub(super) fn is_key_token(raw: &str) -> bool {
  let mut chars = raw.chars();
  let first = match chars.next() {
    Some(val) => val,
    None => return false,
  };
  if !first.is_ascii_lowercase() {
    return false;
  }
  chars.all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

pub(super) fn currency_token_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  bare_string_parser().filter(|value| looks_like_currency(value.content))
}
