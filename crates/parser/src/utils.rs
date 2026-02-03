use smallvec::SmallVec;

use crate::ast;

pub(crate) fn looks_like_currency(raw: &str) -> bool {
  fn is_inner_char(c: char) -> bool {
    c.is_ascii_uppercase() || c.is_ascii_digit() || matches!(c, '\'' | '.' | '_' | '-')
  }

  let mut chars = raw.chars();
  let Some(first) = chars.next() else {
    return false;
  };

  // Leading slash currencies are allowed but must contain an uppercase letter and end with
  // an alphanumeric character (aligns with the Python CURRENCY_RE).
  if first == '/' {
    let mut saw_letter = false;
    let mut last = first;
    for ch in chars {
      if ch.is_ascii_uppercase() {
        saw_letter = true;
      }
      if !is_inner_char(ch) {
        return false;
      }
      last = ch;
    }
    return saw_letter && (last.is_ascii_uppercase() || last.is_ascii_digit());
  }

  if !first.is_ascii_uppercase() {
    return false;
  }

  let mut last = first;
  for ch in chars {
    if !is_inner_char(ch) {
      return false;
    }
    last = ch;
  }

  last.is_ascii_uppercase() || last.is_ascii_digit()
}

pub(crate) fn looks_like_date(raw: &str) -> bool {
  let bytes = raw.as_bytes();
  bytes.len() >= 10
    && bytes.get(4) == Some(&b'-')
    && bytes.get(7) == Some(&b'-')
    && bytes.first().is_some_and(u8::is_ascii_digit)
    && bytes.get(1).is_some_and(u8::is_ascii_digit)
    && bytes.get(2).is_some_and(u8::is_ascii_digit)
    && bytes.get(3).is_some_and(u8::is_ascii_digit)
    && bytes.get(5).is_some_and(u8::is_ascii_digit)
    && bytes.get(6).is_some_and(u8::is_ascii_digit)
    && bytes.get(8).is_some_and(u8::is_ascii_digit)
    && bytes.get(9).is_some_and(u8::is_ascii_digit)
}

pub(crate) fn split_currencies<'a>(
  raw: ast::WithSpan<&'a str>,
) -> impl Iterator<Item = ast::WithSpan<&'a str>> + 'a {
  raw
    .content
    .split(',')
    .filter(|token| !token.trim().is_empty())
    .map(move |token| {
      let trimmed = token.trim();
      ast::WithSpan::new(raw.span, trimmed)
    })
}

type TagLinkBuckets<'a> = (
  SmallVec<[ast::WithSpan<&'a str>; 2]>,
  SmallVec<[ast::WithSpan<&'a str>; 2]>,
);

pub(crate) fn split_tags_links_group(group: ast::WithSpan<&str>) -> Vec<ast::WithSpan<&str>> {
  let mut tokens = Vec::new();
  let mut start: Option<usize> = None;

  for (idx, ch) in group.content.char_indices() {
    if ch.is_whitespace() {
      if let Some(s) = start.take() {
        let end = idx;
        let span = ast::Span::from_range(group.span.start + s, group.span.start + end);
        tokens.push(ast::WithSpan::new(span, &group.content[s..end]));
      }
    } else if start.is_none() {
      start = Some(idx);
    }
  }

  if let Some(s) = start {
    let end = group.content.len();
    let span = ast::Span::from_range(group.span.start + s, group.span.start + end);
    tokens.push(ast::WithSpan::new(span, &group.content[s..end]));
  }

  tokens
}

pub(crate) fn parse_tags_links<'a>(
  groups: impl IntoIterator<Item = ast::WithSpan<&'a str>>,
) -> TagLinkBuckets<'a> {
  let mut tags: SmallVec<[ast::WithSpan<&'a str>; 2]> = SmallVec::new();
  let mut links: SmallVec<[ast::WithSpan<&'a str>; 2]> = SmallVec::new();

  for group in groups {
    let base_ptr = group.content.as_ptr() as usize;
    let group_start = group.span.start;
    for token in group.content.split_whitespace() {
      let token_ptr = token.as_ptr() as usize;
      let offset = token_ptr.saturating_sub(base_ptr);
      let token_start = group_start + offset;
      if let Some(tag) = token.strip_prefix('#') {
        let start = token_start + 1;
        let end = start + tag.len();
        tags.push(ast::WithSpan::new(ast::Span::from_range(start, end), tag));
      } else if let Some(link) = token.strip_prefix('^') {
        let start = token_start + 1;
        let end = start + link.len();
        links.push(ast::WithSpan::new(ast::Span::from_range(start, end), link));
      }
    }
  }

  (tags, links)
}

pub(crate) fn attach_key_values<'a>(
  directive: &mut ast::Directive<'a>,
  key_values: SmallVec<[ast::KeyValue<'a>; 4]>,
) {
  if key_values.is_empty() {
    return;
  }
  match directive {
    ast::Directive::Open(val) => val.key_values = key_values,
    ast::Directive::Close(val) => val.key_values = key_values,
    ast::Directive::Balance(val) => val.key_values = key_values,
    ast::Directive::Pad(val) => val.key_values = key_values,
    ast::Directive::Commodity(val) => val.key_values = key_values,
    ast::Directive::Price(val) => val.key_values = key_values,
    ast::Directive::Event(val) => val.key_values = key_values,
    ast::Directive::Query(val) => val.key_values = key_values,
    ast::Directive::Note(val) => val.key_values = key_values,
    ast::Directive::Document(val) => val.key_values = key_values,
    ast::Directive::Custom(val) => val.key_values = key_values,
    _ => {}
  }
}

pub(crate) fn expand_directive_span_to<'a>(
  directive: ast::Directive<'a>,
  start: usize,
  end_span: usize,
  source: &'a str,
) -> ast::Directive<'a> {
  let end = end_span + if end_span < source.len() { 1 } else { 0 };
  let span = ast::Span::from_range(start, end);
  match directive {
    ast::Directive::Open(mut val) => {
      val.span = span;
      ast::Directive::Open(val)
    }
    ast::Directive::Close(mut val) => {
      val.span = span;
      ast::Directive::Close(val)
    }
    ast::Directive::Balance(mut val) => {
      val.span = span;
      ast::Directive::Balance(val)
    }
    ast::Directive::Pad(mut val) => {
      val.span = span;
      ast::Directive::Pad(val)
    }
    ast::Directive::Commodity(mut val) => {
      val.span = span;
      ast::Directive::Commodity(val)
    }
    ast::Directive::Price(mut val) => {
      val.span = span;
      ast::Directive::Price(val)
    }
    ast::Directive::Event(mut val) => {
      val.span = span;
      ast::Directive::Event(val)
    }
    ast::Directive::Query(mut val) => {
      val.span = span;
      ast::Directive::Query(val)
    }
    ast::Directive::Note(mut val) => {
      val.span = span;
      ast::Directive::Note(val)
    }
    ast::Directive::Document(mut val) => {
      val.span = span;
      ast::Directive::Document(val)
    }
    ast::Directive::Custom(mut val) => {
      val.span = span;
      ast::Directive::Custom(val)
    }
    ast::Directive::Option(mut val) => {
      val.span = span;
      ast::Directive::Option(val)
    }
    ast::Directive::Include(mut val) => {
      val.span = span;
      ast::Directive::Include(val)
    }
    ast::Directive::Plugin(mut val) => {
      val.span = span;
      ast::Directive::Plugin(val)
    }
    ast::Directive::PushTag(mut val) => {
      val.span = span;
      ast::Directive::PushTag(val)
    }
    ast::Directive::PopTag(mut val) => {
      val.span = span;
      ast::Directive::PopTag(val)
    }
    ast::Directive::PushMeta(mut val) => {
      val.span = span;
      ast::Directive::PushMeta(val)
    }
    ast::Directive::PopMeta(mut val) => {
      val.span = span;
      ast::Directive::PopMeta(val)
    }
    ast::Directive::Comment(mut val) => {
      val.span = span;
      ast::Directive::Comment(val)
    }
    ast::Directive::Headline(mut val) => {
      val.span = span;
      ast::Directive::Headline(val)
    }
    ast::Directive::Transaction(mut val) => {
      val.span = span;
      ast::Directive::Transaction(val)
    }
    ast::Directive::Raw(mut val) => {
      val.span = span;
      ast::Directive::Raw(val)
    }
  }
}

pub(crate) fn empty_meta() -> ast::Meta {
  ast::Meta {
    filename: std::sync::Arc::new(String::new()),
    line: 0,
    column: 0,
  }
}
