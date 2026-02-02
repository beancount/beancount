#![allow(dead_code)]
#![allow(clippy::large_enum_variant)]
#![deny(clippy::unwrap_used, clippy::expect_used)]

pub mod ast;
pub mod core;
pub mod path_utils;

pub use core::{CoreDirective, normalize_directives};

/// Split a basic amount string (`"NUMBER CURRENCY"`) into its components.
pub fn parse_amount_tokens(raw: &str) -> Option<(&str, &str)> {
  let mut parts = raw.split_whitespace();
  let number = parts.next()?;
  let currency = parts.next()?;
  Some((number, currency))
}

use chumsky::prelude::*;
use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
  pub filename: String,
  pub line: usize,
  pub column: usize,
  pub message: String,
}

impl std::fmt::Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}:{}:{}: {}",
      self.filename, self.line, self.column, self.message
    )
  }
}

impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

type Error<'src> = extra::Err<Simple<'src, char>>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Line<'a> {
  span: ast::Span,
  content: &'a str,
  trimmed: &'a str,
  line_no: usize,
}

pub fn parse_str<'a>(source: &'a str, filename: &str) -> Result<Vec<ast::Directive<'a>>> {
  let lines = parse_lines(source);

  let mut directives = Vec::with_capacity(lines.len() / 20);

  let parsers = LineParsers {
    directives: LineDirectiveParsers {
      include: include_directive_parser(),
      plugin: plugin_directive_parser(),
      option: option_directive_parser(),
      pushtag: pushtag_directive_parser(),
      poptag: poptag_directive_parser(),
      pushmeta: pushmeta_directive_parser(),
      popmeta: popmeta_directive_parser(),
      comment: comment_directive_parser(),
      headline: headline_directive_parser(),
      open: open_directive_parser(),
      close: close_directive_parser(),
      balance: balance_directive_parser(),
      pad: pad_directive_parser(),
      commodity: commodity_directive_parser(),
      price: price_directive_parser(),
      event: event_directive_parser(),
      query: query_directive_parser(),
      note: note_directive_parser(),
      document: document_directive_parser(),
      custom: custom_directive_parser(),
    },
    posting: posting_parser(),
    key_value: key_value_parser(),
  };

  let mut index = 0;
  while index < lines.len() {
    let line = &lines[index];
    if line.trimmed.is_empty() {
      index += 1;
      continue;
    }

    // Fast path: directive kind is prefiltered in parse_line_directive().

    let mut is_txn_header = false;
    if !line.content.starts_with(' ') && !line.content.starts_with('\t') {
      let trimmed = line.content.trim_start();
      if let Some(first) = trimmed.chars().next()
        && first.is_ascii_digit()
      {
        let mut parts = trimmed.split_whitespace();
        let date = parts.next().unwrap_or("");
        let flag = parts.next().unwrap_or("");
        if looks_like_date(date) && (flag == "*" || flag == "!") {
          is_txn_header = true;
        }
      }
    }

    let line_meta = meta_from_line(filename, line);

    if is_txn_header && let Some(mut txn) = parse_transaction_header(line, filename, source) {
      let mut end_span = line.span.end;
      let mut tags_links_lines: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
      let mut comments: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
      let mut key_values: SmallVec<[ast::KeyValue<'a>; 4]> = SmallVec::new();
      let mut postings: SmallVec<[ast::Posting<'a>; 4]> = SmallVec::new();

      index += 1;
      while index < lines.len() {
        let next = &lines[index];
        if next.trimmed.is_empty() {
          break;
        }
        end_span = next.span.end;
        if next.trimmed.starts_with(';') {
          if let Some(comment) = trimmed_with_span(next) {
            comments.push(comment);
          }
          index += 1;
          continue;
        }
        let _posting_indent = next
          .content
          .chars()
          .take_while(|c| *c == ' ' || *c == '\t')
          .count();
        if let Some(kv) = parse_key_value_line(next, filename, source, &parsers.key_value) {
          if let Some(posting) = postings.last_mut() {
            posting.key_values.push(kv);
          } else {
            key_values.push(kv);
          }
          index += 1;
          continue;
        }
        if let Some(posting) = parse_posting_line_loose(next, filename, source, &parsers.posting) {
          postings.push(posting);
          index += 1;
          continue;
        }
        if let Some(tags_line) = tags_links_line(next) {
          tags_links_lines.push(tags_line);
          index += 1;
          continue;
        }
        break;
      }

      txn.postings = postings;
      txn.key_values = key_values;
      txn.comments = comments;

      let mut tags_links_lines = tags_links_lines;
      if let Some(inline) = txn.tags_links.clone() {
        tags_links_lines.insert(0, inline.clone());
      }
      let (tags, links) = parse_tags_links(tags_links_lines.clone());
      txn.tags = tags;
      txn.links = links;
      txn.tags_links_lines = tags_links_lines;
      txn.tags_links = txn
        .tags_links
        .clone()
        .or_else(|| txn.tags_links_lines.first().cloned());
      txn.comment = txn
        .comment
        .clone()
        .or_else(|| txn.comments.first().cloned());
      txn.meta = line_meta.clone();
      let txn_end = end_span + if end_span < source.len() { 1 } else { 0 };
      txn.span = ast::Span::from_range(line.span.start, txn_end);

      directives.push(ast::Directive::Transaction(txn));
      continue;
    }

    match parse_line_directive(line, &parsers.directives) {
      LineDirectiveOutcome::Parsed(mut directive) => {
        let mut end_span = line.span.end;
        let mut key_values: SmallVec<[ast::KeyValue<'a>; 4]> = SmallVec::new();
        let mut lookahead = index + 1;
        while lookahead < lines.len() {
          let next = &lines[lookahead];
          if next.trimmed.is_empty() {
            break;
          }
          if let Some(kv) = parse_key_value_line(next, filename, source, &parsers.key_value) {
            key_values.push(kv);
            end_span = next.span.end;
            lookahead += 1;
            continue;
          }
          break;
        }

        attach_key_values(&mut directive, key_values);
        rewrite_meta(line_meta.clone(), &mut directive);
        directive = expand_directive_span_to(directive, line.span.start, end_span, source);
        directives.push(directive);
        index = lookahead;
        continue;
      }
      LineDirectiveOutcome::Error => {
        let (raw, next_index) = raw_block_from(&lines, index, source, line_meta);
        directives.push(ast::Directive::Raw(raw));
        index = next_index;
        continue;
      }
      LineDirectiveOutcome::NotDirective => {}
    }
    let (raw, next_index) = raw_block_from(&lines, index, source, line_meta);
    directives.push(ast::Directive::Raw(raw));
    index = next_index;
  }

  Ok(directives)
}

fn parse_lines<'a>(source: &'a str) -> Vec<Line<'a>> {
  let mut lines = Vec::new();
  let mut offset = 0usize;
  let mut line_no = 1usize;

  for chunk in source.split_inclusive('\n') {
    let (raw_line, has_newline) = match chunk.strip_suffix('\n') {
      Some(rest) => (rest, true),
      None => (chunk, false),
    };
    let raw_line = raw_line.strip_suffix('\r').unwrap_or(raw_line);
    let trimmed = raw_line.trim();
    let start = offset;
    let end = start + raw_line.len();
    lines.push(Line {
      span: ast::Span::from_range(start, end),
      content: raw_line,
      trimmed,
      line_no,
    });
    offset += chunk.len();
    line_no += 1;
    if !has_newline {
      break;
    }
  }

  // Handle case where input is empty (no split_inclusive output)
  if source.is_empty() {
    lines.push(Line {
      span: ast::Span::from_range(0, 0),
      content: "",
      trimmed: "",
      line_no: 1,
    });
  }

  lines
}

fn raw_block_from<'a>(
  lines: &[Line<'a>],
  start_index: usize,
  source: &'a str,
  meta: ast::Meta,
) -> (ast::Raw<'a>, usize) {
  let line = &lines[start_index];
  let mut end = line.span.end;
  let mut index = start_index + 1;

  while index < lines.len() {
    let next = &lines[index];
    if next.trimmed.is_empty() {
      break;
    }
    if !next.content.starts_with(' ') && !next.content.starts_with('\t') {
      break;
    }
    end = next.span.end;
    index += 1;
  }

  let text = &source[line.span.start..end];
  let span = ast::Span::from_range(line.span.start, end);
  (ast::Raw { meta, span, text }, index)
}

fn parse_posting_line_loose<'a>(
  line: &Line<'a>,
  filename: &str,
  source: &'a str,
  parser: &impl Parser<'a, &'a str, ast::Posting<'a>, Error<'a>>,
) -> Option<ast::Posting<'a>> {
  if line.trimmed.is_empty() || line.trimmed.starts_with(';') {
    return None;
  }

  let parsed = parser.parse(line.content).into_result().ok()?;
  let mut posting = offset_posting(parsed, line.span.start);
  posting.meta = meta_from_line(filename, line);
  let posting_end = line.span.end + if line.span.end < source.len() { 1 } else { 0 };
  posting.span = ast::Span::from_range(line.span.start, posting_end);
  Some(posting)
}

fn parse_key_value_line<'a>(
  line: &Line<'a>,
  filename: &str,
  _source: &'a str,
  parser: &impl Parser<'a, &'a str, ast::KeyValue<'a>, Error<'a>>,
) -> Option<ast::KeyValue<'a>> {
  if !line.content.starts_with(' ') && !line.content.starts_with('\t') {
    return None;
  }
  if line.trimmed.is_empty() || line.trimmed.starts_with(';') {
    return None;
  }
  let parsed = parser.parse(line.content).into_result().ok()?;
  if !is_key_token(parsed.key.content) {
    return None;
  }
  let mut kv = offset_key_value(parsed, line.span.start);
  kv.meta = meta_from_line(filename, line);
  Some(kv)
}

fn is_key_token(raw: &str) -> bool {
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

fn tags_links_line<'a>(line: &Line<'a>) -> Option<ast::WithSpan<&'a str>> {
  let trimmed = line.trimmed;
  if trimmed.is_empty() {
    return None;
  }
  if trimmed.starts_with('#') || trimmed.starts_with('^') {
    return Some(ast::WithSpan::new(line.span, trimmed));
  }
  None
}

fn trimmed_with_span<'a>(line: &Line<'a>) -> Option<ast::WithSpan<&'a str>> {
  let trimmed = line.trimmed;
  if trimmed.is_empty() {
    return None;
  }
  let offset = line.content.find(trimmed).unwrap_or(0);
  let start = line.span.start + offset;
  let end = start + trimmed.len();
  Some(ast::WithSpan::new(
    ast::Span::from_range(start, end),
    trimmed,
  ))
}

fn parse_tags_links<'a>(
  groups: impl IntoIterator<Item = ast::WithSpan<&'a str>>,
) -> (
  SmallVec<[ast::WithSpan<&'a str>; 2]>,
  SmallVec<[ast::WithSpan<&'a str>; 2]>,
) {
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

  tags.sort_by(|a, b| a.content.cmp(b.content));
  tags.dedup_by(|a, b| a.content == b.content);
  links.sort_by(|a, b| a.content.cmp(b.content));
  links.dedup_by(|a, b| a.content == b.content);

  (tags, links)
}

fn meta_from_line(filename: &str, line: &Line<'_>) -> ast::Meta {
  ast::Meta {
    filename: filename.to_owned(),
    line: line.line_no,
    column: 1,
  }
}

fn expand_directive_span_to<'a>(
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

fn attach_key_values<'a>(
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

enum LineDirectiveOutcome<'a> {
  Parsed(ast::Directive<'a>),
  NotDirective,
  Error,
}

fn parse_line_with<'a, P>(parser: &P, line: &Line<'a>) -> LineDirectiveOutcome<'a>
where
  P: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
{
  match parser.parse(line.content).into_result() {
    Ok(parsed) => LineDirectiveOutcome::Parsed(offset_directive(parsed, line.span.start)),
    Err(_) => LineDirectiveOutcome::Error,
  }
}

fn parse_line_directive<
  'a,
  PInclude,
  PPlugin,
  POption,
  PPushtag,
  PPoptag,
  PPushmeta,
  PPopmeta,
  PComment,
  PHeadline,
  POpen,
  PClose,
  PBalance,
  PPad,
  PCommodity,
  PPrice,
  PEvent,
  PQuery,
  PNote,
  PDocument,
  PCustom,
>(
  line: &Line<'a>,
  parsers: &LineDirectiveParsers<
    PInclude,
    PPlugin,
    POption,
    PPushtag,
    PPoptag,
    PPushmeta,
    PPopmeta,
    PComment,
    PHeadline,
    POpen,
    PClose,
    PBalance,
    PPad,
    PCommodity,
    PPrice,
    PEvent,
    PQuery,
    PNote,
    PDocument,
    PCustom,
  >,
) -> LineDirectiveOutcome<'a>
where
  PInclude: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPlugin: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  POption: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPushtag: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPoptag: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPushmeta: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPopmeta: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PComment: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PHeadline: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  POpen: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PClose: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PBalance: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPad: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PCommodity: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PPrice: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PEvent: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PQuery: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PNote: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PDocument: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
  PCustom: Parser<'a, &'a str, ast::Directive<'a>, Error<'a>>,
{
  // Indented lines belong to postings or key-values and cannot start a directive.
  if line.content.starts_with(' ') || line.content.starts_with('\t') {
    return LineDirectiveOutcome::NotDirective;
  }

  let trimmed = line.content.trim_start();
  if trimmed.is_empty() {
    return LineDirectiveOutcome::NotDirective;
  }

  let Some(first) = trimmed.chars().next() else {
    return LineDirectiveOutcome::NotDirective;
  };
  let token = trimmed.split_whitespace().next().unwrap_or("");
  if first == ';' {
    return parse_line_with(&parsers.comment, line);
  }
  if first == '*' {
    return parse_line_with(&parsers.headline, line);
  }
  if first.is_ascii_digit() {
    if !looks_like_date(token) {
      return LineDirectiveOutcome::NotDirective;
    }
    let mut parts = trimmed.split_whitespace();
    let Some(_date) = parts.next() else {
      return LineDirectiveOutcome::Error;
    };
    let Some(second) = parts.next() else {
      return LineDirectiveOutcome::Error;
    };
    return match second {
      "open" => parse_line_with(&parsers.open, line),
      "close" => parse_line_with(&parsers.close, line),
      "balance" => parse_line_with(&parsers.balance, line),
      "pad" => parse_line_with(&parsers.pad, line),
      "commodity" => parse_line_with(&parsers.commodity, line),
      "price" => parse_line_with(&parsers.price, line),
      "event" => parse_line_with(&parsers.event, line),
      "query" => parse_line_with(&parsers.query, line),
      "note" => parse_line_with(&parsers.note, line),
      "document" => parse_line_with(&parsers.document, line),
      "custom" => parse_line_with(&parsers.custom, line),
      "*" | "!" => LineDirectiveOutcome::NotDirective,
      _ => LineDirectiveOutcome::NotDirective,
    };
  }
  if first.is_ascii_alphabetic() {
    return match token {
      "plugin" => parse_line_with(&parsers.plugin, line),
      "include" => parse_line_with(&parsers.include, line),
      "pushtag" => parse_line_with(&parsers.pushtag, line),
      "poptag" => parse_line_with(&parsers.poptag, line),
      "pushmeta" => parse_line_with(&parsers.pushmeta, line),
      "popmeta" => parse_line_with(&parsers.popmeta, line),
      "option" => parse_line_with(&parsers.option, line),
      _ => LineDirectiveOutcome::NotDirective,
    };
  }

  LineDirectiveOutcome::NotDirective
}

struct LineDirectiveParsers<
  PInclude,
  PPlugin,
  POption,
  PPushtag,
  PPoptag,
  PPushmeta,
  PPopmeta,
  PComment,
  PHeadline,
  POpen,
  PClose,
  PBalance,
  PPad,
  PCommodity,
  PPrice,
  PEvent,
  PQuery,
  PNote,
  PDocument,
  PCustom,
> {
  include: PInclude,
  plugin: PPlugin,
  option: POption,
  pushtag: PPushtag,
  poptag: PPoptag,
  pushmeta: PPushmeta,
  popmeta: PPopmeta,
  comment: PComment,
  headline: PHeadline,
  open: POpen,
  close: PClose,
  balance: PBalance,
  pad: PPad,
  commodity: PCommodity,
  price: PPrice,
  event: PEvent,
  query: PQuery,
  note: PNote,
  document: PDocument,
  custom: PCustom,
}

struct LineParsers<
  PInclude,
  PPlugin,
  POption,
  PPushtag,
  PPoptag,
  PPushmeta,
  PPopmeta,
  PComment,
  PHeadline,
  POpen,
  PClose,
  PBalance,
  PPad,
  PCommodity,
  PPrice,
  PEvent,
  PQuery,
  PNote,
  PDocument,
  PCustom,
  PPosting,
  PKeyValue,
> {
  directives: LineDirectiveParsers<
    PInclude,
    PPlugin,
    POption,
    PPushtag,
    PPoptag,
    PPushmeta,
    PPopmeta,
    PComment,
    PHeadline,
    POpen,
    PClose,
    PBalance,
    PPad,
    PCommodity,
    PPrice,
    PEvent,
    PQuery,
    PNote,
    PDocument,
    PCustom,
  >,
  posting: PPosting,
  key_value: PKeyValue,
}

fn keyword_span_parser<'src>(
  keyword: &'static str,
) -> impl Parser<'src, &'src str, ast::Span, Error<'src>> {
  just(keyword).map_with(|_, e| {
    let span: SimpleSpan = e.span();
    ast::Span::from_range(span.start, span.end)
  })
}

fn include_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  keyword_span_parser("include")
    .then_ignore(ws1)
    .then(quoted_string)
    .map(|(keyword, filename)| {
      ast::Directive::Include(ast::Include {
        meta: empty_meta(),
        span: filename.span,
        keyword,
        filename,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn plugin_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  keyword_span_parser("plugin")
    .then_ignore(ws1)
    .then(quoted_string)
    .then(ws1.ignore_then(quoted_string).or_not())
    .map(|((keyword, name), config)| {
      ast::Directive::Plugin(ast::Plugin {
        meta: empty_meta(),
        span: name.span,
        keyword,
        name,
        config,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn pushtag_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let token = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice();

  let spanned_token = token.map_with(|value: &str, e| {
    let span: SimpleSpan = e.span();
    ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
  });

  let tag_token = just('#').or_not().ignore_then(spanned_token);

  keyword_span_parser("pushtag")
    .then_ignore(ws1)
    .then(tag_token)
    .map(|(keyword, tag)| {
      ast::Directive::PushTag(ast::TagDirective {
        meta: empty_meta(),
        span: tag.span,
        keyword,
        tag,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn poptag_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let token = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice();

  let spanned_token = token.map_with(|value: &str, e| {
    let span: SimpleSpan = e.span();
    ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
  });

  let tag_token = just('#').or_not().ignore_then(spanned_token);

  keyword_span_parser("poptag")
    .then_ignore(ws1)
    .then(tag_token)
    .map(|(keyword, tag)| {
      ast::Directive::PopTag(ast::TagDirective {
        meta: empty_meta(),
        span: tag.span,
        keyword,
        tag,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn pushmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  let bare_string = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let pushmeta_key = any()
    .filter(|c: &char| !c.is_whitespace() && *c != ':')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let bool_value = bare_string.filter(|value| {
    value.content.eq_ignore_ascii_case("true") || value.content.eq_ignore_ascii_case("false")
  });

  let date_value = bare_string.filter(|value| looks_like_date(value.content));

  let pushmeta_value = choice((
    quoted_string.map(|value| value.map(ast::KeyValueValue::String)),
    bool_value
      .map(|value| value.map(|raw| ast::KeyValueValue::Bool(raw.eq_ignore_ascii_case("true")))),
    date_value.map(|value| value.map(ast::KeyValueValue::Date)),
    bare_string.map(|value| value.map(ast::KeyValueValue::UnquotedString)),
  ))
  .or_not();

  keyword_span_parser("pushmeta")
    .then_ignore(ws1)
    .then(pushmeta_key)
    .then_ignore(ws0)
    .then_ignore(just(':'))
    .then_ignore(ws0)
    .then(pushmeta_value)
    .map(|((keyword, key), value)| {
      ast::Directive::PushMeta(ast::PushMeta {
        meta: empty_meta(),
        span: key.span,
        keyword,
        key,
        value,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn popmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

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
    .then_ignore(ws1)
    .then(popmeta_key)
    .then_ignore(just(':').or_not())
    .map(|(keyword, key)| {
      ast::Directive::PopMeta(ast::PopMeta {
        meta: empty_meta(),
        span: key.span,
        keyword,
        key,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn option_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  keyword_span_parser("option")
    .then_ignore(ws1)
    .then(quoted_string)
    .then_ignore(ws1)
    .then(quoted_string)
    .map(|((keyword, key), value)| {
      ast::Directive::Option(ast::OptionDirective {
        meta: empty_meta(),
        span: key.span,
        keyword,
        key,
        value,
        comment: None,
      })
    })
    .then_ignore(ws0)
}

fn ws0_parser<'src>() -> impl Parser<'src, &'src str, (), Error<'src>> {
  any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored()
}

fn ws1_parser<'src>() -> impl Parser<'src, &'src str, (), Error<'src>> {
  any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored()
}

fn quoted_string_parser<'src>()
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

fn bare_string_parser<'src>() -> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>>
{
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

fn spanned_token_parser<'src>()
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

fn date_parser<'src>() -> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
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

fn rest_trimmed_parser<'src>() -> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>>
{
  any().repeated().to_slice().map_with(|value: &str, e| {
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

fn comment_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  ws0
    .then(just(';'))
    .then(any().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      ast::Directive::Comment(ast::Comment {
        meta: empty_meta(),
        span: ast::Span::from_range(span.start, span.end),
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      })
    })
}

fn headline_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  ws0
    .then(just('*'))
    .then(any().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      let text = text.trim_start();
      ast::Directive::Headline(ast::Headline {
        meta: empty_meta(),
        span: ast::Span::from_range(span.start, span.end),
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      })
    })
}

fn open_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();
  let quoted_string = quoted_string_parser();
  let bare_string = bare_string_parser();
  let open_currency = bare_string.filter(|value| !value.content.starts_with('"'));

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("open"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then(
      ws1_parser()
        .ignore_then(open_currency)
        .repeated()
        .collect::<Vec<_>>(),
    )
    .then(ws1_parser().ignore_then(quoted_string).or_not())
    .map(|((((date, keyword), account), currencies), opt_booking)| {
      let currencies = currencies.into_iter().flat_map(split_currencies).collect();
      ast::Directive::Open(ast::Open {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        account,
        currencies,
        opt_booking,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn close_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("close"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .map(|((date, keyword), account)| {
      ast::Directive::Close(ast::Close {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        account,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn balance_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();

  let amount = spanned_token_parser()
    .then(ws1_parser().ignore_then(spanned_token_parser()).or_not())
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw_span = ast::Span::from_range(span.start, span.end);
      let raw: &str = e.slice();
      ast::Amount {
        raw: ast::WithSpan::new(raw_span, raw.trim()),
        number: ast::NumberExpr::Literal(number),
        currency,
      }
    });

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("balance"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .map(|(((date, keyword), account), amount)| {
      ast::Directive::Balance(ast::Balance {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        account,
        amount,
        tolerance: None,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn pad_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("pad"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .map(|(((date, keyword), account), from_account)| {
      ast::Directive::Pad(ast::Pad {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        account,
        from_account,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn commodity_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("commodity"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .map(|((date, keyword), currency)| {
      ast::Directive::Commodity(ast::Commodity {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        currency,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn price_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();

  let amount = spanned_token_parser()
    .then(ws1_parser().ignore_then(spanned_token_parser()).or_not())
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw_span = ast::Span::from_range(span.start, span.end);
      let raw: &str = e.slice();
      ast::Amount {
        raw: ast::WithSpan::new(raw_span, raw.trim()),
        number: ast::NumberExpr::Literal(number),
        currency,
      }
    });

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("price"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .map(|(((date, keyword), currency), amount)| {
      ast::Directive::Price(ast::Price {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        currency,
        amount,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn event_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();
  let event_string = choice((quoted_string_parser(), spanned_token_parser()));
  let event_string_tail = choice((quoted_string_parser(), spanned_token_parser()));

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("event"))
    .then_ignore(ws1_parser())
    .then(event_string)
    .then_ignore(ws1_parser())
    .then(event_string_tail)
    .map(|(((date, keyword), event_type), desc)| {
      ast::Directive::Event(ast::Event {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        event_type,
        desc,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn query_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();
  let query_rest = rest_trimmed_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("query"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(query_rest)
    .map(|(((date, keyword), name), query)| {
      ast::Directive::Query(ast::Query {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        name,
        query,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn note_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();
  let note = rest_trimmed_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("note"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(note)
    .map(|(((date, keyword), account), note)| {
      ast::Directive::Note(ast::Note {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        account,
        note,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn document_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();
  let document_filename = choice((quoted_string_parser(), spanned_token_parser()));
  let header_rest = rest_trimmed_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("document"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(document_filename)
    .then(ws1_parser().ignore_then(header_rest).or_not())
    .map(|((((date, keyword), account), filename), tags_links)| {
      let (tags, links) = match tags_links.clone() {
        Some(value) => parse_tags_links([value]),
        None => (SmallVec::new(), SmallVec::new()),
      };
      ast::Directive::Document(ast::Document {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        account,
        filename,
        tags_links,
        tags,
        links,
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn custom_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("custom"))
    .then_ignore(ws1_parser())
    .then(choice((quoted_string_parser(), spanned_token_parser())))
    .then(
      ws1_parser()
        .ignore_then(custom_value_parser())
        .repeated()
        .collect::<Vec<_>>(),
    )
    .map(|(((date, keyword), name), values)| {
      ast::Directive::Custom(ast::Custom {
        meta: empty_meta(),
        span: date.span,
        keyword,
        date,
        name,
        values: values.into_iter().collect(),
        comment: None,
        key_values: SmallVec::new(),
      })
    })
    .then_ignore(ws0)
}

fn parse_transaction_header<'a>(
  line: &Line<'a>,
  _filename: &str,
  _source: &'a str,
) -> Option<ast::Transaction<'a>> {
  // Transactions must start at column 0; indented lines are postings or continuations.
  if line.content.starts_with(' ') || line.content.starts_with('\t') {
    return None;
  }

  let trimmed = line.trimmed;
  let mut parts = trimmed.split_whitespace();
  let date = parts.next()?;
  let flag = parts.next()?;
  if flag != "*" && flag != "!" {
    return None;
  }
  let after_flag = trimmed
    .find(flag)
    .map(|idx| &trimmed[idx + flag.len()..])
    .unwrap_or("");
  let remaining = after_flag;
  let (payee, narration, trailing) =
    if let Some((first_token, rest)) = parse_quoted_token(line, remaining) {
      if let Some((second_token, trailing)) = parse_quoted_token(line, rest) {
        (Some(first_token), Some(second_token), Some(trailing))
      } else {
        (None, Some(first_token), Some(rest))
      }
    } else {
      (None, None, Some(remaining))
    };

  let inline_tags_links = trailing.and_then(|rest| {
    let idx = rest.find(['#', '^'])?;
    let content = rest[idx..].trim_start();
    if content.is_empty() {
      return None;
    }
    let start = line
      .content
      .find(content)
      .map(|pos| line.span.start + pos)?;
    let end = start + content.len();
    Some(ast::WithSpan::new(
      ast::Span::from_range(start, end),
      content,
    ))
  });

  Some(ast::Transaction {
    meta: empty_meta(),
    span: line.span,
    date: span_for_token(line, date),
    txn: Some(span_for_token(line, flag)),
    payee,
    narration,
    tags_links: inline_tags_links,
    tags: SmallVec::new(),
    links: SmallVec::new(),
    comment: None,
    tags_links_lines: SmallVec::new(),
    comments: SmallVec::new(),
    key_values: SmallVec::new(),
    postings: SmallVec::new(),
  })
}

fn parse_quoted_token<'a>(
  line: &Line<'a>,
  remaining: &'a str,
) -> Option<(ast::WithSpan<&'a str>, &'a str)> {
  let s = remaining.trim_start();
  if !s.starts_with('"') {
    return None;
  }
  let end = s[1..].find('"')? + 2;
  let token = &s[..end];
  let base_ptr = line.content.as_ptr() as usize;
  let token_ptr = token.as_ptr() as usize;
  let start = if token_ptr >= base_ptr && token_ptr + token.len() <= base_ptr + line.content.len() {
    line.span.start + (token_ptr - base_ptr)
  } else {
    let offset = line.content.find(token).unwrap_or(0);
    line.span.start + offset
  };
  let span = ast::Span::from_range(start, start + token.len());
  Some((ast::WithSpan::new(span, token), &s[end..]))
}

fn looks_like_currency(raw: &str) -> bool {
  !raw.is_empty() && raw.chars().all(|c| c.is_ascii_uppercase())
}

fn looks_like_date(raw: &str) -> bool {
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

fn number_literal_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  let sign = just('+').or(just('-')).or_not();
  let digits = any()
    .filter(|c: &char| c.is_ascii_digit())
    .repeated()
    .at_least(1);
  let frac = just('.').then(digits).or_not();
  sign
    .then(digits)
    .then(frac)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    })
}

fn number_expr_parser<'src>() -> impl Parser<'src, &'src str, ast::NumberExpr<'src>, Error<'src>> {
  recursive(|_expr| {
    let literal = number_literal_parser()
      .map(ast::NumberExpr::Literal)
      .boxed();

    let op_mul = choice((
      just('*').to(ast::BinaryOp::Mul),
      just('/').to(ast::BinaryOp::Div),
    ))
    .map_with(|op, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), op)
    })
    .boxed();

    let op_add = choice((
      just('+').to(ast::BinaryOp::Add),
      just('-').to(ast::BinaryOp::Sub),
    ))
    .map_with(|op, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), op)
    })
    .boxed();

    let product = literal.clone().foldl(
      op_mul.then(literal.clone()).repeated(),
      |left, (op, right)| {
        let span = ast::Span::from_range(left.span().start, right.span().end);
        ast::NumberExpr::Binary {
          span,
          left: Box::new(left),
          op,
          right: Box::new(right),
        }
      },
    );

    product
      .clone()
      .foldl(op_add.then(product).repeated(), |left, (op, right)| {
        let span = ast::Span::from_range(left.span().start, right.span().end);
        ast::NumberExpr::Binary {
          span,
          left: Box::new(left),
          op,
          right: Box::new(right),
        }
      })
  })
}

fn custom_value_parser<'src>() -> impl Parser<'src, &'src str, ast::CustomValue<'src>, Error<'src>>
{
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  let bare_string = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let bool_value = bare_string.filter(|value| {
    value.content.eq_ignore_ascii_case("true") || value.content.eq_ignore_ascii_case("false")
  });

  let date_value = bare_string.filter(|value| looks_like_date(value.content));

  let currency_value = bare_string.filter(|value| looks_like_currency(value.content));

  let amount_value = number_literal_parser()
    .then_ignore(ws1)
    .then(currency_value)
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
      ast::CustomValue {
        raw: raw.clone(),
        kind: ast::CustomValueKind::Amount,
        number: None,
        amount: Some(ast::Amount {
          raw,
          number: ast::NumberExpr::Literal(number),
          currency: Some(currency),
        }),
      }
    });

  let number_expr_value = number_expr_parser().map_with(|expr, e| {
    let span: SimpleSpan = e.span();
    let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
    ast::CustomValue {
      raw,
      kind: ast::CustomValueKind::Number,
      number: Some(expr),
      amount: None,
    }
  });

  let string_value = quoted_string.map(|raw| ast::CustomValue {
    raw,
    kind: ast::CustomValueKind::String,
    number: None,
    amount: None,
  });

  let date_value = date_value.map(|raw| ast::CustomValue {
    raw,
    kind: ast::CustomValueKind::Date,
    number: None,
    amount: None,
  });

  let bool_value = bool_value.map(|raw| ast::CustomValue {
    raw: raw.clone(),
    kind: ast::CustomValueKind::Bool,
    number: None,
    amount: None,
  });

  let account_value = bare_string.map(|raw| ast::CustomValue {
    raw,
    kind: ast::CustomValueKind::Account,
    number: None,
    amount: None,
  });

  choice((
    amount_value,
    string_value,
    date_value,
    bool_value,
    number_expr_value,
    account_value,
  ))
}

fn key_value_parser<'src>() -> impl Parser<'src, &'src str, ast::KeyValue<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();

  let key = any()
    .filter(|c: &char| !c.is_whitespace() && *c != ':')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  let bare_string = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let bool_value = bare_string.filter(|value| {
    value.content.eq_ignore_ascii_case("true") || value.content.eq_ignore_ascii_case("false")
  });

  let date_value = bare_string.filter(|value| looks_like_date(value.content));

  let value = choice((
    quoted_string.map(|value| value.map(ast::KeyValueValue::String)),
    bool_value
      .map(|value| value.map(|raw| ast::KeyValueValue::Bool(raw.eq_ignore_ascii_case("true")))),
    date_value.map(|value| value.map(ast::KeyValueValue::Date)),
    bare_string.map(|value| value.map(ast::KeyValueValue::UnquotedString)),
  ));

  ws0
    .ignore_then(key)
    .then_ignore(ws0)
    .then_ignore(just(':'))
    .then_ignore(ws0)
    .then(value.or_not())
    .then_ignore(ws0)
    .map_with(|(key, value), e| {
      let span: SimpleSpan = e.span();
      ast::KeyValue {
        meta: empty_meta(),
        span: ast::Span::from_range(span.start, span.end),
        key,
        value,
      }
    })
}

fn posting_parser<'src>() -> impl Parser<'src, &'src str, ast::Posting<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let optflag = any()
    .filter(|c: &char| "*!#&?%PSTCURM".contains(*c))
    .repeated()
    .exactly(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let bare_string = any()
    .filter(|c: &char| !c.is_whitespace())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let account = bare_string.filter(|value| value.content.contains(':'));

  let currency = bare_string.filter(|value| looks_like_currency(value.content));

  let amount = number_expr_parser()
    .then_ignore(ws1)
    .then(currency)
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
      ast::Amount {
        raw,
        number,
        currency: Some(currency),
      }
    })
    .boxed();

  let price_annotation = choice((
    amount.clone(),
    currency.map_with(|currency, e| {
      let span: SimpleSpan = e.span();
      let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
      ast::Amount {
        raw: raw.clone(),
        number: ast::NumberExpr::Missing { span: raw.span },
        currency: Some(currency),
      }
    }),
  ));

  let price_operator = choice((
    just("@@").to(ast::PriceOperator::Total),
    just('@').to(ast::PriceOperator::PerUnit),
  ))
  .map_with(|op, e| {
    let span: SimpleSpan = e.span();
    ast::WithSpan::new(ast::Span::from_range(span.start, span.end), op)
  });

  let comment = just(';')
    .then(any().repeated().to_slice())
    .map_with(|_: (char, &str), e| {
      let span: SimpleSpan = e.span();
      let slice: &str = e.slice();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), slice)
    });

  ws0
    .ignore_then(optflag.or_not())
    .then(account)
    .then(ws1.ignore_then(amount).or_not())
    .then(ws0.ignore_then(cost_spec_parser()).or_not())
    .then(
      ws0
        .ignore_then(price_operator)
        .then_ignore(ws1)
        .then(price_annotation)
        .map(|(op, annotation)| (Some(op), Some(annotation)))
        .or_not()
        .map(|value| value.unwrap_or((None, None))),
    )
    .then(ws0.ignore_then(comment).or_not())
    .map_with(|value, e| {
      let span: SimpleSpan = e.span();
      let (left, comment) = value;
      let ((((opt_flag, account), amount), cost_spec), price) = left;
      let (price_operator, price_annotation) = price;
      ast::Posting {
        meta: empty_meta(),
        span: ast::Span::from_range(span.start, span.end),
        opt_flag,
        account,
        amount,
        cost_spec,
        price_operator,
        price_annotation,
        comment,
        key_values: SmallVec::new(),
      }
    })
}

fn cost_spec_parser<'src>() -> impl Parser<'src, &'src str, ast::CostSpec<'src>, Error<'src>> {
  let ws0 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .ignored();
  let ws1 = any()
    .filter(|c: &char| *c == ' ' || *c == '\t')
    .repeated()
    .at_least(1)
    .ignored();

  let currency = any()
    .filter(|c: &char| !c.is_whitespace() && *c != '}' && *c != ',' && *c != '#')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    })
    .filter(|value| looks_like_currency(value.content));

  let number_expr = number_expr_parser().boxed();

  let amount = number_expr
    .clone()
    .then_ignore(ws1)
    .then(currency)
    .map(|(per, currency)| ast::CostAmount {
      per: Some(per),
      total: None,
      currency: Some(currency),
    });

  let amount_total = number_expr
    .clone()
    .or_not()
    .then_ignore(ws0)
    .then_ignore(just('#'))
    .then_ignore(ws0)
    .then(number_expr.clone().or_not())
    .then_ignore(ws1)
    .then(currency)
    .map(|((per, total), currency)| ast::CostAmount {
      per,
      total,
      currency: Some(currency),
    });

  let cost_amount = choice((amount_total, amount));

  let date = any()
    .filter(|c: &char| c.is_ascii_digit())
    .repeated()
    .exactly(4)
    .then_ignore(just('-'))
    .then(
      any()
        .filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(2),
    )
    .then_ignore(just('-'))
    .then(
      any()
        .filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(2),
    )
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  #[derive(Clone)]
  enum CostComp<'a> {
    Amount(ast::CostAmount<'a>),
    Date(ast::WithSpan<&'a str>),
    Merge(ast::WithSpan<bool>),
    Ignored,
  }

  let quoted_string = just('"')
    .ignore_then(any().filter(|c: &char| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .ignored()
    .map(|_| CostComp::Ignored);

  let cost_comp = choice((
    cost_amount.map(CostComp::Amount),
    date.map(CostComp::Date),
    just('*').map_with(|_, e| {
      let span: SimpleSpan = e.span();
      CostComp::Merge(ast::WithSpan::new(
        ast::Span::from_range(span.start, span.end),
        true,
      ))
    }),
    quoted_string,
  ))
  .boxed();

  let comp_list = cost_comp
    .clone()
    .then(
      ws0
        .ignore_then(just(','))
        .ignore_then(ws0)
        .ignore_then(cost_comp)
        .repeated()
        .collect::<Vec<_>>(),
    )
    .map(|(head, tail): (CostComp<'src>, Vec<CostComp<'src>>)| {
      let mut comps = Vec::with_capacity(1 + tail.len());
      comps.push(head);
      comps.extend(tail);
      comps
    })
    .or_not();

  let single = just('{')
    .then(comp_list.clone())
    .then_ignore(just('}'))
    .map_with(|(_, comps), e| {
      let span: SimpleSpan = e.span();
      (comps, false, span, e.slice())
    });

  let double = just("{{")
    .then(comp_list)
    .then_ignore(just("}}"))
    .map_with(|(_, comps), e| {
      let span: SimpleSpan = e.span();
      (comps, true, span, e.slice())
    });

  choice((double, single)).map(|(comps, is_total, span, slice)| {
    let mut amount = None;
    let mut date = None;
    let label = None;
    let mut merge = None;
    if let Some(comp_list) = comps {
      for comp in comp_list {
        match comp {
          CostComp::Amount(value) if amount.is_none() => amount = Some(value),
          CostComp::Date(value) if date.is_none() => date = Some(value),
          CostComp::Merge(value) if merge.is_none() => merge = Some(value),
          CostComp::Ignored => {}
          _ => {}
        }
      }
    }

    let raw_span = ast::Span::from_range(span.start, span.end);
    ast::CostSpec {
      raw: ast::WithSpan::new(raw_span, slice),
      amount,
      date,
      label,
      merge,
      is_total: ast::WithSpan::new(raw_span, is_total),
    }
  })
}

fn empty_meta() -> ast::Meta {
  ast::Meta {
    filename: String::new(),
    line: 0,
    column: 0,
  }
}

fn rewrite_meta(meta: ast::Meta, directive: &mut ast::Directive<'_>) {
  match directive {
    ast::Directive::Open(val) => val.meta = meta,
    ast::Directive::Close(val) => val.meta = meta,
    ast::Directive::Balance(val) => val.meta = meta,
    ast::Directive::Pad(val) => val.meta = meta,
    ast::Directive::Transaction(val) => val.meta = meta,
    ast::Directive::Commodity(val) => val.meta = meta,
    ast::Directive::Price(val) => val.meta = meta,
    ast::Directive::Event(val) => val.meta = meta,
    ast::Directive::Query(val) => val.meta = meta,
    ast::Directive::Note(val) => val.meta = meta,
    ast::Directive::Document(val) => val.meta = meta,
    ast::Directive::Custom(val) => val.meta = meta,
    ast::Directive::Option(val) => val.meta = meta,
    ast::Directive::Include(val) => val.meta = meta,
    ast::Directive::Plugin(val) => val.meta = meta,
    ast::Directive::PushTag(val) => val.meta = meta,
    ast::Directive::PopTag(val) => val.meta = meta,
    ast::Directive::PushMeta(val) => val.meta = meta,
    ast::Directive::PopMeta(val) => val.meta = meta,
    ast::Directive::Comment(val) => val.meta = meta,
    ast::Directive::Headline(val) => val.meta = meta,
    ast::Directive::Raw(val) => val.meta = meta,
  }
}

fn split_currencies<'a>(
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

fn span_for_token<'a>(line: &Line<'a>, token: &'a str) -> ast::WithSpan<&'a str> {
  if token.is_empty() {
    return ast::WithSpan::new(line.span, token);
  }
  let base_ptr = line.content.as_ptr() as usize;
  let token_ptr = token.as_ptr() as usize;
  let start = if token_ptr >= base_ptr && token_ptr + token.len() <= base_ptr + line.content.len() {
    line.span.start + (token_ptr - base_ptr)
  } else {
    let rel = line.content.find(token).unwrap_or(0);
    line.span.start + rel
  };
  let end = start + token.len();
  ast::WithSpan::new(ast::Span::from_range(start, end), token)
}

fn offset_span(span: ast::Span, offset: usize) -> ast::Span {
  ast::Span::from_range(span.start + offset, span.end + offset)
}

fn offset_with_span<T>(value: ast::WithSpan<T>, offset: usize) -> ast::WithSpan<T> {
  ast::WithSpan::new(offset_span(value.span, offset), value.content)
}

fn offset_number_expr<'a>(expr: ast::NumberExpr<'a>, offset: usize) -> ast::NumberExpr<'a> {
  match expr {
    ast::NumberExpr::Missing { span } => ast::NumberExpr::Missing {
      span: offset_span(span, offset),
    },
    ast::NumberExpr::Literal(value) => ast::NumberExpr::Literal(offset_with_span(value, offset)),
    ast::NumberExpr::Binary {
      span,
      left,
      op,
      right,
    } => ast::NumberExpr::Binary {
      span: offset_span(span, offset),
      left: Box::new(offset_number_expr(*left, offset)),
      op: offset_with_span(op, offset),
      right: Box::new(offset_number_expr(*right, offset)),
    },
  }
}

fn offset_amount<'a>(amount: ast::Amount<'a>, offset: usize) -> ast::Amount<'a> {
  ast::Amount {
    raw: offset_with_span(amount.raw, offset),
    number: offset_number_expr(amount.number, offset),
    currency: amount.currency.map(|value| offset_with_span(value, offset)),
  }
}

fn offset_key_value<'a>(kv: ast::KeyValue<'a>, offset: usize) -> ast::KeyValue<'a> {
  ast::KeyValue {
    meta: kv.meta,
    span: offset_span(kv.span, offset),
    key: offset_with_span(kv.key, offset),
    value: kv.value.map(|value| offset_with_span(value, offset)),
  }
}

fn offset_custom_value<'a>(value: ast::CustomValue<'a>, offset: usize) -> ast::CustomValue<'a> {
  ast::CustomValue {
    raw: offset_with_span(value.raw, offset),
    kind: value.kind,
    number: value.number.map(|expr| offset_number_expr(expr, offset)),
    amount: value.amount.map(|amt| offset_amount(amt, offset)),
  }
}

fn offset_cost_amount<'a>(amount: ast::CostAmount<'a>, offset: usize) -> ast::CostAmount<'a> {
  ast::CostAmount {
    per: amount.per.map(|expr| offset_number_expr(expr, offset)),
    total: amount.total.map(|expr| offset_number_expr(expr, offset)),
    currency: amount.currency.map(|value| offset_with_span(value, offset)),
  }
}

fn offset_cost_spec<'a>(cost: ast::CostSpec<'a>, offset: usize) -> ast::CostSpec<'a> {
  ast::CostSpec {
    raw: offset_with_span(cost.raw, offset),
    amount: cost.amount.map(|value| offset_cost_amount(value, offset)),
    date: cost.date.map(|value| offset_with_span(value, offset)),
    label: cost.label.map(|value| offset_with_span(value, offset)),
    merge: cost.merge.map(|value| offset_with_span(value, offset)),
    is_total: offset_with_span(cost.is_total, offset),
  }
}

fn offset_posting<'a>(posting: ast::Posting<'a>, offset: usize) -> ast::Posting<'a> {
  ast::Posting {
    meta: posting.meta,
    span: offset_span(posting.span, offset),
    opt_flag: posting
      .opt_flag
      .map(|value| offset_with_span(value, offset)),
    account: offset_with_span(posting.account, offset),
    amount: posting.amount.map(|value| offset_amount(value, offset)),
    cost_spec: posting
      .cost_spec
      .map(|value| offset_cost_spec(value, offset)),
    price_operator: posting
      .price_operator
      .map(|value| offset_with_span(value, offset)),
    price_annotation: posting
      .price_annotation
      .map(|value| offset_amount(value, offset)),
    comment: posting.comment.map(|value| offset_with_span(value, offset)),
    key_values: posting
      .key_values
      .into_iter()
      .map(|value| offset_key_value(value, offset))
      .collect(),
  }
}

fn offset_directive<'a>(directive: ast::Directive<'a>, offset: usize) -> ast::Directive<'a> {
  match directive {
    ast::Directive::Open(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.account = offset_with_span(val.account, offset);
      val.currencies = val
        .currencies
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.opt_booking = val.opt_booking.map(|value| offset_with_span(value, offset));
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Open(val)
    }
    ast::Directive::Close(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.account = offset_with_span(val.account, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Close(val)
    }
    ast::Directive::Balance(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.account = offset_with_span(val.account, offset);
      val.amount = offset_amount(val.amount, offset);
      val.tolerance = val.tolerance.map(|value| offset_with_span(value, offset));
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Balance(val)
    }
    ast::Directive::Pad(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.account = offset_with_span(val.account, offset);
      val.from_account = offset_with_span(val.from_account, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Pad(val)
    }
    ast::Directive::Commodity(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.currency = offset_with_span(val.currency, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Commodity(val)
    }
    ast::Directive::Price(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.currency = offset_with_span(val.currency, offset);
      val.amount = offset_amount(val.amount, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Price(val)
    }
    ast::Directive::Event(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.event_type = offset_with_span(val.event_type, offset);
      val.desc = offset_with_span(val.desc, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Event(val)
    }
    ast::Directive::Query(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.name = offset_with_span(val.name, offset);
      val.query = offset_with_span(val.query, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Query(val)
    }
    ast::Directive::Note(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.account = offset_with_span(val.account, offset);
      val.note = offset_with_span(val.note, offset);
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Note(val)
    }
    ast::Directive::Document(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.account = offset_with_span(val.account, offset);
      val.filename = offset_with_span(val.filename, offset);
      val.tags_links = val.tags_links.map(|value| offset_with_span(value, offset));
      val.tags = val
        .tags
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.links = val
        .links
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Document(val)
    }
    ast::Directive::Custom(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.date = offset_with_span(val.date, offset);
      val.name = offset_with_span(val.name, offset);
      val.values = val
        .values
        .into_iter()
        .map(|value| offset_custom_value(value, offset))
        .collect();
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Custom(val)
    }
    ast::Directive::Option(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.key = offset_with_span(val.key, offset);
      val.value = offset_with_span(val.value, offset);
      ast::Directive::Option(val)
    }
    ast::Directive::Include(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.filename = offset_with_span(val.filename, offset);
      ast::Directive::Include(val)
    }
    ast::Directive::Plugin(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.name = offset_with_span(val.name, offset);
      val.config = val.config.map(|value| offset_with_span(value, offset));
      ast::Directive::Plugin(val)
    }
    ast::Directive::PushTag(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.tag = offset_with_span(val.tag, offset);
      ast::Directive::PushTag(val)
    }
    ast::Directive::PopTag(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.tag = offset_with_span(val.tag, offset);
      ast::Directive::PopTag(val)
    }
    ast::Directive::PushMeta(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.key = offset_with_span(val.key, offset);
      val.value = val.value.map(|value| offset_with_span(value, offset));
      ast::Directive::PushMeta(val)
    }
    ast::Directive::PopMeta(mut val) => {
      val.span = offset_span(val.span, offset);
      val.keyword = offset_span(val.keyword, offset);
      val.key = offset_with_span(val.key, offset);
      ast::Directive::PopMeta(val)
    }
    ast::Directive::Comment(mut val) => {
      val.span = offset_span(val.span, offset);
      val.text = offset_with_span(val.text, offset);
      ast::Directive::Comment(val)
    }
    ast::Directive::Headline(mut val) => {
      val.span = offset_span(val.span, offset);
      val.text = offset_with_span(val.text, offset);
      ast::Directive::Headline(val)
    }
    ast::Directive::Transaction(mut val) => {
      val.span = offset_span(val.span, offset);
      val.date = offset_with_span(val.date, offset);
      val.txn = val.txn.map(|value| offset_with_span(value, offset));
      val.payee = val.payee.map(|value| offset_with_span(value, offset));
      val.narration = val.narration.map(|value| offset_with_span(value, offset));
      val.tags_links = val.tags_links.map(|value| offset_with_span(value, offset));
      val.tags = val
        .tags
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.links = val
        .links
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.comment = val.comment.map(|value| offset_with_span(value, offset));
      val.tags_links_lines = val
        .tags_links_lines
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.comments = val
        .comments
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect();
      val.key_values = val
        .key_values
        .into_iter()
        .map(|value| offset_key_value(value, offset))
        .collect();
      ast::Directive::Transaction(val)
    }
    ast::Directive::Raw(mut val) => {
      val.span = offset_span(val.span, offset);
      ast::Directive::Raw(val)
    }
  }
}
