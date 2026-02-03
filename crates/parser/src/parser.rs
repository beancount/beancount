//! 递归下降的行扫描解析器（合并 parser.rs 与 parser1.rs）。
//! 按行扫描，子解析器产出 `DirectiveKind`，再拼装为最终 `ast::Directive`。

use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::utils::{
  attach_key_values, empty_meta, expand_directive_span_to, looks_like_currency, looks_like_date,
  parse_tags_links, split_currencies,
};
use crate::{Error, Result, ast};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DirectiveKind<'a> {
  Include {
    keyword: ast::Span,
    filename: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Plugin {
    keyword: ast::Span,
    name: ast::WithSpan<&'a str>,
    config: Option<ast::WithSpan<&'a str>>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Option {
    keyword: ast::Span,
    key: ast::WithSpan<&'a str>,
    value: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  PushTag {
    keyword: ast::Span,
    tag: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  PopTag {
    keyword: ast::Span,
    tag: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  PushMeta {
    keyword: ast::Span,
    key: ast::WithSpan<&'a str>,
    value: Option<ast::WithSpan<ast::KeyValueValue<'a>>>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  PopMeta {
    keyword: ast::Span,
    key: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Comment {
    text: ast::WithSpan<&'a str>,
  },
  Headline {
    text: ast::WithSpan<&'a str>,
  },
  Open {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    account: ast::WithSpan<&'a str>,
    currencies: SmallVec<[ast::WithSpan<&'a str>; 2]>,
    opt_booking: Option<ast::WithSpan<&'a str>>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Close {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    account: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Balance {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    account: ast::WithSpan<&'a str>,
    amount: ast::Amount<'a>,
    tolerance: Option<ast::WithSpan<&'a str>>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Pad {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    account: ast::WithSpan<&'a str>,
    from_account: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Commodity {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    currency: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Price {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    currency: ast::WithSpan<&'a str>,
    amount: ast::Amount<'a>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Event {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    event_type: ast::WithSpan<&'a str>,
    desc: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Query {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    name: ast::WithSpan<&'a str>,
    query: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Note {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    account: ast::WithSpan<&'a str>,
    note: ast::WithSpan<&'a str>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Document {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    account: ast::WithSpan<&'a str>,
    filename: ast::WithSpan<&'a str>,
    tags_links: Option<ast::WithSpan<&'a str>>,
    tags: SmallVec<[ast::WithSpan<&'a str>; 2]>,
    links: SmallVec<[ast::WithSpan<&'a str>; 2]>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
  Custom {
    keyword: ast::Span,
    date: ast::WithSpan<&'a str>,
    name: ast::WithSpan<&'a str>,
    values: SmallVec<[ast::CustomValue<'a>; 2]>,
    comment: Option<ast::WithSpan<&'a str>>,
  },
}

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

    let mut is_txn_header = false;
    if !line.content.starts_with(' ') && !line.content.starts_with('\t') {
      let trimmed = line.content.trim_start();
      if let Some(first) = trimmed.chars().next()
        && first.is_ascii_digit()
      {
        let mut parts = trimmed.split_whitespace();
        let date = parts.next().unwrap_or("");
        let flag = parts.next().unwrap_or("");
        let is_single_flag =
          flag.chars().count() == 1 && !flag.chars().next().unwrap_or('0').is_ascii_digit();
        if looks_like_date(date) && (flag == "txn" || is_single_flag) {
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

    if let Some((directive, next_index)) = parse_multiline_plugin(
      &lines,
      index,
      source,
      &parsers.directives.plugin,
      &line_meta,
    ) {
      directives.push(directive);
      index = next_index;
      continue;
    }

    if let Some((directive, next_index)) =
      parse_multiline_query(&lines, index, source, &parsers.directives.query, &line_meta)
    {
      directives.push(directive);
      index = next_index;
      continue;
    }

    match parse_line_directive(line, &parsers.directives, &line_meta) {
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
    offset = end + if has_newline { 1 } else { 0 };
    line_no += 1;
  }

  lines
}

fn raw_block_from<'a>(
  lines: &[Line<'a>],
  mut index: usize,
  source: &'a str,
  meta: ast::Meta,
) -> (ast::Raw<'a>, usize) {
  let start = lines[index].span.start;
  let mut end = lines[index].span.end;
  while index + 1 < lines.len() {
    let next = &lines[index + 1];
    if next.trimmed.is_empty() || next.content.starts_with(' ') || next.content.starts_with('\t') {
      end = next.span.end;
      index += 1;
      continue;
    }
    break;
  }

  let text = &source[start..end];
  let span = ast::Span::from_range(start, end);
  (ast::Raw { meta, span, text }, index + 1)
}

fn parse_multiline_plugin<'a, P>(
  lines: &[Line<'a>],
  start_index: usize,
  source: &'a str,
  parser: &P,
  meta: &ast::Meta,
) -> Option<(ast::Directive<'a>, usize)>
where
  P: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
{
  let line = &lines[start_index];
  if !line.trimmed.starts_with("plugin") {
    return None;
  }

  let mut end_index = start_index;
  let mut quote_count = line.content.matches('"').count();
  while quote_count % 2 != 0 && end_index + 1 < lines.len() {
    end_index += 1;
    quote_count += lines[end_index].content.matches('"').count();
  }

  if quote_count % 2 == 0 && end_index == start_index {
    return None;
  }

  let block_start = lines[start_index].span.start;
  let block_end = lines[end_index].span.end;
  let text = &source[block_start..block_end];

  let parsed = parser.parse(text).into_result().ok()?;
  let span = ast::Span::from_range(block_start, block_end);
  let directive = build_directive_from_kind(
    offset_directive_kind(parsed, block_start),
    meta.clone(),
    span,
  );

  Some((directive, end_index + 1))
}

fn parse_multiline_query<'a, P>(
  lines: &[Line<'a>],
  start_index: usize,
  source: &'a str,
  parser: &P,
  meta: &ast::Meta,
) -> Option<(ast::Directive<'a>, usize)>
where
  P: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
{
  let line = &lines[start_index];
  let mut parts = line.trimmed.split_whitespace();
  let date = parts.next()?;
  let keyword = parts.next()?;
  if !looks_like_date(date) || keyword != "query" {
    return None;
  }

  let mut end_index = start_index;
  let mut quote_count = line.content.matches('"').count();
  while quote_count % 2 != 0 && end_index + 1 < lines.len() {
    end_index += 1;
    quote_count += lines[end_index].content.matches('"').count();
  }

  if quote_count % 2 == 0 && end_index == start_index {
    return None;
  }

  let block_start = lines[start_index].span.start;
  let block_end = lines[end_index].span.end;
  let text = &source[block_start..block_end];

  let parsed = parser.parse(text).into_result().ok()?;
  let span = ast::Span::from_range(block_start, block_end);
  let directive = build_directive_from_kind(
    offset_directive_kind(parsed, block_start),
    meta.clone(),
    span,
  );

  Some((directive, end_index + 1))
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

fn meta_from_line(filename: &str, line: &Line<'_>) -> ast::Meta {
  ast::Meta {
    filename: filename.to_owned(),
    line: line.line_no,
    column: 1,
  }
}

enum LineDirectiveOutcome<'a> {
  Parsed(ast::Directive<'a>),
  NotDirective,
  Error,
}

fn parse_line_with<'a, P>(parser: &P, line: &Line<'a>, meta: &ast::Meta) -> LineDirectiveOutcome<'a>
where
  P: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
{
  match parser.parse(line.content).into_result() {
    Ok(parsed) => {
      let kind = offset_directive_kind(parsed, line.span.start);
      let directive = build_directive_from_kind(kind, meta.clone(), line.span);
      LineDirectiveOutcome::Parsed(directive)
    }
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
  meta: &ast::Meta,
) -> LineDirectiveOutcome<'a>
where
  PInclude: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPlugin: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  POption: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPushtag: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPoptag: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPushmeta: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPopmeta: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PComment: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PHeadline: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  POpen: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PClose: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PBalance: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPad: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PCommodity: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PPrice: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PEvent: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PQuery: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PNote: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PDocument: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
  PCustom: Parser<'a, &'a str, DirectiveKind<'a>, Error<'a>>,
{
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
    return parse_line_with(&parsers.comment, line, meta);
  }
  if first == '#' {
    return parse_line_with(&parsers.comment, line, meta);
  }
  if first == '*' {
    return parse_line_with(&parsers.headline, line, meta);
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
      "open" => parse_line_with(&parsers.open, line, meta),
      "close" => parse_line_with(&parsers.close, line, meta),
      "balance" => parse_line_with(&parsers.balance, line, meta),
      "pad" => parse_line_with(&parsers.pad, line, meta),
      "commodity" => parse_line_with(&parsers.commodity, line, meta),
      "price" => parse_line_with(&parsers.price, line, meta),
      "event" => parse_line_with(&parsers.event, line, meta),
      "query" => parse_line_with(&parsers.query, line, meta),
      "note" => parse_line_with(&parsers.note, line, meta),
      "document" => parse_line_with(&parsers.document, line, meta),
      "custom" => parse_line_with(&parsers.custom, line, meta),
      "*" | "!" => LineDirectiveOutcome::NotDirective,
      _ => LineDirectiveOutcome::NotDirective,
    };
  }
  if first.is_ascii_alphabetic() {
    return match token {
      "plugin" => parse_line_with(&parsers.plugin, line, meta),
      "include" => parse_line_with(&parsers.include, line, meta),
      "pushtag" => parse_line_with(&parsers.pushtag, line, meta),
      "poptag" => parse_line_with(&parsers.poptag, line, meta),
      "pushmeta" => parse_line_with(&parsers.pushmeta, line, meta),
      "popmeta" => parse_line_with(&parsers.popmeta, line, meta),
      "option" => parse_line_with(&parsers.option, line, meta),
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

fn parse_transaction_header<'a>(
  line: &Line<'a>,
  _filename: &str,
  _source: &'a str,
) -> Option<ast::Transaction<'a>> {
  if line.content.starts_with(' ') || line.content.starts_with('\t') {
    return None;
  }

  let trimmed = line.trimmed;
  let mut parts = trimmed.split_whitespace();
  let date = parts.next()?;
  let flag = parts.next()?;
  let is_single_flag =
    flag.chars().count() == 1 && !flag.chars().next().unwrap_or('0').is_ascii_digit();
  if !(flag == "txn" || is_single_flag) {
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

fn build_directive_from_kind<'a>(
  kind: DirectiveKind<'a>,
  meta: ast::Meta,
  span: ast::Span,
) -> ast::Directive<'a> {
  match kind {
    DirectiveKind::Include {
      keyword,
      filename,
      comment,
    } => ast::Directive::Include(ast::Include {
      meta,
      span,
      keyword,
      filename,
      comment,
    }),
    DirectiveKind::Plugin {
      keyword,
      name,
      config,
      comment,
    } => ast::Directive::Plugin(ast::Plugin {
      meta,
      span,
      keyword,
      name,
      config,
      comment,
    }),
    DirectiveKind::Option {
      keyword,
      key,
      value,
      comment,
    } => ast::Directive::Option(ast::OptionDirective {
      meta,
      span,
      keyword,
      key,
      value,
      comment,
    }),
    DirectiveKind::PushTag {
      keyword,
      tag,
      comment,
    } => ast::Directive::PushTag(ast::TagDirective {
      meta,
      span,
      keyword,
      tag,
      comment,
    }),
    DirectiveKind::PopTag {
      keyword,
      tag,
      comment,
    } => ast::Directive::PopTag(ast::TagDirective {
      meta,
      span,
      keyword,
      tag,
      comment,
    }),
    DirectiveKind::PushMeta {
      keyword,
      key,
      value,
      comment,
    } => ast::Directive::PushMeta(ast::PushMeta {
      meta,
      span,
      keyword,
      key,
      value,
      comment,
    }),
    DirectiveKind::PopMeta {
      keyword,
      key,
      comment,
    } => ast::Directive::PopMeta(ast::PopMeta {
      meta,
      span,
      keyword,
      key,
      comment,
    }),
    DirectiveKind::Comment { text } => ast::Directive::Comment(ast::Comment { meta, span, text }),
    DirectiveKind::Headline { text } => {
      ast::Directive::Headline(ast::Headline { meta, span, text })
    }
    DirectiveKind::Open {
      keyword,
      date,
      account,
      currencies,
      opt_booking,
      comment,
    } => ast::Directive::Open(ast::Open {
      meta,
      span,
      keyword,
      date,
      account,
      currencies,
      opt_booking,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Close {
      keyword,
      date,
      account,
      comment,
    } => ast::Directive::Close(ast::Close {
      meta,
      span,
      keyword,
      date,
      account,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Balance {
      keyword,
      date,
      account,
      amount,
      tolerance,
      comment,
    } => ast::Directive::Balance(ast::Balance {
      meta,
      span,
      keyword,
      date,
      account,
      amount,
      tolerance,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Pad {
      keyword,
      date,
      account,
      from_account,
      comment,
    } => ast::Directive::Pad(ast::Pad {
      meta,
      span,
      keyword,
      date,
      account,
      from_account,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Commodity {
      keyword,
      date,
      currency,
      comment,
    } => ast::Directive::Commodity(ast::Commodity {
      meta,
      span,
      keyword,
      date,
      currency,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Price {
      keyword,
      date,
      currency,
      amount,
      comment,
    } => ast::Directive::Price(ast::Price {
      meta,
      span,
      keyword,
      date,
      currency,
      amount,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Event {
      keyword,
      date,
      event_type,
      desc,
      comment,
    } => ast::Directive::Event(ast::Event {
      meta,
      span,
      keyword,
      date,
      event_type,
      desc,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Query {
      keyword,
      date,
      name,
      query,
      comment,
    } => ast::Directive::Query(ast::Query {
      meta,
      span,
      keyword,
      date,
      name,
      query,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Note {
      keyword,
      date,
      account,
      note,
      comment,
    } => ast::Directive::Note(ast::Note {
      meta,
      span,
      keyword,
      date,
      account,
      note,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Document {
      keyword,
      date,
      account,
      filename,
      tags_links,
      tags,
      links,
      comment,
    } => ast::Directive::Document(ast::Document {
      meta,
      span,
      keyword,
      date,
      account,
      filename,
      tags_links,
      tags,
      links,
      comment,
      key_values: SmallVec::new(),
    }),
    DirectiveKind::Custom {
      keyword,
      date,
      name,
      values,
      comment,
    } => ast::Directive::Custom(ast::Custom {
      meta,
      span,
      keyword,
      date,
      name,
      values,
      comment,
      key_values: SmallVec::new(),
    }),
  }
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

fn offset_directive_kind<'a>(kind: DirectiveKind<'a>, offset: usize) -> DirectiveKind<'a> {
  match kind {
    DirectiveKind::Include {
      keyword,
      filename,
      comment,
    } => DirectiveKind::Include {
      keyword: offset_span(keyword, offset),
      filename: offset_with_span(filename, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Plugin {
      keyword,
      name,
      config,
      comment,
    } => DirectiveKind::Plugin {
      keyword: offset_span(keyword, offset),
      name: offset_with_span(name, offset),
      config: config.map(|value| offset_with_span(value, offset)),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Option {
      keyword,
      key,
      value,
      comment,
    } => DirectiveKind::Option {
      keyword: offset_span(keyword, offset),
      key: offset_with_span(key, offset),
      value: offset_with_span(value, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::PushTag {
      keyword,
      tag,
      comment,
    } => DirectiveKind::PushTag {
      keyword: offset_span(keyword, offset),
      tag: offset_with_span(tag, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::PopTag {
      keyword,
      tag,
      comment,
    } => DirectiveKind::PopTag {
      keyword: offset_span(keyword, offset),
      tag: offset_with_span(tag, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::PushMeta {
      keyword,
      key,
      value,
      comment,
    } => DirectiveKind::PushMeta {
      keyword: offset_span(keyword, offset),
      key: offset_with_span(key, offset),
      value: value.map(|value| offset_with_span(value, offset)),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::PopMeta {
      keyword,
      key,
      comment,
    } => DirectiveKind::PopMeta {
      keyword: offset_span(keyword, offset),
      key: offset_with_span(key, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Comment { text } => DirectiveKind::Comment {
      text: offset_with_span(text, offset),
    },
    DirectiveKind::Headline { text } => DirectiveKind::Headline {
      text: offset_with_span(text, offset),
    },
    DirectiveKind::Open {
      keyword,
      date,
      account,
      currencies,
      opt_booking,
      comment,
    } => DirectiveKind::Open {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      account: offset_with_span(account, offset),
      currencies: currencies
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect(),
      opt_booking: opt_booking.map(|value| offset_with_span(value, offset)),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Close {
      keyword,
      date,
      account,
      comment,
    } => DirectiveKind::Close {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      account: offset_with_span(account, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Balance {
      keyword,
      date,
      account,
      amount,
      tolerance,
      comment,
    } => DirectiveKind::Balance {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      account: offset_with_span(account, offset),
      amount: offset_amount(amount, offset),
      tolerance: tolerance.map(|value| offset_with_span(value, offset)),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Pad {
      keyword,
      date,
      account,
      from_account,
      comment,
    } => DirectiveKind::Pad {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      account: offset_with_span(account, offset),
      from_account: offset_with_span(from_account, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Commodity {
      keyword,
      date,
      currency,
      comment,
    } => DirectiveKind::Commodity {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      currency: offset_with_span(currency, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Price {
      keyword,
      date,
      currency,
      amount,
      comment,
    } => DirectiveKind::Price {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      currency: offset_with_span(currency, offset),
      amount: offset_amount(amount, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Event {
      keyword,
      date,
      event_type,
      desc,
      comment,
    } => DirectiveKind::Event {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      event_type: offset_with_span(event_type, offset),
      desc: offset_with_span(desc, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Query {
      keyword,
      date,
      name,
      query,
      comment,
    } => DirectiveKind::Query {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      name: offset_with_span(name, offset),
      query: offset_with_span(query, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Note {
      keyword,
      date,
      account,
      note,
      comment,
    } => DirectiveKind::Note {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      account: offset_with_span(account, offset),
      note: offset_with_span(note, offset),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Document {
      keyword,
      date,
      account,
      filename,
      tags_links,
      tags,
      links,
      comment,
    } => DirectiveKind::Document {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      account: offset_with_span(account, offset),
      filename: offset_with_span(filename, offset),
      tags_links: tags_links.map(|value| offset_with_span(value, offset)),
      tags: tags
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect(),
      links: links
        .into_iter()
        .map(|value| offset_with_span(value, offset))
        .collect(),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
    DirectiveKind::Custom {
      keyword,
      date,
      name,
      values,
      comment,
    } => DirectiveKind::Custom {
      keyword: offset_span(keyword, offset),
      date: offset_with_span(date, offset),
      name: offset_with_span(name, offset),
      values: values
        .into_iter()
        .map(|value| offset_custom_value(value, offset))
        .collect(),
      comment: comment.map(|value| offset_with_span(value, offset)),
    },
  }
}

fn keyword_span_parser<'src>(
  keyword: &'static str,
) -> impl Parser<'src, &'src str, ast::Span, Error<'src>> {
  just(keyword).map_with(|_: &str, e| {
    let span: SimpleSpan = e.span();
    ast::Span::from_range(span.start, span.end)
  })
}

pub(crate) fn include_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();

  keyword_span_parser("include")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .map(|(keyword, filename)| DirectiveKind::Include {
      keyword,
      filename,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn plugin_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();

  keyword_span_parser("plugin")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .map(|((keyword, name), config)| DirectiveKind::Plugin {
      keyword,
      name,
      config,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn pushtag_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let ws1 = ws1_parser();

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
    .map(|(keyword, tag)| DirectiveKind::PushTag {
      keyword,
      tag,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn poptag_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let ws1 = ws1_parser();

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
    .map(|(keyword, tag)| DirectiveKind::PopTag {
      keyword,
      tag,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn pushmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let quoted_string = quoted_string_parser();

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
    .then_ignore(ws1_parser())
    .then(pushmeta_key)
    .then_ignore(ws0_parser())
    .then_ignore(just(':'))
    .then_ignore(ws0_parser())
    .then(pushmeta_value)
    .map(|((keyword, key), value)| DirectiveKind::PushMeta {
      keyword,
      key,
      value,
      comment: None,
    })
    .then_ignore(ws0_parser())
}

pub(crate) fn popmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
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
    .then_ignore(ws1_parser())
    .then(popmeta_key)
    .then_ignore(just(':').or_not())
    .map(|(keyword, key)| DirectiveKind::PopMeta {
      keyword,
      key,
      comment: None,
    })
    .then_ignore(ws0_parser())
}

pub(crate) fn option_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  keyword_span_parser("option")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .map(|((keyword, key), value)| DirectiveKind::Option {
      keyword,
      key,
      value,
      comment: None,
    })
    .then_ignore(ws0_parser())
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

fn inline_comment_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  ws0_parser()
    .ignore_then(just(';').then(any().repeated()).to_slice())
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text)
    })
}

pub(crate) fn comment_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  ws0
    .ignore_then(choice((just(';').ignored(), just('#').ignored())))
    .then(any().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      DirectiveKind::Comment {
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      }
    })
}

pub(crate) fn headline_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  ws0
    .then(just('*'))
    .then(any().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      let text = text.trim_start();
      DirectiveKind::Headline {
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      }
    })
}

pub(crate) fn open_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();
  let quoted_string = quoted_string_parser();
  let bare_string = bare_string_parser();
  let open_currency =
    bare_string.filter(|value| !value.content.starts_with('"') && !value.content.starts_with(';'));
  let comment = inline_comment_parser().or_not();

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
    .then(comment)
    .map(
      |(((((date, keyword), account), currencies), opt_booking), comment)| {
        let currencies: SmallVec<[ast::WithSpan<&'src str>; 2]> =
          currencies.into_iter().flat_map(split_currencies).collect();
        DirectiveKind::Open {
          keyword,
          date,
          account,
          currencies,
          opt_booking,
          comment,
        }
      },
    )
    .then_ignore(ws0)
}

pub(crate) fn close_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("close"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .map(|((date, keyword), account)| DirectiveKind::Close {
      keyword,
      date,
      account,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn balance_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();
  let currency = || spanned_token_parser().filter(|value| looks_like_currency(value.content));
  let comment = inline_comment_parser().or_not();

  let tolerance = ws0_parser()
    .ignore_then(just('~'))
    .ignore_then(ws0_parser())
    .ignore_then(number_literal_parser())
    .or_not();

  let amount = number_literal_parser()
    .then(tolerance)
    .then(ws1_parser().ignore_then(currency()))
    .map_with(|((number, tolerance), currency), e| {
      let span: SimpleSpan = e.span();
      let raw_span = ast::Span::from_range(span.start, span.end);
      let raw: &str = e.slice();
      let trimmed = raw.trim();
      let offset = raw.find(trimmed).unwrap_or(0);
      let raw_span = ast::Span::from_range(
        raw_span.start + offset,
        raw_span.start + offset + trimmed.len(),
      );
      (
        ast::Amount {
          raw: ast::WithSpan::new(raw_span, trimmed),
          number: ast::NumberExpr::Literal(number),
          currency: Some(currency),
        },
        tolerance,
      )
    });

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("balance"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .then(comment)
    .map(
      |((((date, keyword), account), (amount, tolerance)), comment)| DirectiveKind::Balance {
        keyword,
        date,
        account,
        amount,
        tolerance,
        comment,
      },
    )
    .then_ignore(ws0)
}

pub(crate) fn pad_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("pad"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .map(
      |(((date, keyword), account), from_account)| DirectiveKind::Pad {
        keyword,
        date,
        account,
        from_account,
        comment: None,
      },
    )
    .then_ignore(ws0)
}

pub(crate) fn commodity_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();

  date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("commodity"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .map(|((date, keyword), currency)| DirectiveKind::Commodity {
      keyword,
      date,
      currency,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn price_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
  let ws0 = ws0_parser();
  let date = date_parser();
  let comment = inline_comment_parser().or_not();

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
    .then(comment)
    .map(
      |((((date, keyword), currency), amount), comment)| DirectiveKind::Price {
        keyword,
        date,
        currency,
        amount,
        comment,
      },
    )
    .then_ignore(ws0)
}

pub(crate) fn event_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
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
    .map(
      |(((date, keyword), event_type), desc)| DirectiveKind::Event {
        keyword,
        date,
        event_type,
        desc,
        comment: None,
      },
    )
    .then_ignore(ws0)
}

pub(crate) fn query_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
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
    .map(|(((date, keyword), name), query)| DirectiveKind::Query {
      keyword,
      date,
      name,
      query,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn note_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
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
    .map(|(((date, keyword), account), note)| DirectiveKind::Note {
      keyword,
      date,
      account,
      note,
      comment: None,
    })
    .then_ignore(ws0)
}

pub(crate) fn document_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
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
      DirectiveKind::Document {
        keyword,
        date,
        account,
        filename,
        tags_links,
        tags,
        links,
        comment: None,
      }
    })
    .then_ignore(ws0)
}

pub(crate) fn custom_directive_parser<'src>()
-> impl Parser<'src, &'src str, DirectiveKind<'src>, Error<'src>> {
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
      let values: SmallVec<[ast::CustomValue<'src>; 2]> = values.into_iter().collect();
      DirectiveKind::Custom {
        keyword,
        date,
        name,
        values,
        comment: None,
      }
    })
    .then_ignore(ws0)
}

fn number_literal_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  let sign = just('+').or(just('-')).or_not();
  let digits_or_sep = any()
    .filter(|c: &char| c.is_ascii_digit() || *c == ',' || *c == '_')
    .repeated()
    .at_least(1);
  let frac = just('.').then(digits_or_sep.clone()).or_not();
  sign
    .then(digits_or_sep)
    .then(frac)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    })
}

fn number_expr_parser<'src>() -> impl Parser<'src, &'src str, ast::NumberExpr<'src>, Error<'src>> {
  recursive(|_expr| {
    let ws0 = ws0_parser().boxed();

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

    let mul_chain = ws0
      .clone()
      .ignore_then(op_mul.clone())
      .then_ignore(ws0.clone())
      .then(literal.clone())
      .repeated();

    let product = literal
      .clone()
      .foldl(mul_chain, |left, (op, right)| {
        let span = ast::Span::from_range(left.span().start, right.span().end);
        ast::NumberExpr::Binary {
          span,
          left: Box::new(left),
          op,
          right: Box::new(right),
        }
      })
      .boxed();

    let add_chain = ws0
      .clone()
      .ignore_then(op_add)
      .then_ignore(ws0)
      .then(product.clone())
      .repeated();

    product.foldl(add_chain, |left, (op, right)| {
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
  let ws1 = ws1_parser();

  let quoted_string = quoted_string_parser();

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

pub(crate) fn key_value_parser<'src>()
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

  let quoted_string = quoted_string_parser();

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
        meta: empty_meta(),
        span: ast::Span::from_range(span.start, span.end),
        key,
        value,
      }
    })
}

pub(crate) fn posting_parser<'src>() -> impl Parser<'src, &'src str, ast::Posting<'src>, Error<'src>>
{
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
    .then(ws1_parser().ignore_then(currency).or_not())
    .map_with(|(number, currency), e| {
      let span: SimpleSpan = e.span();
      let raw = ast::WithSpan::new(ast::Span::from_range(span.start, span.end), e.slice());
      ast::Amount {
        raw,
        number,
        currency,
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

  ws0_parser()
    .ignore_then(optflag.or_not())
    .then_ignore(ws0_parser())
    .then(account)
    .then(ws1_parser().ignore_then(amount).or_not())
    .then(ws0_parser().ignore_then(cost_spec_parser()).or_not())
    .then(
      ws0_parser()
        .ignore_then(price_operator)
        .then_ignore(ws1_parser())
        .then(price_annotation)
        .map(|(op, annotation)| (Some(op), Some(annotation)))
        .or_not()
        .map(|value| value.unwrap_or((None, None))),
    )
    .then(ws0_parser().ignore_then(comment).or_not())
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
    .then(ws1_parser().ignore_then(currency).or_not())
    .map(|(per, currency)| ast::CostAmount {
      per: Some(per),
      total: None,
      currency,
    });

  let amount_total = number_expr
    .clone()
    .or_not()
    .then_ignore(ws0_parser())
    .then_ignore(just('#'))
    .then_ignore(ws0_parser())
    .then(number_expr.clone().or_not())
    .then(ws1_parser().ignore_then(currency).or_not())
    .map(|((per, total), currency)| ast::CostAmount {
      per,
      total,
      currency,
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
    date.map(CostComp::Date),
    cost_amount.map(CostComp::Amount),
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

  let comp_list = || {
    cost_comp
      .clone()
      .then(
        ws0_parser()
          .ignore_then(just(','))
          .ignore_then(ws0_parser())
          .ignore_then(cost_comp.clone())
          .repeated()
          .collect::<Vec<_>>(),
      )
      .map(|(head, tail): (CostComp<'src>, Vec<CostComp<'src>>)| {
        let mut comps = Vec::with_capacity(1 + tail.len());
        comps.push(head);
        comps.extend(tail);
        comps
      })
      .or_not()
  };

  let single = just('{')
    .then(comp_list())
    .then_ignore(just('}'))
    .map_with(|(_, comps), e| {
      let span: SimpleSpan = e.span();
      (comps, false, span, e.slice())
    });

  let double = just("{{")
    .then(comp_list())
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
