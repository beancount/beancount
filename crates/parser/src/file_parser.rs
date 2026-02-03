//! 基于单个巨大 chumsky 组合的整文件解析器（file_parser.rs）。
//!
//! 这个模块用一个“巨大的” Chumsky 解析器一次性解析整个文件，
//! 子解析器直接产出 `ast::Directive`，并在本模块内补齐 meta。
//! 入口为 `parse_str_chumsky`。
use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::utils::{
  attach_key_values, empty_meta, expand_directive_span_to, line_starts, looks_like_currency,
  looks_like_date, meta_from_offset, parse_tags_links, split_currencies,
};
use crate::{Error, ParseError, ast};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FileItem<'a> {
  pub text: &'a str,
  pub span: ast::Span,
  pub kind: FileItemKind<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum FileItemKind<'a> {
  Directive(ast::Directive<'a>),
  Posting(ast::Posting<'a>),
  KeyValue(ast::KeyValue<'a>),
  TagsLinks(ast::WithSpan<&'a str>),
  Empty,
  Raw,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TransactionHeader<'a> {
  date: ast::WithSpan<&'a str>,
  flag: ast::WithSpan<&'a str>,
  payee: Option<ast::WithSpan<&'a str>>,
  narration: Option<ast::WithSpan<&'a str>>,
  tags_links: Option<ast::WithSpan<&'a str>>,
}

enum DirectiveKind<'a> {
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

pub(crate) fn file_items_parser<'src>()
-> impl Parser<'src, &'src str, Vec<FileItem<'src>>, Error<'src>> {
  let directive_line = directive_parser().map_with(|directive, e| {
    let span: SimpleSpan = e.span();
    FileItem {
      text: e.slice(),
      span: ast::Span::from_range(span.start, span.end),
      kind: FileItemKind::Directive(directive),
    }
  });

  let posting_line = indented_posting_parser().map_with(|posting, e| {
    let span: SimpleSpan = e.span();
    FileItem {
      text: e.slice(),
      span: ast::Span::from_range(span.start, span.end),
      kind: FileItemKind::Posting(posting),
    }
  });

  let key_value_line = indented_key_value_parser().map_with(|kv, e| {
    let span: SimpleSpan = e.span();
    FileItem {
      text: e.slice(),
      span: ast::Span::from_range(span.start, span.end),
      kind: FileItemKind::KeyValue(kv),
    }
  });

  let tags_links_line = tags_links_line_parser().map_with(|tags_links, e| {
    let span: SimpleSpan = e.span();
    FileItem {
      text: e.slice(),
      span: ast::Span::from_range(span.start, span.end),
      kind: FileItemKind::TagsLinks(tags_links),
    }
  });

  // Ensure the empty-line parser consumes at least one character to avoid
  // zero-length progress inside the repeated() loop.
  let empty_line = choice((
    ws0_parser().then_ignore(just('\n')).map_with(|_, e| {
      let span: SimpleSpan = e.span();
      FileItem {
        text: e.slice(),
        span: ast::Span::from_range(span.start, span.end),
        kind: FileItemKind::Empty,
      }
    }),
    ws1_parser().then_ignore(end()).map_with(|_, e| {
      let span: SimpleSpan = e.span();
      FileItem {
        text: e.slice(),
        span: ast::Span::from_range(span.start, span.end),
        kind: FileItemKind::Empty,
      }
    }),
  ));

  let raw_line = any()
    .filter(|c: &char| *c != '\n')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|_: &str, e| {
      let span: SimpleSpan = e.span();
      FileItem {
        text: e.slice(),
        span: ast::Span::from_range(span.start, span.end),
        kind: FileItemKind::Raw,
      }
    });

  choice((
    directive_line.then_ignore(line_end()),
    key_value_line.then_ignore(line_end()),
    posting_line.then_ignore(line_end()),
    tags_links_line.then_ignore(line_end()),
    empty_line,
    raw_line.then_ignore(line_end()),
  ))
  .boxed()
  .repeated()
  .collect::<Vec<_>>()
  .then_ignore(end())
  .boxed()
}

pub fn parse_str_chumsky<'a>(
  source: &'a str,
  filename: &str,
) -> std::result::Result<Vec<ast::Directive<'a>>, ParseError> {
  let line_starts = line_starts(source);
  let items = file_items_parser()
    .parse(source)
    .into_result()
    .map_err(|mut errors| match errors.pop() {
      Some(err) => {
        let span = err.span();
        let meta = meta_from_offset(filename, span.start, &line_starts);
        ParseError {
          filename: meta.filename,
          line: meta.line,
          column: meta.column,
          message: err.to_string(),
        }
      }
      None => ParseError {
        filename: filename.to_owned(),
        line: 1,
        column: 1,
        message: "parse error".to_string(),
      },
    })?;

  Ok(assemble_directives(items, source, filename, &line_starts))
}

fn assemble_directives<'a>(
  items: Vec<FileItem<'a>>,
  source: &'a str,
  filename: &str,
  line_starts: &[usize],
) -> Vec<ast::Directive<'a>> {
  let mut directives = Vec::with_capacity(items.len() / 4);
  let mut index = 0;

  while index < items.len() {
    let item = &items[index];
    if matches!(item.kind, FileItemKind::Empty) {
      index += 1;
      continue;
    }
    match &item.kind {
      FileItemKind::Directive(directive) => {
        if let ast::Directive::Transaction(txn) = directive {
          let txn = assemble_transaction(
            txn.clone(),
            &items,
            &mut index,
            source,
            filename,
            line_starts,
          );
          directives.push(ast::Directive::Transaction(txn));
          continue;
        }

        let mut directive = directive.clone();
        let meta = meta_from_offset(filename, item.span.start, line_starts);
        rewrite_meta(meta, &mut directive);

        let mut end_span = item.span.end;
        let mut key_values: SmallVec<[ast::KeyValue<'a>; 4]> = SmallVec::new();
        let mut lookahead = index + 1;
        while lookahead < items.len() {
          let next = &items[lookahead];
          match &next.kind {
            FileItemKind::KeyValue(kv) => {
              let mut kv = kv.clone();
              kv.meta = meta_from_offset(filename, next.span.start, line_starts);
              key_values.push(kv);
              end_span = next.span.end;
              lookahead += 1;
            }
            FileItemKind::Empty => break,
            _ => break,
          }
        }

        attach_key_values(&mut directive, key_values);
        directive = expand_directive_span_to(directive, item.span.start, end_span, source);
        directives.push(directive);
        index = lookahead;
      }
      _ => {
        let (raw, next_index) = raw_block_from(&items, index, source, filename, line_starts);
        directives.push(ast::Directive::Raw(raw));
        index = next_index;
      }
    }
  }

  directives
}

fn raw_block_from<'a>(
  items: &[FileItem<'a>],
  mut index: usize,
  source: &'a str,
  filename: &str,
  line_starts: &[usize],
) -> (ast::Raw<'a>, usize) {
  let start = items[index].span.start;
  let mut end = items[index].span.end;

  while index + 1 < items.len() {
    let next = &items[index + 1];
    let trimmed = next.text.trim();
    if trimmed.is_empty() || next.text.starts_with(' ') || next.text.starts_with('\t') {
      end = next.span.end;
      index += 1;
    } else {
      break;
    }
  }

  let meta = meta_from_offset(filename, start, line_starts);
  let text = &source[start..end];
  let span = ast::Span::from_range(start, end);
  (ast::Raw { meta, span, text }, index + 1)
}

fn assemble_transaction<'a>(
  mut txn: ast::Transaction<'a>,
  items: &[FileItem<'a>],
  index: &mut usize,
  source: &'a str,
  filename: &str,
  line_starts: &[usize],
) -> ast::Transaction<'a> {
  let item = &items[*index];
  let mut end_span = item.span.end;
  let mut tags_links_lines: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
  let mut comments: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
  let mut key_values: SmallVec<[ast::KeyValue<'a>; 4]> = SmallVec::new();
  let mut postings: SmallVec<[ast::Posting<'a>; 4]> = SmallVec::new();

  *index += 1;
  while *index < items.len() {
    let next = &items[*index];
    match &next.kind {
      FileItemKind::Empty => {
        end_span = next.span.end;
        break;
      }
      FileItemKind::Posting(posting) => {
        let mut posting = posting.clone();
        posting.meta = meta_from_offset(filename, next.span.start, line_starts);
        let mut span_end = next.span.end;
        if span_end < source.len() && source.as_bytes().get(span_end) == Some(&b'\n') {
          span_end += 1;
          posting.span = ast::Span::from_range(posting.span.start, span_end);
        }
        postings.push(posting);
        end_span = span_end;
        *index += 1;
      }
      FileItemKind::KeyValue(kv) => {
        let mut kv = kv.clone();
        kv.meta = meta_from_offset(filename, next.span.start, line_starts);
        if let Some(last) = postings.last_mut() {
          last.key_values.push(kv);
        } else {
          key_values.push(kv);
        }
        end_span = next.span.end;
        *index += 1;
      }
      FileItemKind::TagsLinks(tags_links) => {
        tags_links_lines.push(tags_links.clone());
        end_span = next.span.end;
        *index += 1;
      }
      FileItemKind::Directive(ast::Directive::Comment(comment)) => {
        comments.push(comment.text.clone());
        end_span = next.span.end;
        *index += 1;
      }
      _ => break,
    }
  }

  let meta = meta_from_offset(filename, item.span.start, line_starts);
  txn.meta = meta.clone();
  txn.span = item.span;
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
  let mut txn_end = end_span;
  if txn_end < source.len() && source.as_bytes().get(txn_end) == Some(&b'\n') {
    txn_end += 1;
  }
  txn.span = ast::Span::from_range(item.span.start, txn_end);

  txn
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

fn directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let includes = choice((
    include_directive_parser(),
    plugin_directive_parser(),
    option_directive_parser(),
  ))
  .boxed();

  let tags = choice((pushtag_directive_parser(), poptag_directive_parser())).boxed();

  let meta = choice((pushmeta_directive_parser(), popmeta_directive_parser())).boxed();

  let comments = choice((comment_directive_parser(), headline_directive_parser())).boxed();

  let accounts = choice((
    open_directive_parser(),
    close_directive_parser(),
    balance_directive_parser(),
    pad_directive_parser(),
    commodity_directive_parser(),
    price_directive_parser(),
    event_directive_parser(),
    query_directive_parser(),
    note_directive_parser(),
    document_directive_parser(),
    custom_directive_parser(),
  ))
  .boxed();

  choice((includes, tags, meta, comments, accounts, transaction_header_parser())).boxed()
}

fn build_directive_from_kind<'a>(kind: DirectiveKind<'a>, span: ast::Span) -> ast::Directive<'a> {
  match kind {
    DirectiveKind::Include {
      keyword,
      filename,
      comment,
    } => ast::Directive::Include(ast::Include {
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
      span,
      keyword,
      key,
      comment,
    }),
    DirectiveKind::Comment { text } => ast::Directive::Comment(ast::Comment {
      meta: empty_meta(),
      span,
      text,
    }),
    DirectiveKind::Headline { text } => ast::Directive::Headline(ast::Headline {
      meta: empty_meta(),
      span,
      text,
    }),
    DirectiveKind::Open {
      keyword,
      date,
      account,
      currencies,
      opt_booking,
      comment,
    } => ast::Directive::Open(ast::Open {
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
      meta: empty_meta(),
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
  keyword_span_parser("include")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .map(|(keyword, filename)| DirectiveKind::Include {
      keyword,
      filename,
      comment: None,
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn plugin_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn pushtag_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
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
    .then_ignore(ws1_parser())
    .then(tag_token)
    .map(|(keyword, tag)| DirectiveKind::PushTag {
      keyword,
      tag,
      comment: None,
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn poptag_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .then_ignore(ws1_parser())
    .then(tag_token)
    .map(|(keyword, tag)| DirectiveKind::PopTag {
      keyword,
      tag,
      comment: None,
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn pushmeta_directive_parser<'src>()
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
    .map(|((keyword, key), value)| DirectiveKind::PushMeta {
      keyword,
      key,
      value,
      comment: None,
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn popmeta_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn option_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
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

fn line_end<'src>() -> impl Parser<'src, &'src str, (), Error<'src>> {
  just('\n').or_not().ignored()
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
  any()
    .filter(|c: &char| *c != '\n')
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

fn comment_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  ws0_parser()
    .then(just(';'))
    .then(any().repeated())
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      DirectiveKind::Comment {
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      }
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
}

fn headline_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  ws0_parser()
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
}

fn open_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();
  let open_currency = bare_string_parser().filter(|value| !value.content.starts_with('"'));

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
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .map(|((((date, keyword), account), currencies), opt_booking)| {
      let currencies = currencies.into_iter().flat_map(split_currencies).collect();
      DirectiveKind::Open {
        keyword,
        date,
        account,
        currencies,
        opt_booking,
        comment: None,
      }
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn close_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn balance_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();

  let currency = || spanned_token_parser().filter(|value| looks_like_currency(value.content));

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
    .map(
      |(((date, keyword), account), (amount, tolerance))| DirectiveKind::Balance {
        keyword,
        date,
        account,
        amount,
        tolerance,
        comment: None,
      },
    )
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
    .boxed()
}

fn pad_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
    .boxed()
}

fn commodity_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn price_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map(
      |(((date, keyword), currency), amount)| DirectiveKind::Price {
        keyword,
        date,
        currency,
        amount,
        comment: None,
      },
    )
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn event_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn query_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn note_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn document_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
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
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn custom_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
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
    .map(|(((date, keyword), name), values)| DirectiveKind::Custom {
      keyword,
      date,
      name,
      values: values.into_iter().collect(),
      comment: None,
    })
    .map_with(|kind, e| {
      let span: SimpleSpan = e.span();
      build_directive_from_kind(kind, ast::Span::from_range(span.start, span.end))
    })
    .then_ignore(ws0_parser())
}

fn transaction_header_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let flag = spanned_token_parser().filter(|value| {
    let chars = value.content.chars().collect::<Vec<_>>();
    let is_single_flag = chars.len() == 1 && !chars[0].is_ascii_digit();
    value.content == "txn" || is_single_flag
  });

  date_parser()
    .then_ignore(ws1_parser())
    .then(flag)
    .then(any().filter(|c: &char| *c != '\n').repeated().to_slice())
    .map_with(|_, e| {
      let span: SimpleSpan = e.span();
      let header = parse_transaction_header_line(e.slice(), span.start);
      let span = ast::Span::from_range(span.start, span.end);
      ast::Directive::Transaction(ast::Transaction {
        meta: empty_meta(),
        span,
        date: header.date,
        txn: Some(header.flag),
        payee: header.payee,
        narration: header.narration,
        tags_links: header.tags_links,
        tags: SmallVec::new(),
        links: SmallVec::new(),
        comment: None,
        tags_links_lines: SmallVec::new(),
        comments: SmallVec::new(),
        key_values: SmallVec::new(),
        postings: SmallVec::new(),
      })
    })
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

  let op_any = choice((op_mul.clone(), op_add.clone())).boxed();
  let op_any_sp = ws0
    .clone()
    .ignore_then(op_any)
    .then_ignore(ws0.clone())
    .boxed();

  literal
    .clone()
    .then(
      op_any_sp
        .then(literal.clone())
        .repeated()
        .collect::<Vec<_>>(),
    )
    .map(|(first, rest): (
      ast::NumberExpr<'src>,
      Vec<(ast::WithSpan<ast::BinaryOp>, ast::NumberExpr<'src>)>,
    )| {
      // Build AST with precedence: * and / before + and -.
      let mut nums: Vec<ast::NumberExpr<'src>> = Vec::with_capacity(rest.len() + 1);
      let mut ops: Vec<ast::WithSpan<ast::BinaryOp>> = Vec::with_capacity(rest.len());

      nums.push(first);
      for (op, num) in rest {
        ops.push(op);
        nums.push(num);
      }

      // First pass: collapse * and /.
      let mut collapsed_nums: Vec<ast::NumberExpr<'src>> = Vec::with_capacity(nums.len());
      let mut collapsed_ops: Vec<ast::WithSpan<ast::BinaryOp>> = Vec::with_capacity(ops.len());

      let mut current = nums.remove(0);
      for op in ops.into_iter() {
        let next_num = nums.remove(0);
        match op.content {
          ast::BinaryOp::Mul | ast::BinaryOp::Div => {
            let span = ast::Span::from_range(current.span().start, next_num.span().end);
            current = ast::NumberExpr::Binary {
              span,
              left: Box::new(current),
              op,
              right: Box::new(next_num),
            };
          }
          ast::BinaryOp::Add | ast::BinaryOp::Sub => {
            collapsed_nums.push(current);
            collapsed_ops.push(op);
            current = next_num;
          }
        }
      }
      collapsed_nums.push(current);

      // Second pass: left-assoc add/sub on collapsed lists.
      let mut iter_nums = collapsed_nums.into_iter();
      let mut result = iter_nums
        .next()
        .expect("number expression requires at least one literal");
      for (op, right) in collapsed_ops.into_iter().zip(iter_nums) {
        let span = ast::Span::from_range(result.span().start, right.span().end);
        result = ast::NumberExpr::Binary {
          span,
          left: Box::new(result),
          op,
          right: Box::new(right),
        };
      }

      result
    })
    .boxed()
}

fn custom_value_parser<'src>() -> impl Parser<'src, &'src str, ast::CustomValue<'src>, Error<'src>>
{
  let bool_value = bare_string_parser().filter(|value| {
    value.content.eq_ignore_ascii_case("true") || value.content.eq_ignore_ascii_case("false")
  });

  let date_value = bare_string_parser().filter(|value| looks_like_date(value.content));

  let currency_value = bare_string_parser().filter(|value| looks_like_currency(value.content));

  let amount_value = number_literal_parser()
    .then_ignore(ws1_parser())
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

  let string_value = quoted_string_parser().map(|raw| ast::CustomValue {
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

  let account_value = bare_string_parser().map(|raw| ast::CustomValue {
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
        meta: empty_meta(),
        span: ast::Span::from_range(span.start, span.end),
        key,
        value,
      }
    })
}

fn indented_key_value_parser<'src>()
-> impl Parser<'src, &'src str, ast::KeyValue<'src>, Error<'src>> {
  let ws1 = ws1_parser();
  ws1.ignore_then(key_value_parser().filter(|kv| is_key_token(kv.key.content)))
}

fn indented_posting_parser<'src>() -> impl Parser<'src, &'src str, ast::Posting<'src>, Error<'src>>
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

  let account = bare_string_parser().filter(|value| value.content.contains(':'));

  let currency_for_amount = bare_string_parser().filter(|value| looks_like_currency(value.content));

  let amount = number_expr_parser()
    .then_ignore(ws1_parser())
    .then(currency_for_amount)
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
    bare_string_parser()
      .filter(|value| looks_like_currency(value.content))
      .map_with(|currency, e| {
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

  ws1_parser()
    .ignore_then(optflag.or_not())
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
    .then_ignore(ws1_parser())
    .then(currency)
    .map(|(per, currency)| ast::CostAmount {
      per: Some(per),
      total: None,
      currency: Some(currency),
    });

  let amount_total = number_expr
    .clone()
    .or_not()
    .then_ignore(ws0_parser())
    .then_ignore(just('#'))
    .then_ignore(ws0_parser())
    .then(number_expr.clone().or_not())
    .then_ignore(ws1_parser())
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

fn tags_links_line_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  any()
    .filter(|c: &char| *c != '\n')
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|line: &str, e| {
      let trimmed = line.trim();
      if trimmed.is_empty() || (!trimmed.starts_with('#') && !trimmed.starts_with('^')) {
        return None;
      }
      let span: SimpleSpan = e.span();
      let offset = line.find(trimmed).unwrap_or(0);
      let start = span.start + offset;
      let end = start + trimmed.len();
      Some(ast::WithSpan::new(
        ast::Span::from_range(start, end),
        trimmed,
      ))
    })
    .filter(|value| value.is_some())
    .map(|value| value.unwrap())
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

fn looks_like_transaction_header(line: &str) -> bool {
  let trimmed = line.trim_start();
  if trimmed.is_empty() {
    return false;
  }
  let mut parts = trimmed.split_whitespace();
  let date = parts.next().unwrap_or("");
  let flag = parts.next().unwrap_or("");
  let mut chars = flag.chars();
  let first = chars.next();
  let is_single_flag = first.is_some() && chars.next().is_none() && !first.unwrap().is_ascii_digit();
  looks_like_date(date) && (flag == "txn" || is_single_flag)
}

fn parse_transaction_header_line<'a>(line: &'a str, line_start: usize) -> TransactionHeader<'a> {
  let trimmed = line.trim_start();
  let mut parts = trimmed.split_whitespace();
  let date = parts.next().unwrap_or("");
  let flag = parts.next().unwrap_or("");
  let after_flag = trimmed
    .find(flag)
    .map(|idx| &trimmed[idx + flag.len()..])
    .unwrap_or("");
  let remaining = after_flag;
  let (payee, narration, trailing) =
    if let Some((first_token, rest)) = parse_quoted_token(line, remaining, line_start) {
      if let Some((second_token, trailing)) = parse_quoted_token(line, rest, line_start) {
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
    let start = line.find(content).map(|pos| line_start + pos)?;
    let end = start + content.len();
    Some(ast::WithSpan::new(
      ast::Span::from_range(start, end),
      content,
    ))
  });

  TransactionHeader {
    date: span_for_token(line, line_start, date),
    flag: span_for_token(line, line_start, flag),
    payee,
    narration,
    tags_links: inline_tags_links,
  }
}

fn parse_quoted_token<'a>(
  line: &'a str,
  remaining: &'a str,
  line_start: usize,
) -> Option<(ast::WithSpan<&'a str>, &'a str)> {
  let s = remaining.trim_start();
  if !s.starts_with('"') {
    return None;
  }
  let end = s[1..].find('"')? + 2;
  let token = &s[..end];
  let base_ptr = line.as_ptr() as usize;
  let token_ptr = token.as_ptr() as usize;
  let start = if token_ptr >= base_ptr && token_ptr + token.len() <= base_ptr + line.len() {
    line_start + (token_ptr - base_ptr)
  } else {
    let offset = line.find(token).unwrap_or(0);
    line_start + offset
  };
  let span = ast::Span::from_range(start, start + token.len());
  Some((ast::WithSpan::new(span, token), &s[end..]))
}

fn span_for_token<'a>(line: &'a str, line_start: usize, token: &'a str) -> ast::WithSpan<&'a str> {
  if token.is_empty() {
    return ast::WithSpan::new(ast::Span::from_range(line_start, line_start), token);
  }
  let base_ptr = line.as_ptr() as usize;
  let token_ptr = token.as_ptr() as usize;
  let start = if token_ptr >= base_ptr && token_ptr + token.len() <= base_ptr + line.len() {
    line_start + (token_ptr - base_ptr)
  } else {
    let rel = line.find(token).unwrap_or(0);
    line_start + rel
  };
  let end = start + token.len();
  ast::WithSpan::new(ast::Span::from_range(start, end), token)
}
