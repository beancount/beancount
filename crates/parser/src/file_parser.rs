//! 基于单个巨大 chumsky 组合的整文件解析器（file_parser.rs）。
//!
//! 这个模块用一个“巨大的” Chumsky 解析器一次性解析整个文件，
//! 子解析器直接构建 `ast::Directive` 并在本模块内补齐 meta。
//! 入口为 `parse_str_chumsky`。
use chumsky::prelude::*;
use ropey::Rope;
use smallvec::SmallVec;
use std::sync::Arc;

use crate::utils::{looks_like_currency, looks_like_date, parse_tags_links, split_currencies};
use crate::{Error, ParseError, ast};

#[derive(Debug, Clone)]
struct ParseCtx {
  filename: Arc<String>,
  rope: Rope,
}

#[derive(Debug, Clone)]
struct MetaAt {
  filename: Arc<String>,
  rope: Rope,
}

impl MetaAt {
  fn at(&self, offset: usize) -> ast::Meta {
    meta_from_rope(&self.filename, &self.rope, offset)
  }
}

impl From<&ParseCtx> for MetaAt {
  fn from(ctx: &ParseCtx) -> Self {
    MetaAt {
      filename: ctx.filename.clone(),
      rope: ctx.rope.clone(),
    }
  }
}

fn key_value_block_parser<'src>()
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

#[derive(Debug, Clone)]
enum TxnBodyLine<'a> {
  Posting(ast::Posting<'a>),
  KeyValue(ast::KeyValue<'a>),
  TagsLinks(ast::WithSpan<&'a str>),
  Comment(ast::WithSpan<&'a str>),
}

fn posting_line_parser<'src>() -> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src
{
  indented_posting_parser()
    .then_ignore(line_end())
    .map_with(move |mut posting, e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      posting.span = span;
      TxnBodyLine::Posting(posting)
    })
}

fn transaction_key_value_line_parser<'src>()
-> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src {
  indented_key_value_parser()
    .then_ignore(line_end())
    .filter(|kv| is_key_token(kv.key.content))
    .map_with(move |mut kv, e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      kv.span = span;
      TxnBodyLine::KeyValue(kv)
    })
}

fn transaction_tags_links_line_parser<'src>()
-> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src {
  tags_links_line_parser()
    .then_ignore(line_end())
    .map(TxnBodyLine::TagsLinks)
}

fn transaction_comment_line_parser<'src>()
-> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src {
  comment_directive_parser()
    .then_ignore(line_end())
    .map(|directive| {
      let ast::Directive::Comment(ast::Comment { text, .. }) = directive else {
        unreachable!("comment_directive_parser only builds comments");
      };
      TxnBodyLine::Comment(text)
    })
}

fn transaction_body_line_parser<'src>()
-> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src {
  choice((
    posting_line_parser(),
    transaction_key_value_line_parser(),
    transaction_tags_links_line_parser(),
    transaction_comment_line_parser(),
  ))
}

fn finalize_transaction<'a>(
  mut txn: ast::Transaction<'a>,
  body: Vec<TxnBodyLine<'a>>,
  span: ast::Span,
) -> ast::Transaction<'a> {
  let mut tags_links_lines: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
  let mut comments: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
  let mut key_values: SmallVec<[ast::KeyValue<'a>; 4]> = SmallVec::new();
  let mut postings: SmallVec<[ast::Posting<'a>; 4]> = SmallVec::new();

  for line in body {
    match line {
      TxnBodyLine::Posting(posting) => postings.push(posting),
      TxnBodyLine::KeyValue(kv) => {
        if let Some(last) = postings.last_mut() {
          last.key_values.push(kv);
        } else {
          key_values.push(kv);
        }
      }
      TxnBodyLine::TagsLinks(tags_links) => tags_links_lines.push(tags_links),
      TxnBodyLine::Comment(comment) => comments.push(comment),
    }
  }

  txn.span = span;
  txn.postings = postings;
  txn.key_values = key_values;
  txn.comments = comments;

  let mut tags_links_lines = tags_links_lines;
  if let Some(inline) = txn.tags_links.clone() {
    tags_links_lines.insert(0, inline);
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

  txn
}

fn transaction_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
  transaction_header_parser()
    .then_ignore(line_end())
    .then(
      transaction_body_line_parser()
        .repeated()
        .collect::<Vec<_>>(),
    )
    .map_with(move |(directive, body), e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      ast::Directive::Transaction(finalize_transaction(directive, body, span))
    })
    .boxed()
}

fn raw_block_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src
{
  let raw_line = any()
    .filter(|c: &char| *c != '\n')
    .repeated()
    .at_least(1)
    .to_slice();

  raw_line
    .then(line_end().ignore_then(raw_line).repeated())
    .then(just('\n').ignored().or_not())
    .map_with(move |((_, _), trailing_nl), e| {
      // Build from the contiguous slice; exclude an optional trailing newline.
      let span: SimpleSpan = e.span();
      let consumed_extra = trailing_nl.map(|_| 1).unwrap_or(0);
      let end = span.end.saturating_sub(consumed_extra);
      let span = ast::Span::from_range(span.start, end);

      let consumed = e.slice();
      let text = &consumed[..consumed.len().saturating_sub(consumed_extra)];

      ast::Directive::Raw(ast::Raw { span, text })
    })
    .boxed()
}

fn skipped_line_parser<'src>()
-> impl Parser<'src, &'src str, Option<Box<ast::Directive<'src>>>, Error<'src>> {
  choice((
    ws0_parser().then_ignore(just('\n')).to(None),
    ws1_parser().then_ignore(end()).to(None),
  ))
}

fn directive_parser<'src>()
-> impl Parser<'src, &'src str, Box<ast::Directive<'src>>, Error<'src>> + 'src {
  choice((
    include_directive_parser().then_ignore(line_end()),
    plugin_directive_parser().then_ignore(line_end()),
    option_directive_parser().then_ignore(line_end()),
    pushtag_directive_parser().then_ignore(line_end()),
    poptag_directive_parser().then_ignore(line_end()),
    pushmeta_directive_parser().then_ignore(line_end()),
    popmeta_directive_parser().then_ignore(line_end()),
    comment_directive_parser().then_ignore(line_end()),
    headline_directive_parser().then_ignore(line_end()),
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
    transaction_directive_parser(),
    raw_block_parser(),
  ))
  .map(Box::new)
  .boxed()
}

fn declarations_parser<'src>()
-> impl Parser<'src, &'src str, Vec<Option<Box<ast::Directive<'src>>>>, Error<'src>> + 'src {
  choice((directive_parser().map(Some), skipped_line_parser()))
    .repeated()
    .collect::<Vec<_>>()
    .boxed()
}

pub fn parse_str_chumsky<'a>(
  source: &'a str,
  filename: &str,
) -> std::result::Result<Vec<ast::Directive<'a>>, ParseError> {
  let ctx = ParseCtx {
    filename: Arc::new(filename.to_owned()),
    rope: Rope::from_str(source),
  };
  let meta_at = MetaAt::from(&ctx);

  let directives = declarations_parser()
    .then_ignore(end())
    .parse(source)
    .into_result()
    .map_err(|mut errors| match errors.pop() {
      Some(err) => {
        let span = err.span();
        let meta = meta_at.at(span.start);
        ParseError {
          line: meta.line,
          column: meta.column,
          message: err.to_string(),
        }
      }
      None => ParseError {
        line: 1,
        column: 1,
        message: "parse error".to_string(),
      },
    })?;

  let directives: Vec<_> = directives
    .into_iter()
    .flatten()
    .map(|directive| *directive)
    .collect();

  Ok(directives)
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

fn plugin_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  keyword_span_parser("plugin")
    .then_ignore(ws1_parser())
    .then(quoted_string_parser())
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .map_with(|((keyword, name), config), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::Plugin(ast::Plugin {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        name,
        config,
        comment: None,
      })
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
    .map_with(|(keyword, tag), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::PushTag(ast::TagDirective {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        tag,
        comment: None,
      })
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
    .map_with(|(keyword, tag), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::PopTag(ast::TagDirective {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        tag,
        comment: None,
      })
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
    .map_with(|(keyword, key), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::PopMeta(ast::PopMeta {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        key,
        comment: None,
      })
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
    .map_with(|((keyword, key), value), e| {
      let span: SimpleSpan = e.span();
      ast::Directive::Option(ast::OptionDirective {
        span: ast::Span::from_range(span.start, span.end),
        keyword,
        key,
        value,
        comment: None,
      })
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
  choice((just('\n').ignored(), end()))
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
      ast::Directive::Comment(ast::Comment {
        span: ast::Span::from_range(span.start, span.end),
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      })
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
      ast::Directive::Headline(ast::Headline {
        span: ast::Span::from_range(span.start, span.end),
        text: ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text),
      })
    })
}

fn open_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();
  let open_currency = bare_string_parser().filter(|value| !value.content.starts_with('"'));

  let header = date
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
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |(((((date, keyword), account), currencies), opt_booking), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let currencies = currencies.into_iter().flat_map(split_currencies).collect();
        let key_values = key_values.unwrap_or_else(SmallVec::new);

        ast::Directive::Open(ast::Open {
          span,
          keyword,
          date,
          account,
          currencies,
          opt_booking,
          comment: None,
          key_values,
        })
      },
    )
}

fn close_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("close"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|(((date, keyword), account), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Close(ast::Close {
        span,
        keyword,
        date,
        account,
        comment: None,
        key_values,
      })
    })
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

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("balance"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |((((date, keyword), account), (amount, tolerance)), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let key_values = key_values.unwrap_or_else(SmallVec::new);

        ast::Directive::Balance(ast::Balance {
          span,
          keyword,
          date,
          account,
          amount,
          tolerance,
          comment: None,
          key_values,
        })
      },
    )
    .boxed()
}

fn pad_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("pad"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |((((date, keyword), account), from_account), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let key_values = key_values.unwrap_or_else(SmallVec::new);

        ast::Directive::Pad(ast::Pad {
          span,
          keyword,
          date,
          account,
          from_account,
          comment: None,
          key_values,
        })
      },
    )
    .boxed()
}

fn commodity_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("commodity"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|(((date, keyword), currency), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Commodity(ast::Commodity {
        span,
        keyword,
        date,
        currency,
        comment: None,
        key_values,
      })
    })
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

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("price"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(amount)
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), currency), amount), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Price(ast::Price {
        span,
        keyword,
        date,
        currency,
        amount,
        comment: None,
        key_values,
      })
    })
}

fn event_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();
  let event_string = choice((quoted_string_parser(), spanned_token_parser()));
  let event_string_tail = choice((quoted_string_parser(), spanned_token_parser()));

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("event"))
    .then_ignore(ws1_parser())
    .then(event_string)
    .then_ignore(ws1_parser())
    .then(event_string_tail)
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), event_type), desc), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Event(ast::Event {
        span,
        keyword,
        date,
        event_type,
        desc,
        comment: None,
        key_values,
      })
    })
}

fn query_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();
  let query_rest = rest_trimmed_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("query"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(query_rest)
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), name), query), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Query(ast::Query {
        span,
        keyword,
        date,
        name,
        query,
        comment: None,
        key_values,
      })
    })
}

fn note_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();
  let note = rest_trimmed_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("note"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(note)
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), account), note), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Note(ast::Note {
        span,
        keyword,
        date,
        account,
        note,
        comment: None,
        key_values,
      })
    })
}

fn document_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let date = date_parser();
  let document_filename = choice((quoted_string_parser(), spanned_token_parser()));
  let header_rest = rest_trimmed_parser();

  let header = date
    .then_ignore(ws1_parser())
    .then(keyword_span_parser("document"))
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then_ignore(ws1_parser())
    .then(document_filename)
    .then(ws1_parser().ignore_then(header_rest).or_not())
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(
      |(((((date, keyword), account), filename), tags_links), key_values), e| {
        let span = ast::Span::from_simple_span(e.span());
        let key_values = key_values.unwrap_or_else(SmallVec::new);
        let (tags, links) = match tags_links.clone() {
          Some(value) => parse_tags_links([value]),
          None => (SmallVec::new(), SmallVec::new()),
        };

        ast::Directive::Document(ast::Document {
          span,
          keyword,
          date,
          account,
          filename,
          tags_links,
          tags,
          links,
          comment: None,
          key_values,
        })
      },
    )
}

fn custom_directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>>
{
  let date = date_parser();

  let header = date
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
    .then_ignore(ws0_parser());

  header
    .then_ignore(line_end())
    .then(key_value_block_parser().or_not())
    .map_with(|((((date, keyword), name), values), key_values), e| {
      let span = ast::Span::from_simple_span(e.span());
      let key_values = key_values.unwrap_or_else(SmallVec::new);

      ast::Directive::Custom(ast::Custom {
        span,
        keyword,
        date,
        name,
        values: values.into_iter().collect(),
        comment: None,
        key_values,
      })
    })
}

#[derive(Debug, Clone)]
struct TransactionHeader<'a> {
  date: ast::WithSpan<&'a str>,
  flag: ast::WithSpan<&'a str>,
  payee: Option<ast::WithSpan<&'a str>>,
  narration: Option<ast::WithSpan<&'a str>>,
  tags_links: Option<ast::WithSpan<&'a str>>,
}

fn transaction_header_parser<'src>()
-> impl Parser<'src, &'src str, ast::Transaction<'src>, Error<'src>> {
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
      ast::Transaction {
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
      }
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

  let op_token = choice((
    just('*').to(ast::BinaryOp::Mul),
    just('/').to(ast::BinaryOp::Div),
    just('+').to(ast::BinaryOp::Add),
    just('-').to(ast::BinaryOp::Sub),
  ))
  .map_with(|op, e| {
    let span: SimpleSpan = e.span();
    ast::WithSpan::new(ast::Span::from_range(span.start, span.end), op)
  })
  .boxed();

  let op_token_sp = ws0.clone().ignore_then(op_token).then_ignore(ws0.clone());

  literal
    .clone()
    .then(op_token_sp.then(literal.clone()).repeated().collect::<Vec<_>>())
    .map(
      |(first, rest): (
        ast::NumberExpr<'src>,
        Vec<(ast::WithSpan<ast::BinaryOp>, ast::NumberExpr<'src>)>,
      )| build_number_expr(first, rest),
    )
    .boxed()
}

fn build_number_expr<'a>(
  first: ast::NumberExpr<'a>,
  rest: Vec<(ast::WithSpan<ast::BinaryOp>, ast::NumberExpr<'a>)>,
) -> ast::NumberExpr<'a> {
  // Hand-written shunting-yard style to avoid deep recursion when expressions are long.
  let mut values: Vec<ast::NumberExpr<'a>> = Vec::with_capacity(rest.len() + 1);
  let mut ops: Vec<ast::WithSpan<ast::BinaryOp>> = Vec::with_capacity(rest.len());

  values.push(first);
  for (op, rhs) in rest {
    while let Some(prev_op) = ops.last() {
      if precedence(prev_op.content) >= precedence(op.content) {
        let op_top = ops.pop().expect("ops not empty");
        let right = values.pop().expect("value exists");
        let left = values.pop().expect("value exists");
        let span = ast::Span::from_range(left.span().start, right.span().end);
        values.push(ast::NumberExpr::Binary {
          span,
          left: Box::new(left),
          op: op_top,
          right: Box::new(right),
        });
      } else {
        break;
      }
    }
    ops.push(op);
    values.push(rhs);
  }

  while let Some(op) = ops.pop() {
    let right = values.pop().expect("value exists");
    let left = values.pop().expect("value exists");
    let span = ast::Span::from_range(left.span().start, right.span().end);
    values.push(ast::NumberExpr::Binary {
      span,
      left: Box::new(left),
      op,
      right: Box::new(right),
    });
  }

  values
    .pop()
    .expect("number expression requires at least one literal")
}

fn precedence(op: ast::BinaryOp) -> u8 {
  match op {
    ast::BinaryOp::Mul | ast::BinaryOp::Div => 2,
    ast::BinaryOp::Add | ast::BinaryOp::Sub => 1,
  }
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
