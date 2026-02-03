use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::{ast, Error};
use crate::utils::{looks_like_currency, parse_tags_links};

use super::common::{currency_token_parser, date_parser, indented_key_value_parser, line_end, spanned_token_parser, tags_links_line_parser, ws0_parser, ws1_parser};
use super::number::number_expr_parser;

#[derive(Debug, Clone)]
struct TransactionHeader<'a> {
  date: ast::WithSpan<&'a str>,
  flag: ast::WithSpan<&'a str>,
  payee: Option<ast::WithSpan<&'a str>>,
  narration: Option<ast::WithSpan<&'a str>>,
  tags_links: Option<ast::WithSpan<&'a str>>,
}

#[derive(Debug, Clone)]
enum TxnBodyLine<'a> {
  Posting(ast::Posting<'a>),
  KeyValue(ast::KeyValue<'a>),
  TagsLinks(ast::WithSpan<&'a str>),
  Comment(ast::WithSpan<&'a str>),
}

pub(super) fn transaction_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
  transaction_header_parser()
    .then_ignore(line_end())
    .then(transaction_body_line_parser().repeated().collect::<Vec<_>>())
    .map_with(move |(directive, body), e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      ast::Directive::Transaction(finalize_transaction(directive, body, span))
    })
    .boxed()
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

fn transaction_body_line_parser<'src>()
-> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src {
  choice((
    posting_line_parser(),
    transaction_key_value_line_parser(),
    transaction_tags_links_line_parser(),
    transaction_comment_line_parser(),
  ))
}

fn posting_line_parser<'src>() -> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src {
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
    .filter(|kv| super::common::is_key_token(kv.key.content))
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
  super::comment::comment_directive_parser()
    .then_ignore(line_end())
    .map(|directive| {
      let ast::Directive::Comment(ast::Comment { text, .. }) = directive else {
        unreachable!("comment_directive_parser only builds comments");
      };
      TxnBodyLine::Comment(text)
    })
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

fn indented_posting_parser<'src>() -> impl Parser<'src, &'src str, ast::Posting<'src>, Error<'src>> {
  let optflag = any()
    .filter(|c: &char| "*!#&?%PSTCURM".contains(*c))
    .repeated()
    .exactly(1)
    .to_slice()
    .map_with(|value: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), value)
    });

  let account = super::common::bare_string_parser().filter(|value| {
    value.content.contains(':')
      && !value.content.ends_with(':')
      && value.content.chars().next().is_some_and(|c| c.is_ascii_uppercase())
  });

  let currency_for_amount = currency_token_parser();

  let amount = number_expr_parser()
    .then(ws1_parser().ignore_then(currency_for_amount).or_not())
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
    super::common::bare_string_parser()
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
    .ignore_then(any().filter(|c: &char| *c != '\n').repeated().to_slice())
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      ast::WithSpan::new(ast::Span::from_range(span.start, span.end), text)
    });

  ws1_parser()
    .ignore_then(optflag.or_not())
    .then(ws0_parser().ignore_then(account))
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

  let amount_total_no_currency = number_expr
    .clone()
    .map(|total| ast::CostAmount {
      per: None,
      total: Some(total),
      currency: None,
    });

  let cost_amount = choice((amount_total, amount, amount_total_no_currency));

  let date = any()
    .filter(|c: &char| c.is_ascii_digit())
    .repeated()
    .exactly(4)
    .then_ignore(just('-'))
    .then(any().filter(|c: &char| c.is_ascii_digit()).repeated().exactly(2))
    .then_ignore(just('-'))
    .then(any().filter(|c: &char| c.is_ascii_digit()).repeated().exactly(2))
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
