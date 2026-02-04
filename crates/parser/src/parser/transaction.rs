use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::utils::{looks_like_currency, parse_tags_links, split_tags_links_group};
use crate::{Error, ast};

use super::common::{
  currency_token_parser, date_parser, indented_key_value_parser, inline_comment_parser, line_end,
  not_eol_parser, quoted_string_parser, rest_trimmed_parser, spanned_token_parser, ws0_parser,
  ws1_parser,
};
use super::number::number_expr_parser;

#[derive(Debug, Clone)]
struct TransactionHeader<'a> {
  date: ast::WithSpan<&'a str>,
  flag: ast::WithSpan<&'a str>,
  payee: Option<ast::WithSpan<&'a str>>,
  narration: Option<ast::WithSpan<&'a str>>,
  tags_links: Option<Vec<ast::WithSpan<&'a str>>>,
}

#[derive(Debug, Clone)]
enum TxnBodyLine<'a> {
  Posting(ast::Posting<'a>),
  KeyValue(ast::KeyValue<'a>),
}

pub(super) fn transaction_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
  let body_line = choice((posting_line_parser(), transaction_key_value_line_parser()));

  // Parse one or more body lines when present; allow an empty body only if no input is consumed.
  let body = body_line
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .or_not()
    .map(|opt| opt.unwrap_or_default());

  transaction_header_parser()
    .then_ignore(ws0_parser())
    .then_ignore(line_end())
    .then(body)
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
    let mut chars = value.content.chars();
    match (chars.next(), chars.next()) {
      (Some(ch), None) => "!&?%PSTCURM*#".contains(ch),
      (None, _) => false,
      _ => value.content == "txn",
    }
  });

  // Optional quoted strings: two => payee + narration, one => narration only.
  // Consume leading whitespace only if a quote follows; otherwise allow trailing spaces after the flag.
  let quoted_after_ws = ws1_parser()
    .then_ignore(just('"').rewind())
    .ignore_then(quoted_string_parser());

  let payee_narration = quoted_after_ws
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .or_not();

  // Optional inline tags/links (stop at inline comment ';').
  let inline_tags_links = ws1_parser()
    .ignore_then(rest_trimmed_parser())
    .filter(|rest| rest.content.starts_with('#') || rest.content.starts_with('^'))
    .map(|group| split_tags_links_group(group))
    .or_not();

  let inline_comment = ws0_parser().ignore_then(inline_comment_parser()).or_not();

  date_parser()
    .then_ignore(ws1_parser())
    .then(flag)
    .then(payee_narration)
    .then(inline_tags_links)
    .then(inline_comment)
    .map_with(
      |((((date, flag), payee_narration), tags_links), comment), e| {
        let span: SimpleSpan = e.span();
        let span = ast::Span::from_range(span.start, span.end);

        let (payee, narration) = match payee_narration {
          Some((payee, Some(narration))) => (Some(payee), Some(narration)),
          Some((narration, None)) => (None, Some(narration)),
          None => (None, None),
        };

        ast::Transaction {
          span,
          date,
          txn: Some(flag),
          payee,
          narration,
          tags_links,
          comment,
          tags: SmallVec::new(),
          links: SmallVec::new(),
          tags_links_lines: SmallVec::new(),
          comments: SmallVec::new(),
          key_values: SmallVec::new(),
          postings: SmallVec::new(),
        }
      },
    )
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
    .filter(|kv| super::common::is_key_token(kv.key.content))
    .map_with(move |mut kv, e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      kv.span = span;
      TxnBodyLine::KeyValue(kv)
    })
}

fn finalize_transaction<'a>(
  mut txn: ast::Transaction<'a>,
  body: Vec<TxnBodyLine<'a>>,
  span: ast::Span,
) -> ast::Transaction<'a> {
  let tags_links_lines: SmallVec<[ast::WithSpan<&'a str>; 8]> = SmallVec::new();
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
    }
  }

  txn.span = span;
  txn.postings = postings;
  txn.key_values = key_values;
  txn.comments = SmallVec::new();

  let mut tags_links_lines = tags_links_lines;
  if let Some(inline_groups) = txn.tags_links.clone() {
    for inline in inline_groups.into_iter().rev() {
      tags_links_lines.insert(0, inline);
    }
  }

  let tags_links = txn.tags_links.clone().or_else(|| {
    (!tags_links_lines.is_empty()).then(|| tags_links_lines.iter().cloned().collect::<Vec<_>>())
  });

  let (tags, links) = parse_tags_links(tags_links_lines.clone());
  txn.tags = tags;
  txn.links = links;
  txn.tags_links_lines = tags_links_lines;
  txn.tags_links = tags_links;
  txn.comment = txn
    .comment
    .clone()
    .or_else(|| txn.comments.first().cloned());

  txn
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

  let account = super::common::bare_string_parser().filter(|value| {
    value.content.contains(':')
      && !value.content.ends_with(':')
      && value
        .content
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_uppercase())
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
    .ignore_then(not_eol_parser().repeated().to_slice())
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

  let amount_total_no_currency = number_expr.clone().map(|total| ast::CostAmount {
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
