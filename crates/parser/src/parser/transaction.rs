use chumsky::prelude::*;
use smallvec::SmallVec;

use crate::parser::common::tags_links_line_parser;
use crate::utils::{looks_like_currency, parse_tags_links};
use crate::{Error, ast};

use super::common::{
  currency_token_parser, date_parser, directive_end_parser, indented_key_value_parser,
  inline_comment_parser, line_end, not_eol_parser, quoted_string_parser, spanned_token_parser,
  ws0_parser, ws1_parser,
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
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> {
  let body_line = choice((posting_line_parser(), transaction_key_value_line_parser()));
  let indented = choice((just(' '), just('\t')));

  // Require indentation but rewind so the body parsers see the full line, keeping spans intact.
  let body = indented
    .ignored()
    .rewind()
    .ignore_then(body_line)
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>();

  date_parser()
    .then_ignore(ws1_parser())
    .then(spanned_token_parser())
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .then(ws1_parser().ignore_then(quoted_string_parser()).or_not())
    .then(ws1_parser().ignore_then(tags_links_line_parser()).or_not())
    .then(ws0_parser().ignore_then(inline_comment_parser().or_not()))
    .map_with(
      |(((((date, flag), payee_or_narration1), payee_or_narration2), tags_links), comment), e| {
        let span: SimpleSpan = e.span();
        let span = ast::Span::from_range(span.start, span.end);

        // payee narration
        // narration
        let (payee, narration) = match (payee_or_narration1, payee_or_narration2) {
          (None, None) => (None, None),
          (None, Some(narration)) => (None, Some(narration)),
          (Some(narration), None) => (None, Some(narration)),
          (Some(payee), Some(narration)) => (Some(payee), Some(narration)),
        };

        let tags_links = tags_links;

        let (tags, links) = match tags_links.clone() {
          Some(line) => parse_tags_links(line),
          None => (SmallVec::new(), SmallVec::new()),
        };

        ast::Transaction {
          span,
          date,
          txn: Some(flag),
          payee: payee,
          narration: narration,
          tags_links,
          comment,
          tags,
          links,
          key_values: SmallVec::new(),
          postings: SmallVec::new(),
        }
      },
    )
    .then_ignore(line_end())
    .then(body.or_not())
    .then_ignore(directive_end_parser())
    .map_with(move |(directive, body), e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(span.start, span.end);
      ast::Directive::Transaction(finalize_transaction(
        directive,
        body.unwrap_or_default(),
        span,
      ))
    })
}

fn posting_line_parser<'src>() -> impl Parser<'src, &'src str, TxnBodyLine<'src>, Error<'src>> + 'src
{
  indented_posting_parser()
    .then_ignore(line_end())
    .map_with(move |mut posting, e| {
      let span: SimpleSpan = e.span();
      let span = ast::Span::from_range(posting.span.start, span.end);
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

  let indent = choice((just(' '), just('\t')))
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(|text: &str, e| {
      let span: SimpleSpan = e.span();
      (text, span)
    });

  let posting_body = optflag
    .or_not()
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
    .then(ws0_parser().ignore_then(comment).or_not());

  indent
    .then(posting_body)
    .map_with(|((_indent, indent_span), value), e| {
      let span: SimpleSpan = e.span();
      let start = indent_span.start;
      let (left, comment) = value;
      let ((((opt_flag, account), amount), cost_spec), price) = left;
      let (price_operator, price_annotation) = price;
      ast::Posting {
        span: ast::Span::from_range(start, span.end),
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

  let amount_total_no_currency = number_expr.clone().map(|total| ast::CostAmount {
    per: None,
    total: Some(total),
    currency: None,
  });

  let amount_total_with_currency = just('#')
    .ignore_then(ws0_parser())
    .ignore_then(number_expr.clone())
    .then(ws1_parser().ignore_then(currency))
    .map(|(total, currency)| ast::CostAmount {
      per: None,
      total: Some(total),
      currency: Some(currency),
    });

  let amount_per_and_total = number_expr
    .clone()
    .then_ignore(ws0_parser())
    .then_ignore(just('#'))
    .then_ignore(ws0_parser())
    .then(number_expr.clone())
    .then(ws1_parser().ignore_then(currency).or_not())
    .map(|((per, total), currency)| ast::CostAmount {
      per: Some(per),
      total: Some(total),
      currency,
    });

  let cost_amount = choice((
    amount_per_and_total,
    amount_total_with_currency,
    amount,
    amount_total_no_currency,
  ));

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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast;
  use chumsky::Parser;

  #[test]
  fn parses_transaction_header_with_tags_and_links() {
    let src = r#"2014-01-01 * "Payee" "Narration" #tag ^link"#;

    let directive = transaction_directive_parser()
      .parse(src)
      .into_output()
      .expect("parser should succeed");

    let txn = match directive {
      ast::Directive::Transaction(txn) => txn,
      other => panic!("expected transaction directive, got {other:?}"),
    };

    assert_eq!(txn.date.content, "2014-01-01");
    assert_eq!(txn.txn.as_ref().map(|flag| flag.content), Some("*"));
    assert_eq!(
      txn.payee.as_ref().map(|payee| payee.content),
      Some("\"Payee\"")
    );
    assert_eq!(
      txn.narration.as_ref().map(|narration| narration.content),
      Some("\"Narration\""),
    );
    assert_eq!(
      txn.tags.iter().map(|tag| tag.content).collect::<Vec<_>>(),
      vec!["tag"],
    );
    assert_eq!(
      txn
        .links
        .iter()
        .map(|link| link.content)
        .collect::<Vec<_>>(),
      vec!["link"],
    );
  }

  #[test]
  fn parses_multiline_transaction_postings() {
    let src = [
      r#"2014-01-01 * "Payee" "Narr""#,
      r#"  Expenses:Food 10 USD"#,
      r#"  Assets:Cash -10 USD"#,
      r#""#,
    ]
    .join("\n");

    let directive = transaction_directive_parser()
      .then_ignore(end())
      .parse(&src)
      .into_result()
      .unwrap();

    let txn = match directive {
      ast::Directive::Transaction(txn) => txn,
      other => panic!("expected transaction directive, got {other:?}"),
    };

    assert_eq!(txn.postings.len(), 2);
    assert_eq!(txn.postings[0].account.content, "Expenses:Food");
    assert_eq!(txn.postings[1].account.content, "Assets:Cash");
  }
}
