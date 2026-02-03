use chumsky::prelude::*;

use crate::{Error, ast};

use super::common::ws0_parser;

pub(super) fn number_literal_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  let sign = just('+').or(just('-')).or_not();
  let digits = any()
    .filter(|c: &char| c.is_ascii_digit() || *c == ',')
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

pub(super) fn number_expr_parser<'src>()
-> impl Parser<'src, &'src str, ast::NumberExpr<'src>, Error<'src>> {
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
    .then(
      op_token_sp
        .then(literal.clone())
        .repeated()
        .collect::<Vec<_>>(),
    )
    .map(
      |(first, rest): (
        ast::NumberExpr<'src>,
        Vec<(ast::WithSpan<ast::BinaryOp>, ast::NumberExpr<'src>)>,
      )| build_number_expr(first, rest),
    )
    .boxed()
}

fn pop_value<'a>(values: &mut Vec<ast::NumberExpr<'a>>) -> ast::NumberExpr<'a> {
  values
    .pop()
    .unwrap_or_else(|| unreachable!("number expression stack underflow"))
}

fn pop_op(
  ops: &mut Vec<ast::WithSpan<ast::BinaryOp>>,
) -> ast::WithSpan<ast::BinaryOp> {
  ops
    .pop()
    .unwrap_or_else(|| unreachable!("operator stack should not be empty here"))
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
        let op_top = pop_op(&mut ops);
        let right = pop_value(&mut values);
        let left = pop_value(&mut values);
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
    let right = pop_value(&mut values);
    let left = pop_value(&mut values);
    let span = ast::Span::from_range(left.span().start, right.span().end);
    values.push(ast::NumberExpr::Binary {
      span,
      left: Box::new(left),
      op,
      right: Box::new(right),
    });
  }

  pop_value(&mut values)
}

fn precedence(op: ast::BinaryOp) -> u8 {
  match op {
    ast::BinaryOp::Mul | ast::BinaryOp::Div => 2,
    ast::BinaryOp::Add | ast::BinaryOp::Sub => 1,
  }
}
