use chumsky::prelude::*;

#[cfg(feature = "rich-errors")]
use chumsky::error::Rich;

#[cfg(not(feature = "rich-errors"))]
type ParserError<'src> = Simple<'src, char>;

#[cfg(feature = "rich-errors")]
type ParserError<'src> = Rich<'src, char>;

use crate::{Error, ast};

use super::common::ws0_parser;

pub(super) fn number_literal_parser<'src>()
-> impl Parser<'src, &'src str, ast::WithSpan<&'src str>, Error<'src>> {
  let sign = just('+').or(just('-')).then(ws0_parser()).or_not();
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

  let literal = number_literal_parser().boxed();

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
    .map_with(|_, e| {
      let span: SimpleSpan = e.span();
      (e.slice(), span.start)
    })
    .try_map(|(text, offset), _span| parse_number_expr_raw(text, offset))
    .boxed()
}

fn pop_value<'a>(values: &mut Vec<ast::NumberExpr<'a>>) -> ast::NumberExpr<'a> {
  values
    .pop()
    .unwrap_or_else(|| unreachable!("number expression stack underflow"))
}

fn pop_op(ops: &mut Vec<ast::WithSpan<ast::BinaryOp>>) -> ast::WithSpan<ast::BinaryOp> {
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

fn parse_number_expr_raw<'src>(
  raw: &'src str,
  offset: usize,
) -> Result<ast::NumberExpr<'src>, ParserError<'src>> {
  let mut idx = 0;
  let len = raw.len();

  let mut rest: Vec<(ast::WithSpan<ast::BinaryOp>, ast::NumberExpr<'src>)> = Vec::new();

  let parse_literal =
    |idx: &mut usize| -> Result<ast::NumberExpr<'src>, ParserError<'src>> {
      skip_ws(raw, idx);
      let lit_start = *idx;

      if *idx >= len {
        return Err(simple_error(offset + *idx, "expected number literal"));
      }

      if matches!(raw.as_bytes().get(*idx), Some(b'+') | Some(b'-')) {
        *idx += 1;
        skip_ws(raw, idx);
      }

      let digits_start = *idx;
      while let Some(b) = raw.as_bytes().get(*idx) {
        if b.is_ascii_digit() || *b == b',' {
          *idx += 1;
        } else {
          break;
        }
      }

      if digits_start == *idx {
        return Err(simple_error(offset + *idx, "expected digits"));
      }

      if matches!(raw.as_bytes().get(*idx), Some(b'.')) {
        let dot_pos = *idx;
        *idx += 1;

        let frac_start = *idx;
        while let Some(b) = raw.as_bytes().get(*idx) {
          if b.is_ascii_digit() || *b == b',' {
            *idx += 1;
          } else {
            break;
          }
        }

        if frac_start == *idx {
          return Err(simple_error(offset + dot_pos, "expected digits after '.'"));
        }
      }

      let lit_end = *idx;
      let span = ast::Span::from_range(offset + lit_start, offset + lit_end);
      Ok(ast::NumberExpr::Literal(ast::WithSpan::new(
        span,
        &raw[lit_start..lit_end],
      )))
    };

  let first = parse_literal(&mut idx)?;

  loop {
    skip_ws(raw, &mut idx);
    if idx >= len {
      break;
    }

    let op_char = match raw.as_bytes().get(idx) {
      Some(b @ (b'+' | b'-' | b'*' | b'/')) => *b as char,
      _ => break,
    };
    let op_span = ast::Span::from_range(offset + idx, offset + idx + 1);
    let op = match op_char {
      '+' => ast::BinaryOp::Add,
      '-' => ast::BinaryOp::Sub,
      '*' => ast::BinaryOp::Mul,
      '/' => ast::BinaryOp::Div,
      _ => unreachable!(),
    };
    idx += 1;

    let rhs = parse_literal(&mut idx)?;
    rest.push((ast::WithSpan::new(op_span, op), rhs));
  }

  Ok(build_number_expr(first, rest))
}

fn skip_ws(raw: &str, idx: &mut usize) {
  while let Some(b) = raw.as_bytes().get(*idx) {
    if matches!(*b, b' ' | b'\t') {
      *idx += 1;
    } else {
      break;
    }
  }
}

#[cfg(not(feature = "rich-errors"))]
fn simple_error<'src>(pos: usize, _msg: &'static str) -> ParserError<'src> {
  Simple::new(None, SimpleSpan::new((), pos..pos + 1))
}

#[cfg(feature = "rich-errors")]
fn simple_error<'src>(pos: usize, msg: &'static str) -> ParserError<'src> {
  Rich::custom(SimpleSpan::new((), pos..pos + 1), msg)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast;
  use chumsky::Parser;

  #[test]
  fn parses_single_literal() {
    let src = "123.45";

    let expr = number_expr_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let literal = match expr {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected literal, got {other:?}"),
    };

    assert_eq!(literal.content, src);
    assert_eq!(literal.span, ast::Span::from_range(0, src.len()));
  }

  #[test]
  fn parses_literal_with_space_after_sign() {
    let src = "- 227000";

    let expr = number_expr_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let literal = match expr {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected literal, got {other:?}"),
    };

    assert_eq!(literal.content, src);
    assert_eq!(literal.span, ast::Span::from_range(0, src.len()));
  }

  #[test]
  fn respects_operator_precedence() {
    let src = "1 + 2 * 3";

    let expr = number_expr_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let (add_span, add_left, add_op, add_right) = match expr {
      ast::NumberExpr::Binary {
        span,
        left,
        op,
        right,
      } => (span, left, op, right),
      other => panic!("expected binary add, got {other:?}"),
    };

    assert_eq!(add_op.content, ast::BinaryOp::Add);
    assert_eq!(add_span, ast::Span::from_range(0, src.len()));

    let left_literal = match *add_left {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected left literal, got {other:?}"),
    };
    assert_eq!(left_literal.content, "1");
    assert_eq!(left_literal.span, ast::Span::from_range(0, 1));

    let (mul_span, mul_left, mul_op, mul_right) = match *add_right {
      ast::NumberExpr::Binary {
        span,
        left,
        op,
        right,
      } => (span, left, op, right),
      other => panic!("expected multiply on the right, got {other:?}"),
    };

    assert_eq!(mul_op.content, ast::BinaryOp::Mul);
    assert_eq!(mul_span, ast::Span::from_range(4, 9));

    let left_two = match *mul_left {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected left literal 2, got {other:?}"),
    };
    assert_eq!(left_two.content, "2");
    assert_eq!(left_two.span, ast::Span::from_range(4, 5));

    let right_three = match *mul_right {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected right literal 3, got {other:?}"),
    };
    assert_eq!(right_three.content, "3");
    assert_eq!(right_three.span, ast::Span::from_range(8, 9));
  }

  #[test]
  fn subtraction_is_left_associative() {
    let src = "10 - 3 - 2";

    let expr = number_expr_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let top = match expr {
      ast::NumberExpr::Binary {
        op, left, right, ..
      } => (op, left, right),
      other => panic!("expected binary expression, got {other:?}"),
    };

    assert_eq!(top.0.content, ast::BinaryOp::Sub);

    let left_sub = match *top.1 {
      ast::NumberExpr::Binary {
        op, left, right, ..
      } => (op, left, right),
      other => panic!("expected left subtraction, got {other:?}"),
    };

    assert_eq!(left_sub.0.content, ast::BinaryOp::Sub);

    let left_ten = match *left_sub.1 {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected left literal 10, got {other:?}"),
    };
    assert_eq!(left_ten.content, "10");

    let right_three = match *left_sub.2 {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected middle literal 3, got {other:?}"),
    };
    assert_eq!(right_three.content, "3");

    let right_two = match *top.2 {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected right literal 2, got {other:?}"),
    };
    assert_eq!(right_two.content, "2");
  }
}
