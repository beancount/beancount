use chumsky::prelude::*;

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

  let literal = number_literal_parser()
    .map(ast::NumberExpr::Literal)
    .boxed();

  let op = |ch: char, op: ast::BinaryOp| {
    just(ch)
      .map_with(move |_, e| {
        let span: SimpleSpan = e.span();
        ast::WithSpan::new(ast::Span::from_range(span.start, span.end), op)
      })
      .boxed()
  };

  let op_mul_div =
    choice((op('*', ast::BinaryOp::Mul), op('/', ast::BinaryOp::Div))).boxed();
  let op_add_sub =
    choice((op('+', ast::BinaryOp::Add), op('-', ast::BinaryOp::Sub))).boxed();

  let op_mul_div_sp = ws0
    .clone()
    .ignore_then(op_mul_div)
    .then_ignore(ws0.clone())
    .boxed();
  let op_add_sub_sp = ws0
    .clone()
    .ignore_then(op_add_sub)
    .then_ignore(ws0.clone())
    .boxed();

  fn build_binary<'a>(
    left: ast::NumberExpr<'a>,
    op: ast::WithSpan<ast::BinaryOp>,
    right: ast::NumberExpr<'a>,
  ) -> ast::NumberExpr<'a> {
    let span = ast::Span::from_range(left.span().start, right.span().end);
    ast::NumberExpr::Binary {
      span,
      left: Box::new(left),
      op,
      right: Box::new(right),
    }
  }

  fn apply_prefix<'a>(
    ops: Vec<ast::WithSpan<char>>,
    mut expr: ast::NumberExpr<'a>,
  ) -> ast::NumberExpr<'a> {
    // Apply prefixes right-to-left.
    for op in ops.into_iter().rev() {
      match op.content {
        '+' => {
          // no-op
        }
        '-' => {
          let zero = ast::NumberExpr::Literal(ast::WithSpan::new(op.span, "0"));
          let sub = ast::WithSpan::new(op.span, ast::BinaryOp::Sub);
          let span = ast::Span::from_range(op.span.start, expr.span().end);
          expr = ast::NumberExpr::Binary {
            span,
            left: Box::new(zero),
            op: sub,
            right: Box::new(expr),
          };
        }
        _ => unreachable!("unexpected prefix operator"),
      }
    }
    expr
  }

  recursive(|expr| {
    // Only consume whitespace *inside* parentheses, not after ')'. Callers may
    // need the post-paren whitespace (e.g. amount + currency parsing).
    let lparen = just('(').then_ignore(ws0.clone()).boxed();
    let rparen = ws0.clone().ignore_then(just(')')).boxed();

    let paren = lparen.ignore_then(expr.clone()).then_ignore(rparen).boxed();

    let primary = choice((literal.clone(), paren)).boxed();

    // Important: parse signed literals as literals first. Only fall back to
    // prefix operators when the literal parser can't consume the leading sign
    // (e.g. "-(1-2)").
    let prefix_op = choice((just('+'), just('-')))
      .map_with(|op_ch, e| {
        let span: SimpleSpan = e.span();
        ast::WithSpan::new(ast::Span::from_range(span.start, span.end), op_ch)
      })
      .then_ignore(ws0.clone())
      .repeated()
      .at_least(1)
      .collect::<Vec<_>>()
      .boxed();

    let unary = primary
      .clone()
      .or(
        prefix_op
          .then(primary.clone())
          .map(|(ops, value)| apply_prefix(ops, value)),
      )
      .boxed();

    let product = unary
      .clone()
      .foldl(
        op_mul_div_sp.then(unary.clone()).repeated(),
        |left, (op, right)| build_binary(left, op, right),
      )
      .boxed();

    product
      .clone()
      .foldl(
        op_add_sub_sp.then(product).repeated(),
        |left, (op, right)| build_binary(left, op, right),
      )
      .boxed()
  })
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

  #[test]
  fn parses_parentheses() {
    let src = "1 * (2 + 3)";

    let expr = number_expr_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let (mul_op, mul_left, mul_right) = match expr {
      ast::NumberExpr::Binary {
        op, left, right, ..
      } => (op, left, right),
      other => panic!("expected multiply expression, got {other:?}"),
    };
    assert_eq!(mul_op.content, ast::BinaryOp::Mul);

    let left_literal = match *mul_left {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected left literal, got {other:?}"),
    };
    assert_eq!(left_literal.content, "1");

    let (add_op, add_left, add_right) = match *mul_right {
      ast::NumberExpr::Binary {
        op, left, right, ..
      } => (op, left, right),
      other => panic!("expected addition inside parens, got {other:?}"),
    };
    assert_eq!(add_op.content, ast::BinaryOp::Add);

    let add_left_lit = match *add_left {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected left literal, got {other:?}"),
    };
    assert_eq!(add_left_lit.content, "2");

    let add_right_lit = match *add_right {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected right literal, got {other:?}"),
    };
    assert_eq!(add_right_lit.content, "3");
  }

  #[test]
  fn parses_unary_minus_before_parentheses() {
    let src = "-(1-2)";

    let expr = number_expr_parser()
      .then_ignore(end())
      .parse(src)
      .into_result()
      .unwrap();

    let (top_op, top_left, top_right) = match expr {
      ast::NumberExpr::Binary {
        op, left, right, ..
      } => (op, left, right),
      other => panic!("expected top-level binary, got {other:?}"),
    };

    assert_eq!(top_op.content, ast::BinaryOp::Sub);

    let zero = match *top_left {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected injected zero literal, got {other:?}"),
    };
    assert_eq!(zero.content, "0");

    let (inner_op, inner_left, inner_right) = match *top_right {
      ast::NumberExpr::Binary {
        op, left, right, ..
      } => (op, left, right),
      other => panic!("expected inner subtraction, got {other:?}"),
    };
    assert_eq!(inner_op.content, ast::BinaryOp::Sub);

    let one = match *inner_left {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected left literal, got {other:?}"),
    };
    assert_eq!(one.content, "1");

    let two = match *inner_right {
      ast::NumberExpr::Literal(value) => value,
      other => panic!("expected right literal, got {other:?}"),
    };
    assert_eq!(two.content, "2");
  }
}
