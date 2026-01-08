use beancount_parser::core::{BinaryOp, CoreDirective, NumberExpr};
use beancount_parser::{normalize_directives, parse_str};
use tree_sitter::Parser;

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn lines(parts: &[&str]) -> String {
  parts.join("\n")
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn parse_core(input: &str, filename: &str) -> Vec<CoreDirective> {
  let ast = parse_str(input, filename).expect("parse failed");
  normalize_directives(ast).expect("normalize failed")
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn parse_core_allow_all_raw(input: &str, filename: &str) -> Vec<CoreDirective> {
  let mut parser = Parser::new();
  parser
    .set_language(&beancount_tree_sitter::language())
    .expect("load grammar");
  let tree = parser.parse(input, None).expect("parse tree");
  let root = tree.root_node();
  let ast = beancount_parser::parse::parse_directives(root, input, filename.to_owned())
    .expect("parse directives");
  normalize_directives(ast).expect("normalize failed")
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn collect_ops(expr: &NumberExpr) -> [bool; 4] {
  let mut seen = [false; 4];
  fn walk(expr: &NumberExpr, seen: &mut [bool; 4]) {
    if let NumberExpr::Binary { left, op, right } = expr {
      match op {
        BinaryOp::Add => seen[0] = true,
        BinaryOp::Sub => seen[1] = true,
        BinaryOp::Mul => seen[2] = true,
        BinaryOp::Div => seen[3] = true,
      }
      walk(left, seen);
      walk(right, seen);
    }
  }
  walk(expr, &mut seen);
  seen
}
