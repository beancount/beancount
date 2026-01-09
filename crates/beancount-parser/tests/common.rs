use beancount_parser::core::{BinaryOp, CoreDirective, NumberExpr};
use beancount_parser::{ast, normalize_directives, parse_str};

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
pub(crate) trait FromCore: Sized {
  fn from_core(dir: CoreDirective) -> Option<Self>;
}

macro_rules! impl_from_core {
  ($ty:ty, $variant:ident) => {
    impl FromCore for $ty {
      fn from_core(dir: CoreDirective) -> Option<Self> {
        match dir {
          CoreDirective::$variant(v) => Some(v),
          _ => None,
        }
      }
    }
  };
}

impl_from_core!(beancount_parser::core::Open, Open);
impl_from_core!(beancount_parser::core::Close, Close);
impl_from_core!(beancount_parser::core::Balance, Balance);
impl_from_core!(beancount_parser::core::Pad, Pad);
impl_from_core!(beancount_parser::core::Transaction, Transaction);
impl_from_core!(beancount_parser::core::Commodity, Commodity);
impl_from_core!(beancount_parser::core::Price, Price);
impl_from_core!(beancount_parser::core::Event, Event);
impl_from_core!(beancount_parser::core::Query, Query);
impl_from_core!(beancount_parser::core::Note, Note);
impl_from_core!(beancount_parser::core::Document, Document);
impl_from_core!(beancount_parser::core::Custom, Custom);
impl_from_core!(beancount_parser::core::OptionDirective, Option);
impl_from_core!(beancount_parser::core::Include, Include);
impl_from_core!(beancount_parser::core::Plugin, Plugin);
impl_from_core!(beancount_parser::core::PushMeta, Pushmeta);
impl_from_core!(beancount_parser::core::PopMeta, Popmeta);
impl_from_core!(beancount_parser::core::Comment, Comment);

#[cfg(test)]
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PushtagDir(pub beancount_parser::core::TagDirective);

#[cfg(test)]
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PoptagDir(pub beancount_parser::core::TagDirective);

impl FromCore for PushtagDir {
  fn from_core(dir: CoreDirective) -> Option<Self> {
    match dir {
      CoreDirective::Pushtag(v) => Some(Self(v)),
      _ => None,
    }
  }
}

impl FromCore for PoptagDir {
  fn from_core(dir: CoreDirective) -> Option<Self> {
    match dir {
      CoreDirective::Poptag(v) => Some(Self(v)),
      _ => None,
    }
  }
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn parse_as<T: FromCore>(input: &str, filename: &str) -> T {
  let directives = parse_core(input, filename);
  assert_eq!(directives.len(), 1, "expected a single directive");
  let directive = directives.into_iter().next().expect("directive");
  T::from_core(directive).unwrap_or_else(|| panic!("unexpected directive"))
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn parse_as_many<T: FromCore>(input: &str, filename: &str) -> Vec<T> {
  let directives = parse_core(input, filename);
  directives
    .into_iter()
    .map(|dir| T::from_core(dir).unwrap_or_else(|| panic!("unexpected directive")))
    .collect()
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn parse_ast<'a>(input: &'a str, filename: &str) -> Vec<ast::Directive<'a>> {
  parse_str(input, filename).expect("parse failed")
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn expect_ast_at<'a, T>(
  directives: &[ast::Directive<'a>],
  index: usize,
  matcher: impl FnOnce(ast::Directive<'a>) -> Option<T>,
) -> T {
  let directive = directives
    .get(index)
    .cloned()
    .unwrap_or_else(|| panic!("missing directive at index {index}"));
  matcher(directive).unwrap_or_else(|| panic!("unexpected directive at index {index}"))
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
