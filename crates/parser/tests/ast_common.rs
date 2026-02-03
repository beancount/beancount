use beancount_parser::{ast, parse_str};

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
