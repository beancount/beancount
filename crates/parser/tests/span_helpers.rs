use beancount_parser::ast::Span;

pub fn span_for(haystack: &str, needle: &str) -> Span {
  let start = haystack
    .find(needle)
    .unwrap_or_else(|| panic!("snippet not found: {needle}"));
  Span::from_range(start, start + needle.len())
}
