use beancount_parser::{parse_strict, render_strict_error};
use chumsky::error::Rich;

#[test]
fn strict_parser_reports_line_and_column() {
  let input = ["2010-01-01 open Assets:Cash", "not-a-directive"].join("\n");

  let Err(errors) = parse_strict(&input) else {
    panic!("strict parser unexpectedly succeeded");
  };

  assert!(!errors.is_empty());
  let error = &errors[0];
  println!("input:\n{}", input);
  println!("first error: {:?}", error);

  let rendered = render_strict_error("input", &input, error);
  println!("rendered:\n{}", rendered);

  let (line, column) = offset_to_line_col(&input, error.span().start);
  assert_eq!(line, 2);
  assert_eq!(column, 1);
  assert!(matches!(error, Rich { .. }));
  let rendered_plain = strip_ansi(&rendered);
  assert!(rendered_plain.contains("not-a-directive"));
  assert!(rendered_plain.to_lowercase().contains("expected"));
}

fn offset_to_line_col(src: &str, offset: usize) -> (usize, usize) {
  let mut line = 1;
  let mut col = 1;
  for (idx, ch) in src.char_indices() {
    if idx >= offset {
      break;
    }
    if ch == '\n' {
      line += 1;
      col = 1;
    } else {
      col += 1;
    }
  }
  (line, col)
}

fn strip_ansi(s: &str) -> String {
  let mut out = String::with_capacity(s.len());
  let mut chars = s.chars().peekable();
  while let Some(c) = chars.next() {
    if c == '\u{1b}' {
      for next in chars.by_ref() {
        if next == 'm' {
          break;
        }
      }
    } else {
      out.push(c);
    }
  }
  out
}
