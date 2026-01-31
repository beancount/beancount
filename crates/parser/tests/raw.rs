mod common;
use beancount_parser::core::{Comment, Headline};
use common::{FromCore, lines, parse_core};

#[test]
fn comments_are_ignored() {
  let input = lines(&[r#"; trailing raw text"#]);

  let directives = parse_core(&input, "book.bean");
  assert_eq!(directives.len(), 1);
  let comment =
    Comment::from_core(directives.into_iter().next().unwrap()).expect("comment directive");
  assert_eq!(comment.text, "; trailing raw text");
}

#[test]
fn org_headline_is_preserved_as_comment() {
  let input = lines(&[r"**** C:\Users\Trim21\proj\count\input.bean"]);

  let directives = parse_core(&input, "book.bean");
  assert_eq!(directives.len(), 1);
  let headline =
    Headline::from_core(directives.into_iter().next().unwrap()).expect("headline directive");
  assert_eq!(headline.text, r"**** C:\Users\Trim21\proj\count\input.bean");
}
