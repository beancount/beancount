mod common;
use beancount_parser::core::Comment;
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
