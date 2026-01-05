use anyhow::{Context, Result};
use beancount_parser::ast::Directive;
use beancount_parser::parse_directives_with_meta;
use tree_sitter::Parser;

#[test]
fn meta_is_attached_to_each_directive() -> Result<()> {
  let input = "\
2010-01-01 open Assets:Cash CNY\n\
2010-01-02 close Assets:Cash\n\
";

  let mut parser = Parser::new();
  parser
    .set_language(&tree_sitter_beancount::language())
    .context("Failed to load beancount grammar")?;
  let tree = parser.parse(input, None).context("Failed to parse input")?;
  let root = tree.root_node();

  let dirs = parse_directives_with_meta(root, input, "book.bean".to_string())?;
  assert_eq!(dirs.len(), 2);

  match &dirs[0] {
    Directive::Open(o) => {
      assert_eq!(o.meta.filename.as_str(), "book.bean");
      assert_eq!(o.meta.line, 1);
      assert_eq!(o.meta.column, 1);
    }
    other => panic!("expected open, got {other:?}"),
  }

  match &dirs[1] {
    Directive::Close(c) => {
      assert_eq!(c.meta.filename.as_str(), "book.bean");
      assert_eq!(c.meta.line, 2);
      assert_eq!(c.meta.column, 1);
    }
    other => panic!("expected close, got {other:?}"),
  }

  Ok(())
}
