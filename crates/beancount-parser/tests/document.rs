mod common;
use beancount_parser::core::{CoreDirective, KeyValueValue};
use common::{lines, parse_core};
use std::path::PathBuf;

#[test]
fn document_directive_with_tags_links() {
  let filename = "books/main.bean";
  let expected_path = PathBuf::from("books").join("file.pdf");
  let input = lines(&[
    r#"2010-10-01 document Assets:Cash "file.pdf" #doc ^lnk"#,
    r#"  reviewed: 2023-03-04"#,
  ]);

  let directives = parse_core(&input, filename);
  let slice = directives.as_slice();
  let [CoreDirective::Document(doc)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(doc.filename, expected_path.to_string_lossy());
  assert_eq!(doc.tags.as_slice(), &["doc"]);
  assert_eq!(doc.links.as_slice(), &["lnk"]);
  assert!(matches!(doc.key_values[0].value, Some(KeyValueValue::Date(ref d)) if d == "2023-03-04"));
}
