mod common;
use beancount_parser::core::{Document, KeyValue, KeyValueValue};
use common::{lines, parse_as};
use smallvec::smallvec;
use std::path::PathBuf;

#[test]
fn document_directive_with_tags_links() {
  let filename = "books/main.bean";
  let expected_path = PathBuf::from("books").join("file.pdf");
  let input = lines(&[
    r#"2010-10-01 document Assets:Cash "file.pdf" #doc ^lnk"#,
    r#"  reviewed: 2023-03-04"#,
  ]);

  let doc: Document = parse_as(&input, filename);

  let kv = doc.key_values[0].clone();
  let mut key_values = doc.key_values.clone();
  key_values[0] = KeyValue {
    value: Some(KeyValueValue::Date("2023-03-04".into())),
    ..kv
  };

  let expected = Document {
    meta: doc.meta.clone(),
    span: doc.span,
    date: "2010-10-01".into(),
    account: "Assets:Cash".into(),
    filename: expected_path.to_string_lossy().into_owned(),
    tags_links: doc.tags_links.clone(),
    tags: smallvec!["doc".into()],
    links: smallvec!["lnk".into()],
    comment: None,
    key_values,
  };

  assert_eq!(doc, expected);
}
