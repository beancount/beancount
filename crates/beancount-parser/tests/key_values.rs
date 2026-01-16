mod common;
use beancount_parser::ast::{Directive, Document, KeyValue, KeyValueValue, Note};
use common::{expect_ast_at, parse_ast};

#[test]
fn collects_key_values_on_note_and_document() {
  let input = [
    "2013-01-01 note Assets:Cash \"hello\"",
    "  key: \"value\"",
    "  num: 42",
    "",
    "2013-01-02 document Assets:Cash \"a.pdf\" #t ^l",
    "  desc: \"receipt\"",
    "",
  ]
  .join("\n");

  let directives = parse_ast(&input, "meta.bean");
  assert_eq!(directives.len(), 2);

  let note = expect_ast_at(&directives, 0, |d| match d {
    Directive::Note(n) => Some(n),
    _ => None,
  });

  let kv0 = note.key_values[0].clone();
  let kv1 = note.key_values[1].clone();
  let expected_note = Directive::Note(Note {
    key_values: smallvec::smallvec![
      KeyValue {
        value: Some(KeyValueValue::String("\"value\"")),
        ..kv0
      },
      KeyValue {
        value: Some(KeyValueValue::UnquotedString("42")),
        ..kv1
      },
    ],
    ..note.clone()
  });

  assert_eq!(directives[0], expected_note);

  let doc = expect_ast_at(&directives, 1, |d| match d {
    Directive::Document(d) => Some(d),
    _ => None,
  });

  let kv = doc.key_values[0].clone();
  let expected_doc = Directive::Document(Document {
    key_values: smallvec::smallvec![KeyValue {
      value: Some(KeyValueValue::String("\"receipt\"")),
      ..kv
    }],
    ..doc.clone()
  });

  assert_eq!(directives[1], expected_doc);
}

#[test]
fn parses_unquoted_string_value() {
  let input = ["2013-01-01 note Assets:Cash \"hello\"", "  key: value", ""].join("\n");

  let directives = parse_ast(&input, "meta.bean");
  assert_eq!(directives.len(), 1);

  let note = expect_ast_at(&directives, 0, |d| match d {
    Directive::Note(n) => Some(n),
    _ => None,
  });

  let kv = note.key_values[0].clone();
  let expected_note = Directive::Note(Note {
    key_values: smallvec::smallvec![KeyValue {
      value: Some(KeyValueValue::UnquotedString("value")),
      ..kv
    }],
    ..note.clone()
  });

  assert_eq!(directives[0], expected_note);
}
