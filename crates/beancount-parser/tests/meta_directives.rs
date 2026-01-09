mod common;
use beancount_parser::core::{KeyValue, KeyValueValue, Note, PopMeta, PushMeta};
use common::{lines, parse_as};

#[test]
fn pushmeta_directive() {
  let input = lines(&[r#"pushmeta meta: "yes""#]);

  let pm: PushMeta = parse_as(&input, "book.bean");

  let expected = PushMeta {
    meta: pm.meta.clone(),
    span: pm.span,
    key_value: "meta: \"yes\"".into(),
  };

  assert_eq!(pm, expected);
}

#[test]
fn popmeta_directive() {
  let input = lines(&[r#"popmeta meta:"#]);

  let pm: PopMeta = parse_as(&input, "book.bean");

  let expected = PopMeta {
    meta: pm.meta.clone(),
    span: pm.span,
    key: "meta".into(),
  };

  assert_eq!(pm, expected);
}

#[test]
fn note_directive_with_metadata() {
  let input = lines(&[
    r#"2010-09-01 note Assets:Cash "note text""#,
    r#"  desc: "quoted desc""#,
  ]);

  let note: Note = parse_as(&input, "book.bean");

  let [kv] = note.key_values.as_slice() else {
    panic!("unexpected key values: {:?}", note.key_values);
  };

  let mut key_values = note.key_values.clone();
  key_values[0] = KeyValue {
    value: Some(KeyValueValue::String("quoted desc".into())),
    ..kv.clone()
  };

  let expected = Note {
    meta: note.meta.clone(),
    span: note.span,
    date: "2010-09-01".into(),
    account: "Assets:Cash".into(),
    note: "note text".into(),
    comment: None,
    key_values,
  };

  assert_eq!(note, expected);
}
