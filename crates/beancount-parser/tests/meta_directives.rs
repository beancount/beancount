mod common;
use beancount_parser::core::{CoreDirective, KeyValueValue};
use common::{lines, parse_core};

#[test]
fn pushmeta_directive() {
  let input = lines(&[r#"pushmeta meta: "yes""#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Pushmeta(pm)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(pm.key_value, "meta: \"yes\"");
}

#[test]
fn popmeta_directive() {
  let input = lines(&[r#"popmeta meta:"#]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Popmeta(pm)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(pm.key, "meta");
}

#[test]
fn note_directive_with_metadata() {
  let input = lines(&[
    r#"2010-09-01 note Assets:Cash "note text""#,
    r#"  desc: "quoted desc""#,
  ]);

  let directives = parse_core(&input, "book.bean");
  let slice = directives.as_slice();
  let [CoreDirective::Note(note)] = slice else {
    panic!("unexpected directives: {slice:?}");
  };

  assert_eq!(note.note, "note text");
  assert_eq!(note.key_values.len(), 1);
  assert!(
    matches!(note.key_values[0].value, Some(KeyValueValue::String(ref s)) if s == "quoted desc")
  );
}
