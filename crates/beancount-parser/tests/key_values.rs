use beancount_parser::{ast::Directive, parse_str};

#[test]
fn collects_key_values_on_note_and_document() {
    let input = vec![
        "2013-01-01 note Assets:Cash \"hello\"",
        "  key: \"value\"",
        "  num: 42",
        "",
        "2013-01-02 document Assets:Cash \"a.pdf\" #t ^l",
        "  desc: \"receipt\"",
        "",
    ]
    .join("\n");

    let directives = parse_str(&input, "meta.bean").expect("parse failed");
    assert_eq!(directives.len(), 2);

    match &directives[0] {
        Directive::Note(note) => {
            assert_eq!(note.key_values.len(), 2);
            assert_eq!(note.key_values[0].key, "key");
            assert_eq!(
                note.key_values[0].value,
                Some(beancount_parser::ast::KeyValueValue::String("\"value\""))
            );
            assert_eq!(note.key_values[1].key, "num");
            assert_eq!(
                note.key_values[1].value,
                Some(beancount_parser::ast::KeyValueValue::Raw("42"))
            );
        }
        other => panic!("expected note, got {other:?}"),
    }

    match &directives[1] {
        Directive::Document(doc) => {
            assert_eq!(doc.key_values.len(), 1);
            assert_eq!(doc.key_values[0].key, "desc");
            assert_eq!(
                doc.key_values[0].value,
                Some(beancount_parser::ast::KeyValueValue::String("\"receipt\""))
            );
        }
        other => panic!("expected document, got {other:?}"),
    }
}

#[test]
fn parses_unquoted_string_value() {
    let input = vec![
        "2013-01-01 note Assets:Cash \"hello\"",
        "  key: value",
        "",
    ]
    .join("\n");

    let directives = parse_str(&input, "meta.bean").expect("parse failed");
    assert_eq!(directives.len(), 1);

    match &directives[0] {
        Directive::Note(note) => {
            assert_eq!(note.key_values.len(), 1);
            assert_eq!(note.key_values[0].key, "key");
            assert_eq!(
                note.key_values[0].value,
                Some(beancount_parser::ast::KeyValueValue::UnquotedString("value"))
            );
        }
        other => panic!("expected note, got {other:?}"),
    }
}
