use beancount_parser::ast::{Directive, Span};
use beancount_parser::parse_str;

fn span_in_line(line_start: usize, line: &str, needle: &str) -> Span {
  let rel = line.find(needle).expect("missing needle in line");
  let start = line_start + rel;
  Span::from_range(start, start + needle.len())
}

fn line_span(input: &str, line: &str) -> Span {
  let needle = format!("{line}\n");
  let start = input.find(&needle).expect("missing segment for line");
  Span::from_range(start, start + needle.len())
}

#[test]
fn spans_cover_single_line_directives_and_children() {
  let open = "2010-01-01 open Assets:Cash";
  let close = "2010-01-02 close Assets:Cash";
  let balance = "2010-01-03 balance Assets:Cash 10 USD";
  let pad = "2010-01-04 pad Assets:Cash Equity:Pad";
  let note = "2010-01-05 note Assets:Cash \"hello\"  ; 1";
  let document = "2010-01-06 document Assets:Cash \"doc.pdf\"";

  let input = [open, close, balance, pad, note, document, ""].join("\n");

  let directives = parse_str(&input).expect("parse failed");
  assert_eq!(directives.len(), 6);

  let open_line = format!("{open}\n");
  let open_span = line_span(&input, open);
  match &directives[0] {
    Directive::Open(dir) => {
      assert_eq!(dir.span, open_span);
      assert_eq!(
        dir.keyword,
        span_in_line(open_span.start, &open_line, "open")
      );
      assert_eq!(
        dir.date.span,
        span_in_line(open_span.start, &open_line, "2010-01-01")
      );
      assert_eq!(
        dir.account.span,
        span_in_line(open_span.start, &open_line, "Assets:Cash")
      );
    }
    other => panic!("expected Open, got {other:?}"),
  }

  let close_line = format!("{close}\n");
  let close_span = line_span(&input, close);
  match &directives[1] {
    Directive::Close(dir) => {
      assert_eq!(dir.span, close_span);
      assert_eq!(
        dir.keyword,
        span_in_line(close_span.start, &close_line, "close")
      );
      assert_eq!(
        dir.date.span,
        span_in_line(close_span.start, &close_line, "2010-01-02")
      );
      assert_eq!(
        dir.account.span,
        span_in_line(close_span.start, &close_line, "Assets:Cash")
      );
    }
    other => panic!("expected Close, got {other:?}"),
  }

  let balance_line = format!("{balance}\n");
  let balance_span = line_span(&input, balance);
  match &directives[2] {
    Directive::Balance(dir) => {
      assert_eq!(dir.span, balance_span);
      assert_eq!(
        dir.keyword,
        span_in_line(balance_span.start, &balance_line, "balance")
      );
      assert_eq!(
        dir.date.span,
        span_in_line(balance_span.start, &balance_line, "2010-01-03")
      );
      assert_eq!(
        dir.account.span,
        span_in_line(balance_span.start, &balance_line, "Assets:Cash")
      );
      assert_eq!(
        dir.amount.raw.span,
        span_in_line(balance_span.start, &balance_line, "10 USD")
      );
    }
    other => panic!("expected Balance, got {other:?}"),
  }

  let pad_line = format!("{pad}\n");
  let pad_span = line_span(&input, pad);
  match &directives[3] {
    Directive::Pad(dir) => {
      assert_eq!(dir.span, pad_span);
      assert_eq!(dir.keyword, span_in_line(pad_span.start, &pad_line, "pad"));
      assert_eq!(
        dir.date.span,
        span_in_line(pad_span.start, &pad_line, "2010-01-04")
      );
      assert_eq!(
        dir.account.span,
        span_in_line(pad_span.start, &pad_line, "Assets:Cash")
      );
      assert_eq!(
        dir.from_account.span,
        span_in_line(pad_span.start, &pad_line, "Equity:Pad"),
      );
    }
    other => panic!("expected Pad, got {other:?}"),
  }

  let note_line = format!("{note}\n");
  let note_span = line_span(&input, note);
  match &directives[4] {
    Directive::Note(dir) => {
      assert_eq!(dir.span, note_span);
      assert_eq!(
        dir.keyword,
        span_in_line(note_span.start, &note_line, "note")
      );
      assert_eq!(
        dir.date.span,
        span_in_line(note_span.start, &note_line, "2010-01-05")
      );
      assert_eq!(
        dir.account.span,
        span_in_line(note_span.start, &note_line, "Assets:Cash")
      );
      assert_eq!(dir.note.content, "\"hello\"");
      assert_eq!(
        dir.note.span,
        span_in_line(note_span.start, &note_line, "\"hello\"")
      );
    }
    other => panic!("expected Note, got {other:?}"),
  }

  let document_line = format!("{document}\n");
  let document_span = line_span(&input, document);
  match &directives[5] {
    Directive::Document(dir) => {
      assert_eq!(dir.span, document_span);
      assert_eq!(
        dir.keyword,
        span_in_line(document_span.start, &document_line, "document")
      );
      assert_eq!(
        dir.date.span,
        span_in_line(document_span.start, &document_line, "2010-01-06")
      );
      assert_eq!(
        dir.account.span,
        span_in_line(document_span.start, &document_line, "Assets:Cash")
      );
      assert_eq!(
        dir.filename.span,
        span_in_line(document_span.start, &document_line, "\"doc.pdf\"")
      );
    }
    other => panic!("expected Document, got {other:?}"),
  }
}

#[test]
fn spans_when_not_first_line() {
  let txn_block = ["2013-06-22 * \"Payee\" \"Narr\"", "  Assets:Cash 1 USD", ""].join("\n");

  let input = ["", "", txn_block.as_str()].join("\n");

  let directives = parse_str(&input).expect("parse failed");

  let start = input.find(&txn_block).expect("missing txn block");
  let expected_span = Span::from_range(start, start + txn_block.len());

  let txn = directives
    .iter()
    .find_map(|d| match d {
      Directive::Transaction(t) => Some(t),
      _ => None,
    })
    .expect("missing transaction directive");

  assert_eq!(txn.span, expected_span);
  assert!(
    txn.span.start > 0,
    "span should reflect offset past header lines"
  );
}
