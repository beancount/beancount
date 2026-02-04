use chumsky::prelude::*;
use ropey::Rope;

use crate::Error;
use crate::{ParseError, Position, ast, position_from_rope};

mod balance;
mod close;
mod comment;
mod commodity;
mod common;
mod custom;
mod document;
mod event;
mod headline;
mod include;
mod note;
mod number;
mod open;
mod option;
mod pad;
mod plugin;
mod popmeta;
mod poptag;
mod price;
mod pushmeta;
mod pushtag;
mod query;
mod raw;
mod transaction;

fn skipped_line_parser<'src>()
-> impl Parser<'src, &'src str, Option<ast::Directive<'src>>, Error<'src>> {
  choice((
    common::ws0_parser().then_ignore(common::newline()).to(None),
    common::ws1_parser().then_ignore(end()).to(None),
  ))
}

fn directive_parser<'src>() -> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src
{
  choice((
    include::include_directive_parser().then_ignore(common::line_end()),
    plugin::plugin_directive_parser().then_ignore(common::line_end()),
    option::option_directive_parser().then_ignore(common::line_end()),
    pushtag::pushtag_directive_parser().then_ignore(common::line_end()),
    poptag::poptag_directive_parser().then_ignore(common::line_end()),
    pushmeta::pushmeta_directive_parser().then_ignore(common::line_end()),
    popmeta::popmeta_directive_parser().then_ignore(common::line_end()),
    comment::comment_directive_parser().then_ignore(common::line_end()),
    headline::headline_directive_parser().then_ignore(common::line_end()),
    open::open_directive_parser(),
    close::close_directive_parser(),
    balance::balance_directive_parser(),
    pad::pad_directive_parser(),
    commodity::commodity_directive_parser(),
    price::price_directive_parser(),
    event::event_directive_parser(),
    query::query_directive_parser(),
    note::note_directive_parser(),
    document::document_directive_parser(),
    custom::custom_directive_parser(),
    transaction::transaction_directive_parser(),
  ))
  .recover_with(via_parser(recovery_raw_directive_parser()))
  .boxed()
}

fn recovery_raw_directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
  let restart = just('\n')
    .ignore_then(any().filter(|c: &char| !c.is_whitespace()))
    .rewind();

  any()
    .and_is(restart.not())
    .repeated()
    .at_least(1)
    .to_slice()
    .map_with(move |_, e| {
      let span: SimpleSpan = e.span();
      let consumed: &str = e.slice();

      // Trim a trailing newline so raw text matches expectations and does not
      // swallow the next top-level directive.
      let (text, end) = if consumed.ends_with("\r\n") {
        (
          &consumed[..consumed.len().saturating_sub(2)],
          span.end.saturating_sub(2),
        )
      } else if consumed.ends_with('\n') {
        (
          &consumed[..consumed.len().saturating_sub(1)],
          span.end.saturating_sub(1),
        )
      } else {
        (consumed, span.end)
      };

      ast::Directive::Raw(ast::Raw {
        span: ast::Span::from_range(span.start, end),
        text,
      }) 
    })
}

fn declarations_parser<'src>()
-> impl Parser<'src, &'src str, Vec<Option<ast::Directive<'src>>>, Error<'src>> + 'src {
  choice((skipped_line_parser(), directive_parser().map(Some)))
    .repeated()
    .collect::<Vec<_>>()
    .boxed()
}

pub fn parse_str_with_rope<'a>(
  source: &'a str,
) -> std::result::Result<(Vec<ast::Directive<'a>>, Rope), ParseError> {
  let rope = Rope::from_str(source);

  let (directives, errors) = declarations_parser()
    .then_ignore(end())
    .parse(source)
    .into_output_errors();

  let directives = match directives {
    Some(value) => value,
    None => {
      let mut errors = errors.into_iter();
      if let Some(err) = errors.next() {
        let span = err.span();
        let Position { line, column } = position_from_rope(&rope, span.start);
        return Err(ParseError {
          line,
          column,
          message: err.to_string(),
        });
      }

      return Err(ParseError {
        line: 1,
        column: 1,
        message: "parse error".to_string(),
      });
    }
  };

  let directives: Vec<_> = directives
    .into_iter()
    .flatten()
    // .map(|directive| directive)
    .collect();

  let indent_of = |offset: usize| {
    let line_char_idx = rope.byte_to_char(offset);
    let line_idx = rope.char_to_line(line_char_idx);
    let line_text = rope.line(line_idx);
    line_text
      .chars()
      .take_while(|c| *c == ' ' || *c == '\t')
      .count()
  };

  let last_is_transaction = directives
    .last()
    .map(|dir| matches!(dir, ast::Directive::Transaction(_)))
    .unwrap_or(false);

  for err in &errors {
    let span = err.span();
    let Position { line, column: _ } = position_from_rope(&rope, span.start);

    let indent = indent_of(span.start);

    // Surface errors that originate on indented lines immediately following a transaction
    // to avoid silently accepting invalid transaction bodies. Other indented errors are
    // recovered into Raw for lenient top-level handling.
    if indent > 0 && last_is_transaction {
      return Err(ParseError {
        line,
        column: indent + 1,
        message: err.to_string(),
      });
    }
  }

  for window in directives.windows(2) {
    if let [ast::Directive::Transaction(_), ast::Directive::Raw(raw)] = window {
      let indent = indent_of(raw.span.start);
      if indent > 0 {
        let Position { line, column: _ } = position_from_rope(&rope, raw.span.start);
        return Err(ParseError {
          line,
          column: indent + 1,
          message: "unexpected raw in transaction body".to_string(),
        });
      }
    }
  }

  Ok((directives, rope))
}

pub fn parse_str<'a>(source: &'a str) -> std::result::Result<Vec<ast::Directive<'a>>, ParseError> {
  parse_str_with_rope(source).map(|(directives, _rope)| directives)
}
