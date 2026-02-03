use chumsky::prelude::*;
use ropey::Rope;
use std::sync::Arc;

use crate::{ast, MetaAt, ParseCtx, ParseError};
use crate::Error;

mod balance;
mod close;
mod commodity;
mod comment;
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
-> impl Parser<'src, &'src str, Option<Box<ast::Directive<'src>>>, Error<'src>> {
  choice((
    common::ws0_parser().then_ignore(just('\n')).to(None),
    common::ws1_parser().then_ignore(end()).to(None),
  ))
}

fn directive_parser<'src>() -> impl Parser<'src, &'src str, Box<ast::Directive<'src>>, Error<'src>> + 'src {
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
    raw::raw_block_parser(),
  ))
  .map(Box::new)
  .boxed()
}

fn declarations_parser<'src>()
-> impl Parser<'src, &'src str, Vec<Option<Box<ast::Directive<'src>>>>, Error<'src>> + 'src {
  choice((directive_parser().map(Some), skipped_line_parser()))
    .repeated()
    .collect::<Vec<_>>()
    .boxed()
}

pub fn parse_str<'a>(
  source: &'a str,
  filename: &str,
) -> std::result::Result<Vec<ast::Directive<'a>>, ParseError> {
  let ctx = ParseCtx {
    filename: Arc::new(filename.to_owned()),
    rope: Rope::from_str(source),
  };
  let meta_at = MetaAt::from(&ctx);

  let directives = declarations_parser()
    .then_ignore(end())
    .parse(source)
    .into_result()
    .map_err(|mut errors| match errors.pop() {
      Some(err) => {
        let span = err.span();
        let meta = meta_at.at(span.start);
        ParseError {
          line: meta.line,
          column: meta.column,
          message: err.to_string(),
        }
      }
      None => ParseError {
        line: 1,
        column: 1,
        message: "parse error".to_string(),
      },
    })?;

  let directives: Vec<_> = directives
    .into_iter()
    .flatten()
    .map(|directive| *directive)
    .collect();

  Ok(directives)
}
