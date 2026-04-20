use chumsky::prelude::*;

use crate::Error;
use crate::ast;

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

#[cfg(feature = "rich-errors")]
pub type StrictError<'src> = chumsky::error::Rich<'src, char>;
#[cfg(not(feature = "rich-errors"))]
pub type StrictError<'src> = chumsky::error::Simple<'src, char>;

fn skipped_line_parser<'src>()
-> impl Parser<'src, &'src str, Option<ast::Directive<'src>>, Error<'src>> {
  choice((
    common::ws0_parser().then_ignore(common::eol()).to(None),
    common::ws1_parser().then_ignore(end()).to(None),
  ))
}

fn directive_parser<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
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
  .recover_with(via_parser(common::raw_directive_recovery_parser()))
}

fn directive_parser_strict<'src>()
-> impl Parser<'src, &'src str, ast::Directive<'src>, Error<'src>> + 'src {
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
}

fn declarations_parser<'src>()
-> impl Parser<'src, &'src str, Vec<Option<ast::Directive<'src>>>, Error<'src>> + 'src {
  choice((skipped_line_parser(), directive_parser().map(Some)))
    .repeated()
    .collect::<Vec<_>>()
}

fn declarations_parser_strict<'src>()
-> impl Parser<'src, &'src str, Vec<Option<ast::Directive<'src>>>, Error<'src>> + 'src {
  choice((skipped_line_parser(), directive_parser_strict().map(Some)))
    .repeated()
    .collect::<Vec<_>>()
}

pub fn parse_lossy<'a>(source: &'a str) -> Vec<ast::Directive<'a>> {
  declarations_parser()
    .then_ignore(end())
    .parse(source)
    .into_output()
    .map(|directives| directives.into_iter().flatten().collect())
    .unwrap_or_else(Vec::new)
}

/// Parse without recovery to `Raw`; returns errors instead.
pub fn parse_strict<'a>(
  source: &'a str,
) -> Result<Vec<ast::Directive<'a>>, Vec<StrictError<'a>>> {
  declarations_parser_strict()
    .then_ignore(end())
    .parse(source)
    .into_result()
    .map(|directives| directives.into_iter().flatten().collect())
}
