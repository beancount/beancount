use crate::path_utils::resolve_path;
use crate::{ParseError, ast};
use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde_json::from_str as parse_json;
use smallvec::SmallVec;
use std::convert::TryFrom;
use std::str::FromStr;

pub type SmallStrVec = SmallVec<[String; 4]>;
pub type SmallKeyValues = SmallVec<[KeyValue; 4]>;
pub type SmallPostings = SmallVec<[Posting; 4]>;
pub type SmallCustomValues = SmallVec<[CustomValue; 2]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoreDirective {
  Open(Open),
  Close(Close),
  Balance(Balance),
  Pad(Pad),
  Transaction(Transaction),
  Commodity(Commodity),
  Price(Price),
  Event(Event),
  Query(Query),
  Note(Note),
  Document(Document),
  Custom(Custom),
  Option(OptionDirective),
  Include(Include),
  Plugin(Plugin),
  Pushtag(TagDirective),
  Poptag(TagDirective),
  Pushmeta(PushMeta),
  Popmeta(PopMeta),
  Headline(Headline),
  Comment(Comment),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Open {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub account: String,
  pub currencies: SmallStrVec,
  pub opt_booking: Option<String>,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Close {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub account: String,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Balance {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub account: String,
  pub amount: Amount,
  pub tolerance: Option<String>,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pad {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub account: String,
  pub from_account: String,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transaction {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub txn: Option<String>,
  pub payee: Option<String>,
  pub narration: Option<String>,
  pub tags: SmallStrVec,
  pub links: SmallStrVec,
  pub key_values: SmallKeyValues,
  pub postings: SmallPostings,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Posting {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub opt_flag: Option<String>,
  pub account: String,
  pub amount: Option<Amount>,
  pub cost_spec: Option<CostSpec>,
  pub price_operator: Option<ast::PriceOperator>,
  pub price_annotation: Option<Amount>,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Commodity {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub currency: String,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Price {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub currency: String,
  pub amount: Amount,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Event {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub event_type: String,
  pub desc: String,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub name: String,
  pub query: String,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Note {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub account: String,
  pub note: String,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub account: String,
  pub filename: String,
  pub tags_links: Option<String>,
  pub tags: SmallStrVec,
  pub links: SmallStrVec,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Custom {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub date: String,
  pub name: String,
  pub values: SmallCustomValues,
  pub comment: Option<String>,
  pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CustomValue {
  String(String),
  Date(NaiveDate),
  Bool(bool),
  Amount(Amount),
  Number(NumberExpr),
  Account(String),
}

fn value_error(meta: &ast::Meta, message: impl Into<String>) -> ParseError {
  ParseError {
    filename: meta.filename.clone(),
    line: meta.line,
    column: meta.column,
    message: message.into(),
  }
}

fn parse_date_value(raw: &str, meta: &ast::Meta, ctx: &str) -> Result<NaiveDate, ParseError> {
  let trimmed = raw.trim();
  NaiveDate::parse_from_str(trimmed, "%Y-%m-%d")
    .map_err(|err| value_error(meta, format!("invalid {} `{}`: {}", ctx, raw, err)))
}

fn parse_bool_value(raw: &str, meta: &ast::Meta, ctx: &str) -> Result<bool, ParseError> {
  let trimmed = raw.trim();
  match trimmed.eq_ignore_ascii_case("true") {
    true => Ok(true),
    false if trimmed.eq_ignore_ascii_case("false") => Ok(false),
    _ => Err(value_error(
      meta,
      format!("invalid {} `{}`: expected TRUE or FALSE", ctx, raw),
    )),
  }
}

fn parse_key_value_value(
  value: Option<ast::KeyValueValue<'_>>,
  meta: &ast::Meta,
  ctx: &str,
  allow_unquoted_on_error: bool,
) -> Result<Option<KeyValueValue>, ParseError> {
  value
    .map(|v| match v {
      ast::KeyValueValue::String(raw) => match unquote_json(raw, meta, ctx) {
        Ok(val) => Ok(KeyValueValue::String(val)),
        Err(err) if allow_unquoted_on_error => Ok(KeyValueValue::UnquotedString(raw.to_string())),
        Err(err) => Err(err),
      },
      ast::KeyValueValue::UnquotedString(raw) => Ok(KeyValueValue::UnquotedString(raw.to_string())),
      ast::KeyValueValue::Date(raw) => Ok(KeyValueValue::Date(raw.to_string())),
      ast::KeyValueValue::Bool(val) => Ok(KeyValueValue::Bool(val)),
      ast::KeyValueValue::Raw(raw) => Ok(KeyValueValue::Raw(raw.to_string())),
    })
    .transpose()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OptionDirective {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub key: String,
  pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Include {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub filename: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Plugin {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub name: String,
  pub config: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagDirective {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub tag: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PushMeta {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub key: String,
  pub value: Option<KeyValueValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PopMeta {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub key: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comment {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Headline {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValue {
  pub meta: ast::Meta,
  pub span: ast::Span,
  pub key: String,
  pub value: Option<KeyValueValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyValueValue {
  String(String),
  UnquotedString(String),
  Date(String),
  Bool(bool),
  Raw(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberExpr {
  Missing,
  Literal(String),
  Binary {
    left: Box<NumberExpr>,
    op: BinaryOp,
    right: Box<NumberExpr>,
  },
}

impl From<ast::BinaryOp> for BinaryOp {
  fn from(op: ast::BinaryOp) -> Self {
    match op {
      ast::BinaryOp::Add => BinaryOp::Add,
      ast::BinaryOp::Sub => BinaryOp::Sub,
      ast::BinaryOp::Mul => BinaryOp::Mul,
      ast::BinaryOp::Div => BinaryOp::Div,
    }
  }
}

impl From<ast::NumberExpr<'_>> for NumberExpr {
  fn from(num: ast::NumberExpr<'_>) -> Self {
    match num {
      ast::NumberExpr::Missing => NumberExpr::Missing,
      ast::NumberExpr::Literal(s) => NumberExpr::Literal(s.to_string()),
      ast::NumberExpr::Binary { left, op, right } => NumberExpr::Binary {
        left: Box::new(NumberExpr::from(*left)),
        op: BinaryOp::from(op),
        right: Box::new(NumberExpr::from(*right)),
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberEvalError {
  pub message: String,
}

impl std::fmt::Display for NumberEvalError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.message)
  }
}

impl std::error::Error for NumberEvalError {}

fn clean_decimal_literal(raw: &str) -> String {
  raw.chars().filter(|c| *c != ',' && *c != ' ').collect()
}

pub fn parse_decimal_literal(raw: &str) -> Result<Decimal, NumberEvalError> {
  let cleaned = clean_decimal_literal(raw);
  if cleaned.trim().is_empty() {
    return Ok(Decimal::ZERO);
  }

  Decimal::from_str(cleaned.trim()).map_err(|err| NumberEvalError {
    message: format!("invalid number `{}`: {}", raw, err),
  })
}

pub fn number_expr_to_decimal(num: &NumberExpr) -> Result<Decimal, NumberEvalError> {
  match num {
    NumberExpr::Missing => Err(NumberEvalError {
      message: "missing number expression".to_string(),
    }),
    NumberExpr::Literal(raw) => parse_decimal_literal(raw),
    NumberExpr::Binary { left, op, right } => {
      let lhs = number_expr_to_decimal(left)?;
      let rhs = number_expr_to_decimal(right)?;
      let result = match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => lhs / rhs,
      };
      Ok(result)
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostAmount {
  pub per: Option<NumberExpr>,
  pub total: Option<NumberExpr>,
  pub currency: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostSpec {
  pub raw: String,
  pub amount: Option<CostAmount>,
  pub date: Option<String>,
  pub label: Option<String>,
  pub merge: bool,
  pub is_total: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount {
  pub raw: String,
  pub number: NumberExpr,
  pub currency: Option<String>,
}

pub fn normalize_directives<'a>(
  directives: Vec<ast::Directive<'a>>,
) -> Result<Vec<CoreDirective>, ParseError> {
  directives
    .into_iter()
    .map(CoreDirective::try_from)
    .collect()
}

impl<'a> TryFrom<ast::Directive<'a>> for CoreDirective {
  type Error = ParseError;

  fn try_from(directive: ast::Directive<'a>) -> Result<Self, Self::Error> {
    match directive {
      ast::Directive::Open(open) => Ok(CoreDirective::Open(Open::try_from(open)?)),
      ast::Directive::Close(close) => Ok(CoreDirective::Close(Close::try_from(close)?)),
      ast::Directive::Balance(balance) => Ok(CoreDirective::Balance(Balance::try_from(balance)?)),
      ast::Directive::Pad(pad) => Ok(CoreDirective::Pad(Pad::try_from(pad)?)),
      ast::Directive::Transaction(txn) => {
        Ok(CoreDirective::Transaction(Transaction::try_from(txn)?))
      }
      ast::Directive::Commodity(cmdty) => Ok(CoreDirective::Commodity(Commodity::try_from(cmdty)?)),
      ast::Directive::Price(price) => Ok(CoreDirective::Price(Price::try_from(price)?)),
      ast::Directive::Event(event) => Ok(CoreDirective::Event(Event::try_from(event)?)),
      ast::Directive::Query(query) => Ok(CoreDirective::Query(Query::try_from(query)?)),
      ast::Directive::Note(note) => Ok(CoreDirective::Note(Note::try_from(note)?)),
      ast::Directive::Document(doc) => Ok(CoreDirective::Document(Document::try_from(doc)?)),
      ast::Directive::Custom(custom) => Ok(CoreDirective::Custom(Custom::try_from(custom)?)),
      ast::Directive::Option(opt) => Ok(CoreDirective::Option(OptionDirective::try_from(opt)?)),
      ast::Directive::Include(include) => Ok(CoreDirective::Include(Include::try_from(include)?)),
      ast::Directive::Plugin(plugin) => Ok(CoreDirective::Plugin(Plugin::try_from(plugin)?)),
      ast::Directive::PushTag(tag) => Ok(CoreDirective::Pushtag(TagDirective::try_from(tag)?)),
      ast::Directive::PopTag(tag) => Ok(CoreDirective::Poptag(TagDirective::try_from(tag)?)),
      ast::Directive::PushMeta(pm) => Ok(CoreDirective::Pushmeta(PushMeta::try_from(pm)?)),
      ast::Directive::PopMeta(pm) => Ok(CoreDirective::Popmeta(PopMeta::try_from(pm)?)),
      ast::Directive::Comment(comment) => Ok(CoreDirective::Comment(Comment::try_from(comment)?)),
      ast::Directive::Headline(headline) => {
        Ok(CoreDirective::Headline(Headline::try_from(headline)?))
      }
    }
  }
}

impl<'a> TryFrom<ast::Comment<'a>> for Comment {
  type Error = ParseError;

  fn try_from(comment: ast::Comment<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: comment.meta,
      span: comment.span,
      text: comment.text.to_string(),
    })
  }
}

impl<'a> TryFrom<ast::Headline<'a>> for Comment {
  type Error = ParseError;

  fn try_from(headline: ast::Headline<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: headline.meta,
      span: headline.span,
      text: headline.text.to_string(),
    })
  }
}

impl<'a> TryFrom<ast::Headline<'a>> for Headline {
  type Error = ParseError;

  fn try_from(headline: ast::Headline<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: headline.meta,
      span: headline.span,
      text: headline.text.to_string(),
    })
  }
}

impl<'a> TryFrom<ast::Open<'a>> for Open {
  type Error = ParseError;

  fn try_from(open: ast::Open<'a>) -> Result<Self, Self::Error> {
    let meta = open.meta.clone();
    Ok(Self {
      meta: meta.clone(),
      span: open.span,
      date: open.date.to_string(),
      account: open.account.to_string(),
      currencies: open
        .currencies
        .into_iter()
        .map(ToString::to_string)
        .collect(),
      opt_booking: open
        .opt_booking
        .map(|booking| unquote_json(booking, &meta, "booking method"))
        .transpose()?,
      comment: open.comment.map(ToString::to_string),
      key_values: open
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Close<'a>> for Close {
  type Error = ParseError;

  fn try_from(close: ast::Close<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: close.meta,
      span: close.span,
      date: close.date.to_string(),
      account: close.account.to_string(),
      comment: close.comment.map(ToString::to_string),
      key_values: close
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Balance<'a>> for Balance {
  type Error = ParseError;

  fn try_from(balance: ast::Balance<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: balance.meta,
      span: balance.span,
      date: balance.date.to_string(),
      account: balance.account.to_string(),
      amount: Amount::try_from(balance.amount)?,
      tolerance: balance.tolerance.map(ToString::to_string),
      comment: balance.comment.map(ToString::to_string),
      key_values: balance
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Pad<'a>> for Pad {
  type Error = ParseError;

  fn try_from(pad: ast::Pad<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: pad.meta,
      span: pad.span,
      date: pad.date.to_string(),
      account: pad.account.to_string(),
      from_account: pad.from_account.to_string(),
      comment: pad.comment.map(ToString::to_string),
      key_values: pad
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Commodity<'a>> for Commodity {
  type Error = ParseError;

  fn try_from(cmdty: ast::Commodity<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: cmdty.meta,
      span: cmdty.span,
      date: cmdty.date.to_string(),
      currency: cmdty.currency.to_string(),
      comment: cmdty.comment.map(ToString::to_string),
      key_values: cmdty
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Price<'a>> for Price {
  type Error = ParseError;

  fn try_from(price: ast::Price<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: price.meta,
      span: price.span,
      date: price.date.to_string(),
      currency: price.currency.to_string(),
      amount: Amount::try_from(price.amount)?,
      comment: price.comment.map(ToString::to_string),
      key_values: price
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Event<'a>> for Event {
  type Error = ParseError;

  fn try_from(event: ast::Event<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: event.meta.clone(),
      span: event.span,
      date: event.date.to_string(),
      event_type: unquote_json(event.event_type, &event.meta, "event type")?,
      desc: unquote_json(event.desc, &event.meta, "event description")?,
      comment: event.comment.map(ToString::to_string),
      key_values: event
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Query<'a>> for Query {
  type Error = ParseError;

  fn try_from(query: ast::Query<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: query.meta.clone(),
      span: query.span,
      date: query.date.to_string(),
      name: unquote_json(query.name, &query.meta, "query name")?,
      query: unquote_json(query.query, &query.meta, "query")?,
      comment: query.comment.map(ToString::to_string),
      key_values: query
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Note<'a>> for Note {
  type Error = ParseError;

  fn try_from(note: ast::Note<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: note.meta.clone(),
      span: note.span,
      date: note.date.to_string(),
      account: note.account.to_string(),
      note: unquote_json(note.note, &note.meta, "note")?,
      comment: note.comment.map(ToString::to_string),
      key_values: note
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Document<'a>> for Document {
  type Error = ParseError;

  fn try_from(doc: ast::Document<'a>) -> Result<Self, Self::Error> {
    let filename = unquote_json(doc.filename, &doc.meta, "document filename")?;
    Ok(Self {
      meta: doc.meta.clone(),
      span: doc.span,
      date: doc.date.to_string(),
      account: doc.account.to_string(),
      filename: resolve_path(&doc.meta.filename, &filename),
      tags_links: doc.tags_links.map(ToString::to_string),
      tags: doc.tags.into_iter().map(ToString::to_string).collect(),
      links: doc.links.into_iter().map(ToString::to_string).collect(),
      comment: doc.comment.map(ToString::to_string),
      key_values: doc
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Custom<'a>> for Custom {
  type Error = ParseError;

  fn try_from(custom: ast::Custom<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: custom.meta.clone(),
      span: custom.span,
      date: custom.date.to_string(),
      name: unquote_json(custom.name, &custom.meta, "custom name")?,
      values: custom
        .values
        .into_iter()
        .map(|v| CustomValue::try_from((v, &custom.meta)))
        .collect::<Result<_, _>>()?,
      comment: custom.comment.map(ToString::to_string),
      key_values: custom
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::OptionDirective<'a>> for OptionDirective {
  type Error = ParseError;

  fn try_from(opt: ast::OptionDirective<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: opt.meta.clone(),
      span: opt.span,
      key: unquote_json(opt.key, &opt.meta, "option key")?,
      value: unquote_json(opt.value, &opt.meta, "option value")?,
    })
  }
}

impl<'a> TryFrom<ast::Include<'a>> for Include {
  type Error = ParseError;

  fn try_from(include: ast::Include<'a>) -> Result<Self, Self::Error> {
    let fname = unquote_json(include.filename, &include.meta, "include filename")?;
    Ok(Self {
      meta: include.meta.clone(),
      span: include.span,
      filename: resolve_path(&include.meta.filename, &fname),
    })
  }
}

impl<'a> TryFrom<ast::Plugin<'a>> for Plugin {
  type Error = ParseError;

  fn try_from(plugin: ast::Plugin<'a>) -> Result<Self, Self::Error> {
    let config = if let Some(raw) = plugin.config {
      match unquote_json(raw, &plugin.meta, "plugin config") {
        Ok(val) => Some(val),
        Err(_) => {
          // Fall back to a lenient stripping of surrounding quotes so
          // that plugins still receive their config string even when it
          // is not valid JSON (e.g. single-quoted Python dicts).
          let trimmed = raw.trim();
          let stripped = trimmed
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(trimmed)
            .to_string();
          Some(stripped)
        }
      }
    } else {
      None
    };

    Ok(Self {
      meta: plugin.meta.clone(),
      span: plugin.span,
      name: unquote_json(plugin.name, &plugin.meta, "plugin name")?,
      config,
    })
  }
}

impl<'a> TryFrom<ast::TagDirective<'a>> for TagDirective {
  type Error = ParseError;

  fn try_from(tag: ast::TagDirective<'a>) -> Result<Self, Self::Error> {
    let tag_value = tag.tag.strip_prefix('#').unwrap_or(tag.tag);
    Ok(Self {
      meta: tag.meta,
      span: tag.span,
      tag: tag_value.to_string(),
    })
  }
}

impl<'a> TryFrom<ast::PushMeta<'a>> for PushMeta {
  type Error = ParseError;

  fn try_from(pm: ast::PushMeta<'a>) -> Result<Self, Self::Error> {
    let meta = pm.meta;
    let value = parse_key_value_value(pm.value, &meta, "pushmeta value", true)?;
    Ok(Self {
      meta,
      span: pm.span,
      key: pm.key.to_string(),
      value,
    })
  }
}

impl<'a> TryFrom<ast::PopMeta<'a>> for PopMeta {
  type Error = ParseError;

  fn try_from(pm: ast::PopMeta<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      meta: pm.meta,
      span: pm.span,
      key: pm.key.to_string(),
    })
  }
}

impl<'a> TryFrom<ast::KeyValue<'a>> for KeyValue {
  type Error = ParseError;

  fn try_from(kv: ast::KeyValue<'a>) -> Result<Self, Self::Error> {
    let value = parse_key_value_value(kv.value, &kv.meta, "metadata value", false)?;

    Ok(Self {
      meta: kv.meta,
      span: kv.span,
      key: kv.key.to_string(),
      value,
    })
  }
}

impl<'a> TryFrom<(ast::CostSpec<'a>, &ast::Meta)> for CostSpec {
  type Error = ParseError;

  fn try_from(input: (ast::CostSpec<'a>, &ast::Meta)) -> Result<Self, Self::Error> {
    let (cost, meta) = input;
    Ok(Self {
      raw: cost.raw.to_string(),
      amount: cost.amount.map(CostAmount::try_from).transpose()?,
      date: cost.date.map(ToString::to_string),
      label: cost
        .label
        .map(|l| unquote_json(l, meta, "cost label"))
        .transpose()?,
      merge: cost.merge,
      is_total: cost.is_total,
    })
  }
}

impl<'a> TryFrom<ast::CostAmount<'a>> for CostAmount {
  type Error = ParseError;

  fn try_from(amount: ast::CostAmount<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      per: amount.per.map(NumberExpr::from),
      total: amount.total.map(NumberExpr::from),
      currency: amount.currency.map(ToString::to_string),
    })
  }
}

impl<'a> TryFrom<ast::Posting<'a>> for Posting {
  type Error = ParseError;

  fn try_from(posting: ast::Posting<'a>) -> Result<Self, Self::Error> {
    let meta = posting.meta;
    let cost_spec = posting
      .cost_spec
      .map(|c| CostSpec::try_from((c, &meta)))
      .transpose()?;
    Ok(Self {
      meta,
      span: posting.span,
      opt_flag: posting.opt_flag.map(ToString::to_string),
      account: posting.account.to_string(),
      amount: posting.amount.map(Amount::try_from).transpose()?,
      cost_spec,
      price_operator: posting.price_operator,
      price_annotation: posting.price_annotation.map(Amount::try_from).transpose()?,
      comment: posting.comment.map(ToString::to_string),
      key_values: posting
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<ast::Transaction<'a>> for Transaction {
  type Error = ParseError;

  fn try_from(txn: ast::Transaction<'a>) -> Result<Self, Self::Error> {
    let meta = txn.meta;
    let payee = txn
      .payee
      .map(|p| unquote_json(p, &meta, "payee"))
      .transpose()?;
    let narration = txn
      .narration
      .map(|n| unquote_json(n, &meta, "narration"))
      .transpose()?;
    Ok(Self {
      meta,
      span: txn.span,
      date: txn.date.to_string(),
      txn: txn.txn.map(ToString::to_string),
      payee,
      narration,
      tags: txn.tags.into_iter().map(ToString::to_string).collect(),
      links: txn.links.into_iter().map(ToString::to_string).collect(),
      key_values: txn
        .key_values
        .into_iter()
        .map(KeyValue::try_from)
        .collect::<Result<_, _>>()?,
      postings: txn
        .postings
        .into_iter()
        .map(Posting::try_from)
        .collect::<Result<_, _>>()?,
    })
  }
}

impl<'a> TryFrom<(ast::CustomValue<'a>, &ast::Meta)> for CustomValue {
  type Error = ParseError;

  fn try_from(val_meta: (ast::CustomValue<'a>, &ast::Meta)) -> Result<Self, Self::Error> {
    let (value, meta) = val_meta;
    let content = match value.kind {
      ast::CustomValueKind::String => {
        let parsed = unquote_json(value.raw, meta, "custom value")?;
        CustomValue::String(parsed)
      }
      ast::CustomValueKind::Date => {
        let parsed = parse_date_value(value.raw, meta, "custom value date")?;
        CustomValue::Date(parsed)
      }
      ast::CustomValueKind::Bool => {
        let parsed = parse_bool_value(value.raw, meta, "custom value bool")?;
        CustomValue::Bool(parsed)
      }
      ast::CustomValueKind::Amount => {
        let amount = value
          .amount
          .ok_or_else(|| value_error(meta, "invalid amount"))?;
        CustomValue::Amount(Amount::try_from(amount)?)
      }
      ast::CustomValueKind::Number => {
        let number = value
          .number
          .ok_or_else(|| value_error(meta, "missing number expression"))?;
        CustomValue::Number(NumberExpr::from(number))
      }
      ast::CustomValueKind::Account => CustomValue::Account(value.raw.trim().to_string()),
    };

    Ok(content)
  }
}

impl<'a> TryFrom<ast::Amount<'a>> for Amount {
  type Error = ParseError;

  fn try_from(amount: ast::Amount<'a>) -> Result<Self, Self::Error> {
    Ok(Self {
      raw: amount.raw.to_string(),
      number: NumberExpr::from(amount.number),
      currency: amount.currency.map(ToString::to_string),
    })
  }
}

fn unquote_json(raw: &str, meta: &ast::Meta, ctx: &str) -> Result<String, ParseError> {
  match parse_json::<String>(raw) {
    Ok(value) => Ok(value),
    Err(err) => {
      // Allow literal newlines inside quoted strings (e.g. multi-line queries) by
      // falling back to a lenient unescaper when JSON parsing rejects control chars.
      if raw.starts_with('"') && raw.ends_with('"') {
        let inner = &raw[1..raw.len() - 1];
        let mut out = String::with_capacity(inner.len());
        let mut chars = inner.chars().peekable();

        while let Some(ch) = chars.next() {
          if ch == '\\' {
            if let Some(next) = chars.next() {
              match next {
                '"' => out.push('"'),
                '\\' => out.push('\\'),
                'n' => out.push('\n'),
                'r' => out.push('\r'),
                't' => out.push('\t'),
                other => {
                  out.push('\\');
                  out.push(other);
                }
              }
            } else {
              out.push('\\');
            }
          } else {
            out.push(ch);
          }
        }

        return Ok(out);
      }

      Err(ParseError {
        filename: meta.filename.clone(),
        line: meta.line,
        column: meta.column,
        message: format!("invalid {}: {}", ctx, err),
      })
    }
  }
}
