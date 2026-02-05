use chumsky::span::SimpleSpan;
use smallvec::SmallVec;
use std::sync::Arc;

#[cfg(feature = "serde")]
use serde::Serialize;

#[cfg(feature = "serde")]
fn serialize_arc_str<S>(value: &std::sync::Arc<String>, serializer: S) -> Result<S::Ok, S::Error>
where
  S: serde::Serializer,
{
  serializer.serialize_str(value.as_str())
}
/// Byte offsets in the original source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

impl From<SimpleSpan> for Span {
  fn from(value: SimpleSpan) -> Self {
    Span::from_simple_span(value)
  }
}

impl From<&SimpleSpan> for Span {
  fn from(value: &SimpleSpan) -> Self {
    Span::from_range(value.start, value.end)
  }
}

impl Span {
  pub fn from_simple_span(e: SimpleSpan) -> Self {
    Self::from_range(e.start, e.end)
  }

  pub fn from_range(start: usize, end: usize) -> Self {
    Self { start, end }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct WithSpan<T> {
  pub span: Span,
  pub content: T,
}

impl<T> WithSpan<T> {
  pub fn new(span: Span, content: T) -> Self {
    Self { span, content }
  }

  pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithSpan<U> {
    WithSpan {
      span: self.span,
      content: f(self.content),
    }
  }
}

/// Source location info attached to each top-level directive.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Meta {
  #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_arc_str"))]
  pub filename: Arc<String>,
  /// 1-based line number.
  pub line: usize,
  /// 1-based column number.
  pub column: usize,
}

/// A typed representation of top-level Beancount directives/entries.
///
/// This is intentionally lossy at first: we keep `span`/`raw` so we can fall back
/// while we incrementally implement full formatting rules.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum Directive<'a> {
  Open(Open<'a>),
  Close(Close<'a>),
  Balance(Balance<'a>),
  Pad(Pad<'a>),
  Transaction(Transaction<'a>),

  Commodity(Commodity<'a>),
  Price(Price<'a>),
  Event(Event<'a>),
  Query(Query<'a>),
  Note(Note<'a>),
  Document(Document<'a>),
  Custom(Custom<'a>),

  Option(OptionDirective<'a>),
  Include(Include<'a>),
  Plugin(Plugin<'a>),
  PushTag(TagDirective<'a>),
  PopTag(TagDirective<'a>),
  PushMeta(PushMeta<'a>),
  PopMeta(PopMeta<'a>),

  /// Line-level comment.
  Comment(Comment<'a>),
  /// Org/Markdown style headline.
  Headline(Headline<'a>),
  /// Unparsed directive line.
  Raw(Raw<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Open<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub currencies: SmallVec<[WithSpan<&'a str>; 2]>,
  pub opt_booking: Option<WithSpan<&'a str>>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Close<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Balance<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub amount: Amount<'a>,
  pub tolerance: Option<WithSpan<&'a str>>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Pad<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub from_account: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Transaction<'a> {
  pub span: Span,
  pub date: WithSpan<&'a str>,
  /// Transaction flag/token (e.g. `*`, `!`) when present.
  pub txn: Option<WithSpan<&'a str>>,
  /// quoted
  pub payee: Option<WithSpan<&'a str>>,
  /// quoted
  pub narration: Option<WithSpan<&'a str>>,
  pub tags_links: Option<Vec<WithSpan<&'a str>>>,
  pub tags: SmallVec<[WithSpan<&'a str>; 2]>,
  pub links: SmallVec<[WithSpan<&'a str>; 2]>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
  pub postings: SmallVec<[Posting<'a>; 4]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PriceOperator {
  /// `@` price operator (per-unit price).
  PerUnit,
  /// `@@` price operator (total price).
  Total,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
pub enum KeyValueValue<'a> {
  String(&'a str),
  UnquotedString(&'a str),
  Date(&'a str),
  Bool(bool),
  Raw(&'a str),
}

impl<'a> KeyValueValue<'a> {
  pub fn as_str(&self) -> std::borrow::Cow<'a, str> {
    match self {
      KeyValueValue::String(s) | KeyValueValue::UnquotedString(s) | KeyValueValue::Raw(s) => {
        std::borrow::Cow::Borrowed(*s)
      }
      KeyValueValue::Date(s) => std::borrow::Cow::Borrowed(*s),
      KeyValueValue::Bool(val) => std::borrow::Cow::Owned(val.to_string()),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
pub enum NumberExpr<'a> {
  Missing {
    span: Span,
  },
  Literal(WithSpan<&'a str>),
  Binary {
    span: Span,
    left: Box<NumberExpr<'a>>,
    op: WithSpan<BinaryOp>,
    right: Box<NumberExpr<'a>>,
  },
}

impl NumberExpr<'_> {
  pub fn span(&self) -> Span {
    match self {
      NumberExpr::Missing { span } => *span,
      NumberExpr::Literal(value) => value.span,
      NumberExpr::Binary { span, .. } => *span,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct KeyValue<'a> {
  pub span: Span,
  pub key: WithSpan<&'a str>,
  pub value: Option<WithSpan<KeyValueValue<'a>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CostAmount<'a> {
  pub per: Option<NumberExpr<'a>>,
  pub total: Option<NumberExpr<'a>>,
  pub currency: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CostSpec<'a> {
  pub raw: WithSpan<&'a str>,
  pub amount: Option<CostAmount<'a>>,
  pub date: Option<WithSpan<&'a str>>,
  pub label: Option<WithSpan<&'a str>>,
  pub merge: Option<WithSpan<bool>>,
  pub is_total: WithSpan<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Posting<'a> {
  pub span: Span,
  pub opt_flag: Option<WithSpan<&'a str>>,
  pub account: WithSpan<&'a str>,
  pub amount: Option<Amount<'a>>,
  pub cost_spec: Option<CostSpec<'a>>,
  pub price_operator: Option<WithSpan<PriceOperator>>,
  pub price_annotation: Option<Amount<'a>>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Commodity<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub currency: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Price<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub currency: WithSpan<&'a str>,
  pub amount: Amount<'a>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Event<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub event_type: WithSpan<&'a str>,
  pub desc: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Query<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub name: WithSpan<&'a str>,
  pub query: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Note<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub note: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Document<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub filename: WithSpan<&'a str>,
  pub tags_links: Option<Vec<WithSpan<&'a str>>>,
  pub tags: SmallVec<[WithSpan<&'a str>; 2]>,
  pub links: SmallVec<[WithSpan<&'a str>; 2]>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Custom<'a> {
  pub span: Span,
  pub keyword: Span,
  pub date: WithSpan<&'a str>,
  pub name: WithSpan<&'a str>,
  pub values: SmallVec<[CustomValue<'a>; 2]>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum CustomValueKind {
  String,
  Date,
  Bool,
  Amount,
  Number,
  Account,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CustomValue<'a> {
  pub raw: WithSpan<&'a str>,
  pub kind: CustomValueKind,
  pub number: Option<NumberExpr<'a>>,
  pub amount: Option<Amount<'a>>,
}

/// Parsed amount token with number and currency captured separately.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Amount<'a> {
  pub raw: WithSpan<&'a str>,
  pub number: NumberExpr<'a>,
  pub currency: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct OptionDirective<'a> {
  pub span: Span,
  pub keyword: Span,
  pub key: WithSpan<&'a str>,
  pub value: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Include<'a> {
  pub span: Span,
  pub keyword: Span,
  pub filename: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Plugin<'a> {
  pub span: Span,
  pub keyword: Span,
  pub name: WithSpan<&'a str>,
  pub config: Option<WithSpan<&'a str>>,
  pub comment: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TagDirective<'a> {
  pub span: Span,
  pub keyword: Span,
  pub tag: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PushMeta<'a> {
  pub span: Span,
  pub keyword: Span,
  pub key: WithSpan<&'a str>,
  pub value: Option<WithSpan<KeyValueValue<'a>>>,
  pub comment: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PopMeta<'a> {
  pub span: Span,
  pub keyword: Span,
  pub key: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Comment<'a> {
  pub span: Span,
  pub text: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Headline<'a> {
  pub span: Span,
  pub text: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Raw<'a> {
  pub span: Span,
  pub text: &'a str,
}
