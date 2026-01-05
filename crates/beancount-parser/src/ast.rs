/// Byte offsets in the original source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

impl Span {
  pub fn from_range(start: usize, end: usize) -> Self {
    Self { start, end }
  }
}

/// Source location info attached to each top-level directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Meta {
  pub filename: String,
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
  Pushtag(TagDirective<'a>),
  Poptag(TagDirective<'a>),
  Pushmeta(Pushmeta<'a>),
  Popmeta(Popmeta<'a>),

  /// Any entry/directive we don't parse yet.
  Raw(Raw<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Raw<'a> {
  pub meta: Meta,
  pub kind: &'a str,
  pub span: Span,
  pub text: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Open<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub account: &'a str,
  pub currencies: Vec<&'a str>,
  pub opt_booking: Option<&'a str>,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Close<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub account: &'a str,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Balance<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub account: &'a str,
  pub amount: Amount<'a>,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pad<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub account: &'a str,
  pub from_account: &'a str,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transaction<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  /// Transaction flag/token (e.g. `*`, `!`) when present.
  pub txn: Option<&'a str>,
  pub payee: Option<String>,
  pub narration: String,
  pub tags_links: Option<&'a str>,
  pub tags: Vec<&'a str>,
  pub links: Vec<&'a str>,
  pub comment: Option<&'a str>,
  /// All tag/link groups attached to the transaction (inline and indented lines).
  pub tags_links_lines: Vec<&'a str>,
  /// All comments attached to the transaction (inline and indented lines).
  pub comments: Vec<&'a str>,
  /// Metadata key/value lines attached to the transaction.
  pub key_values: Vec<KeyValue<'a>>,
  /// Postings within the transaction, in source order.
  pub postings: Vec<Posting<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyValueValue<'a> {
  String(String),
  Raw(&'a str),
}

impl<'a> KeyValueValue<'a> {
  pub fn as_str(&'a self) -> std::borrow::Cow<'a, str> {
    match self {
      KeyValueValue::String(s) => std::borrow::Cow::Owned(s.clone()),
      KeyValueValue::Raw(r) => std::borrow::Cow::Borrowed(r),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValue<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: &'a str,
  pub value: KeyValueValue<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Posting<'a> {
  pub meta: Meta,
  pub span: Span,
  pub opt_flag: Option<&'a str>,
  pub account: &'a str,
  pub amount: Option<Amount<'a>>,
  pub cost_spec: Option<&'a str>,
  pub price_operator: Option<&'a str>,
  pub price_annotation: Option<Amount<'a>>,
  pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Commodity<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub currency: &'a str,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Price<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub currency: &'a str,
  pub amount: Amount<'a>,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Event<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub event_type: &'a str,
  pub desc: &'a str,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub name: &'a str,
  pub query: &'a str,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Note<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub account: &'a str,
  pub note: &'a str,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub account: &'a str,
  pub filename: &'a str,
  pub tags_links: Option<&'a str>,
  pub tags: Vec<&'a str>,
  pub links: Vec<&'a str>,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Custom<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: &'a str,
  pub name: &'a str,
  pub values: Vec<&'a str>,
  pub comment: Option<&'a str>,
  pub key_values: Vec<KeyValue<'a>>,
}

/// Parsed amount token with number and currency captured separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount<'a> {
  pub raw: &'a str,
  pub number: &'a str,
  pub currency: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OptionDirective<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: &'a str,
  pub value: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Include<'a> {
  pub meta: Meta,
  pub span: Span,
  pub filename: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Plugin<'a> {
  pub meta: Meta,
  pub span: Span,
  pub name: &'a str,
  pub config: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagDirective<'a> {
  pub meta: Meta,
  pub span: Span,
  pub tag: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pushmeta<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key_value: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Popmeta<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: &'a str,
}
