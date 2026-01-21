use smallvec::SmallVec;
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
  PushTag(TagDirective<'a>),
  PopTag(TagDirective<'a>),
  PushMeta(PushMeta<'a>),
  PopMeta(PopMeta<'a>),

  /// Line-level comment.
  Comment(Comment<'a>),
  /// Org/Markdown style headline.
  Headline(Headline<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Open<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub currencies: SmallVec<[WithSpan<&'a str>; 8]>,
  pub opt_booking: Option<WithSpan<&'a str>>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Close<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Balance<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub amount: Amount<'a>,
  pub tolerance: Option<WithSpan<&'a str>>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pad<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub from_account: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transaction<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  /// Transaction flag/token (e.g. `*`, `!`) when present.
  pub txn: Option<WithSpan<&'a str>>,
  pub payee: Option<WithSpan<&'a str>>,
  pub narration: Option<WithSpan<&'a str>>,
  pub tags_links: Option<WithSpan<&'a str>>,
  pub tags: SmallVec<[WithSpan<&'a str>; 2]>,
  pub links: SmallVec<[WithSpan<&'a str>; 2]>,
  pub comment: Option<WithSpan<&'a str>>,
  pub tags_links_lines: SmallVec<[WithSpan<&'a str>; 8]>,
  pub comments: SmallVec<[WithSpan<&'a str>; 8]>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
  pub postings: SmallVec<[Posting<'a>; 4]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PriceOperator {
  /// `@` price operator (per-unit price).
  PerUnit,
  /// `@@` price operator (total price).
  Total,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl<'a> NumberExpr<'a> {
  pub fn span(&self) -> Span {
    match self {
      NumberExpr::Missing { span }
      | NumberExpr::Literal(WithSpan { span, .. })
      | NumberExpr::Binary { span, .. } => *span,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValue<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: WithSpan<&'a str>,
  pub value: Option<WithSpan<KeyValueValue<'a>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostAmount<'a> {
  pub per: Option<NumberExpr<'a>>,
  pub total: Option<NumberExpr<'a>>,
  pub currency: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostSpec<'a> {
  pub raw: WithSpan<&'a str>,
  pub amount: Option<CostAmount<'a>>,
  pub date: Option<WithSpan<&'a str>>,
  pub label: Option<WithSpan<&'a str>>,
  pub merge: Option<WithSpan<bool>>,
  pub is_total: WithSpan<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Posting<'a> {
  pub meta: Meta,
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
pub struct Commodity<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub currency: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Price<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub currency: WithSpan<&'a str>,
  pub amount: Amount<'a>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Event<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub event_type: WithSpan<&'a str>,
  pub desc: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Query<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub name: WithSpan<&'a str>,
  pub query: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Note<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub note: WithSpan<&'a str>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub account: WithSpan<&'a str>,
  pub filename: WithSpan<&'a str>,
  pub tags_links: Option<WithSpan<&'a str>>,
  pub tags: SmallVec<[WithSpan<&'a str>; 2]>,
  pub links: SmallVec<[WithSpan<&'a str>; 2]>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Custom<'a> {
  pub meta: Meta,
  pub span: Span,
  pub date: WithSpan<&'a str>,
  pub name: WithSpan<&'a str>,
  pub values: SmallVec<[CustomValue<'a>; 2]>,
  pub comment: Option<WithSpan<&'a str>>,
  pub key_values: SmallVec<[KeyValue<'a>; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CustomValueKind {
  String,
  Date,
  Bool,
  Amount,
  Number,
  Account,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomValue<'a> {
  pub raw: WithSpan<&'a str>,
  pub kind: CustomValueKind,
  pub number: Option<NumberExpr<'a>>,
  pub amount: Option<Amount<'a>>,
}

/// Parsed amount token with number and currency captured separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount<'a> {
  pub raw: WithSpan<&'a str>,
  pub number: NumberExpr<'a>,
  pub currency: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OptionDirective<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: WithSpan<&'a str>,
  pub value: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Include<'a> {
  pub meta: Meta,
  pub span: Span,
  pub filename: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Plugin<'a> {
  pub meta: Meta,
  pub span: Span,
  pub name: WithSpan<&'a str>,
  pub config: Option<WithSpan<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagDirective<'a> {
  pub meta: Meta,
  pub span: Span,
  pub tag: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PushMeta<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: WithSpan<&'a str>,
  pub value: Option<WithSpan<KeyValueValue<'a>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PopMeta<'a> {
  pub meta: Meta,
  pub span: Span,
  pub key: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comment<'a> {
  pub meta: Meta,
  pub span: Span,
  pub text: WithSpan<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Headline<'a> {
  pub meta: Meta,
  pub span: Span,
  pub text: WithSpan<&'a str>,
}
