use beancount_parser::{ParseError, ast};
use serde_json::from_str as parse_json;
use smallvec::SmallVec;
use std::convert::TryFrom;
use std::path::Path;

pub(crate) type SmallStrVec = SmallVec<[String; 4]>;
pub(crate) type SmallKeyValues = SmallVec<[KeyValue; 4]>;
pub(crate) type SmallPostings = SmallVec<[Posting; 4]>;
pub(crate) type SmallCustomValues = SmallVec<[CustomValue; 2]>;

#[derive(Debug, Clone)]
pub(crate) enum CoreDirective {
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
    Pushmeta(Pushmeta),
    Popmeta(Popmeta),
    Raw(Raw),
}

#[derive(Debug, Clone)]
pub(crate) struct Raw {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub kind: String,
    pub text: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Open {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub account: String,
    pub currencies: SmallStrVec,
    pub opt_booking: Option<String>,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Close {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub account: String,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Balance {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub account: String,
    pub amount: Amount,
    pub tolerance: Option<String>,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Pad {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub account: String,
    pub from_account: String,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Transaction {
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

#[derive(Debug, Clone)]
pub(crate) struct Posting {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub opt_flag: Option<String>,
    pub account: String,
    pub amount: Option<Amount>,
    pub cost_spec: Option<CostSpec>,
    pub price_operator: Option<String>,
    pub price_annotation: Option<Amount>,
    pub comment: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct Commodity {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub currency: String,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Price {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub currency: String,
    pub amount: Amount,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Event {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub event_type: String,
    pub desc: String,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Query {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub name: String,
    pub query: String,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Note {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub account: String,
    pub note: String,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct Document {
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

#[derive(Debug, Clone)]
pub(crate) struct Custom {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub date: String,
    pub name: String,
    pub values: SmallCustomValues,
    pub comment: Option<String>,
    pub key_values: SmallKeyValues,
}

#[derive(Debug, Clone)]
pub(crate) struct CustomValue {
    pub raw: String,
    pub kind: ast::CustomValueKind,
    pub string: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct OptionDirective {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub key: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Include {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub filename: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Plugin {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub name: String,
    pub config: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct TagDirective {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub tag: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Pushmeta {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub key_value: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Popmeta {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub key: String,
}

#[derive(Debug, Clone)]
pub(crate) struct KeyValue {
    pub meta: ast::Meta,
    pub span: ast::Span,
    pub key: String,
    pub value: Option<KeyValueValue>,
}

#[derive(Debug, Clone)]
pub(crate) enum KeyValueValue {
    String(String),
    Raw(String),
}

#[derive(Debug, Clone)]
pub(crate) struct CostAmount {
    pub per: Option<String>,
    pub total: Option<String>,
    pub currency: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct CostSpec {
    pub raw: String,
    pub amount: Option<CostAmount>,
    pub date: Option<String>,
    pub label: Option<String>,
    pub merge: bool,
    pub is_total: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct Amount {
    pub raw: String,
    pub number: String,
    pub currency: String,
}

pub(crate) fn normalize_directives<'a>(
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
            ast::Directive::Balance(balance) => {
                Ok(CoreDirective::Balance(Balance::try_from(balance)?))
            }
            ast::Directive::Pad(pad) => Ok(CoreDirective::Pad(Pad::try_from(pad)?)),
            ast::Directive::Transaction(txn) => {
                Ok(CoreDirective::Transaction(Transaction::try_from(txn)?))
            }
            ast::Directive::Commodity(cmdty) => {
                Ok(CoreDirective::Commodity(Commodity::try_from(cmdty)?))
            }
            ast::Directive::Price(price) => Ok(CoreDirective::Price(Price::try_from(price)?)),
            ast::Directive::Event(event) => Ok(CoreDirective::Event(Event::try_from(event)?)),
            ast::Directive::Query(query) => Ok(CoreDirective::Query(Query::try_from(query)?)),
            ast::Directive::Note(note) => Ok(CoreDirective::Note(Note::try_from(note)?)),
            ast::Directive::Document(doc) => Ok(CoreDirective::Document(Document::try_from(doc)?)),
            ast::Directive::Custom(custom) => Ok(CoreDirective::Custom(Custom::try_from(custom)?)),
            ast::Directive::Option(opt) => {
                Ok(CoreDirective::Option(OptionDirective::try_from(opt)?))
            }
            ast::Directive::Include(include) => {
                Ok(CoreDirective::Include(Include::try_from(include)?))
            }
            ast::Directive::Plugin(plugin) => Ok(CoreDirective::Plugin(Plugin::try_from(plugin)?)),
            ast::Directive::Pushtag(tag) => {
                Ok(CoreDirective::Pushtag(TagDirective::try_from(tag)?))
            }
            ast::Directive::Poptag(tag) => Ok(CoreDirective::Poptag(TagDirective::try_from(tag)?)),
            ast::Directive::Pushmeta(pm) => Ok(CoreDirective::Pushmeta(Pushmeta::try_from(pm)?)),
            ast::Directive::Popmeta(pm) => Ok(CoreDirective::Popmeta(Popmeta::try_from(pm)?)),
            ast::Directive::Raw(raw) => Ok(CoreDirective::Raw(Raw::try_from(raw)?)),
        }
    }
}

impl<'a> TryFrom<ast::Raw<'a>> for Raw {
    type Error = ParseError;

    fn try_from(raw: ast::Raw<'a>) -> Result<Self, Self::Error> {
        Ok(Self {
            meta: raw.meta,
            span: raw.span,
            kind: raw.kind.to_string(),
            text: raw.text.to_string(),
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
            event_type: event.event_type.to_string(),
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
            name: query.name.to_string(),
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
            name: custom.name.to_string(),
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
        Ok(Self {
            meta: plugin.meta.clone(),
            span: plugin.span,
            name: unquote_json(plugin.name, &plugin.meta, "plugin name")?,
            config: plugin
                .config
                .map(|c| unquote_json(c, &plugin.meta, "plugin config"))
                .transpose()?,
        })
    }
}

impl<'a> TryFrom<ast::TagDirective<'a>> for TagDirective {
    type Error = ParseError;

    fn try_from(tag: ast::TagDirective<'a>) -> Result<Self, Self::Error> {
        Ok(Self {
            meta: tag.meta,
            span: tag.span,
            tag: tag.tag.to_string(),
        })
    }
}

impl<'a> TryFrom<ast::Pushmeta<'a>> for Pushmeta {
    type Error = ParseError;

    fn try_from(pm: ast::Pushmeta<'a>) -> Result<Self, Self::Error> {
        Ok(Self {
            meta: pm.meta,
            span: pm.span,
            key_value: pm.key_value.to_string(),
        })
    }
}

impl<'a> TryFrom<ast::Popmeta<'a>> for Popmeta {
    type Error = ParseError;

    fn try_from(pm: ast::Popmeta<'a>) -> Result<Self, Self::Error> {
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
        let value = kv
            .value
            .map(|v| match v {
                ast::KeyValueValue::String(raw) => {
                    unquote_json(raw, &kv.meta, "metadata value").map(KeyValueValue::String)
                }
                ast::KeyValueValue::Raw(raw) => Ok(KeyValueValue::Raw(raw.to_string())),
            })
            .transpose()?;

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
            per: amount.per.map(ToString::to_string),
            total: amount.total.map(ToString::to_string),
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
            price_operator: posting.price_operator.map(ToString::to_string),
            price_annotation: posting.price_annotation.map(Amount::try_from).transpose()?,
            comment: posting.comment.map(ToString::to_string),
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
        let string = match value.kind {
            ast::CustomValueKind::String => Some(unquote_json(value.raw, meta, "custom value")?),
            _ => None,
        };

        Ok(Self {
            raw: value.raw.to_string(),
            kind: value.kind,
            string,
        })
    }
}

impl<'a> TryFrom<ast::Amount<'a>> for Amount {
    type Error = ParseError;

    fn try_from(amount: ast::Amount<'a>) -> Result<Self, Self::Error> {
        Ok(Self {
            raw: amount.raw.to_string(),
            number: amount.number.to_string(),
            currency: amount.currency.to_string(),
        })
    }
}

fn unquote_json(raw: &str, meta: &ast::Meta, ctx: &str) -> Result<String, ParseError> {
    parse_json::<String>(raw).map_err(|err| ParseError {
        filename: meta.filename.clone(),
        line: meta.line,
        column: meta.column,
        message: format!("invalid {}: {}", ctx, err),
    })
}

fn resolve_path(base_filename: &str, filename: &str) -> String {
    let path = Path::new(filename);
    if path.is_absolute() {
        return filename.to_string();
    }

    let mut base_dir = Path::new(base_filename)
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .to_path_buf();

    if (base_dir.as_os_str().is_empty() || base_filename.starts_with('<'))
        && let Ok(cwd) = std::env::current_dir()
    {
        base_dir = cwd;
    }

    base_dir.join(path).to_string_lossy().into_owned()
}
