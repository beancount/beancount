//! struct constructor order matter in this file.
//! It decide thr order to consume lexer tokens.
//!
use crate::data::{
    Amount, Balance, Commodity, CostSpec, Document, Event, Opt, Pad, Posting, PostingCost, Price,
    Query, Transaction,
};
use crate::parser::{MyParser, Rule};
use pyo3::prelude::*;
use rust_decimal::Decimal;

use crate::data::{self, Close, Custom, Metadata, Note, Open, Plugin};
use crate::error::{ParseError, ParseResult};
use crate::ParserError;
use chrono::NaiveDate;
use core::num;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use pyo3::prelude::*;
use std::backtrace::Backtrace;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::vec;

#[pyclass]
pub struct File {
    #[pyo3(get)]
    // we force beancount format to be valid utf8
    pub includes: Vec<String>,

    #[pyo3(get)]
    pub options: Vec<data::Opt>,

    #[pyo3(get)]
    pub directives: Vec<Directive>,
}

// #[pyclass]
#[derive(Debug, Clone)]
pub enum Directive {
    Open(data::Open),
    Close(data::Close),
    Commodity(data::Commodity),
    Transaction(data::Transaction),
    Pad(data::Pad),
    Balance(data::Balance),
    Price(data::Price),
    Event(data::Event),
    Plugin(data::Plugin),
    Option(data::Opt),
    Custom(data::Custom),
    Note(data::Note),
    Document(data::Document),
    Query(data::Query),
    // S(String),
}

impl IntoPy<Py<PyAny>> for Directive {
    fn into_py(self, py: Python) -> Py<PyAny> {
        match self {
            Directive::Open(x) => x.into_py(py),
            Directive::Close(x) => x.into_py(py),
            Directive::Commodity(x) => x.into_py(py),
            Directive::Custom(x) => x.into_py(py),
            Directive::Transaction(x) => x.into_py(py),
            Directive::Pad(x) => x.into_py(py),
            Directive::Balance(x) => x.into_py(py),
            Directive::Price(x) => x.into_py(py),
            Directive::Event(x) => x.into_py(py),
            Directive::Plugin(x) => x.into_py(py),
            Directive::Option(x) => x.into_py(py),
            Directive::Note(x) => x.into_py(py),
            Directive::Document(x) => x.into_py(py),
            Directive::Query(x) => x.into_py(py),
            // Directive::S(x) => x.into_py(py),
        }
    }
}

#[derive(Debug, Default)]
struct ParseState<'i> {
    // Track pushed tag count with HashMap<&str, u64> instead of only tracking
    // tags with HashSet<&str> because the spec allows pushing multiple of the
    // same tag, and conformance with bean-check requires an equal number of
    // pops.
    pushed_tags: HashMap<&'i str, u16>,
}

impl<'i> ParseState<'i> {
    fn push_tag(&mut self, tag: &'i str) {
        *self.pushed_tags.entry(tag).or_insert(0) += 1;
    }

    fn pop_tag(&mut self, tag: &str) -> Result<(), String> {
        match self.pushed_tags.get_mut(tag) {
            Some(count) => {
                if *count <= 1 {
                    self.pushed_tags.remove(tag);
                } else {
                    *count -= 1;
                }
                Ok(())
            }
            _ => Err(format!("Attempting to pop absent tag: '{}'", tag)),
        }
    }

    fn get_pushed_tags(&self) -> impl Iterator<Item = &&str> {
        self.pushed_tags.keys()
    }
}

fn extract_tag<'i>(pair: Pair<'i, Rule>) -> ParseResult<&'i str> {
    let mut pairs = pair.into_inner();
    let pair = pairs
        .next()
        .ok_or_else(|| ParseError::invalid_state("tag"))?;
    Ok(&pair.as_str()[1..])
}

#[pyfunction(name = "parse")]
pub fn py_parse(content: &str) -> PyResult<File> {
    return parse(content).or_else(|err| Err(ParserError::new_err(format!("{:#?}", err))));
}

pub fn parse(content: &str) -> ParseResult<File> {
    let entries = MyParser::parse(Rule::file, content)?
        .next()
        .ok_or_else(|| ParseError::invalid_state("non-empty parse result"))?;

    let mut state = ParseState {
        // root_names: ["Assets", "Liabilities", "Equity", "Income", "Expenses"]
        //     .iter()
        //     .map(|ty| (ty.to_string(), ty.to_string()))
        //     .collect(),
        pushed_tags: HashMap::new(),
    };

    let mut directives = Vec::new();

    let mut includes = Vec::new();

    for entry in entries.into_inner() {
        match entry.as_rule() {
            Rule::EOI => {
                let pushed_tags = state
                    .get_pushed_tags()
                    .map(|s| format!("'{}'", s))
                    .collect::<Vec<String>>()
                    .join(", ");
                if !pushed_tags.is_empty() {
                    return Err(ParseError::invalid_input_with_span(
                        format!("Unbalanced pushed tag(s): {}", pushed_tags),
                        entry.as_span(),
                    ));
                }
                break;
            }
            Rule::pushtag => {
                state.push_tag(extract_tag(entry)?);
            }
            Rule::poptag => {
                let span = entry.as_span();
                if let Err(msg) = state.pop_tag(extract_tag(entry)?) {
                    return Err(ParseError::invalid_input_with_span(msg, span));
                }
            }
            Rule::include => {
                includes.push(get_quoted_str(entry.into_inner().next().unwrap())?);
            }
            _ => {
                let lino = entry.line_col().0;
                let mut dir = directive(entry, &state)?;

                match &mut dir {
                    Directive::Open(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Close(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Commodity(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Transaction(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Pad(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Balance(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Price(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Event(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Plugin(ref mut dir) => {
                        // dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Option(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Custom(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Note(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Document(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                    Directive::Query(ref mut dir) => {
                        dir.meta.insert("lineno".into(), lino.to_string());
                    }
                }

                directives.push(dir);
            }
        }
    }

    return Ok(File {
        includes,
        options: vec![],
        directives,
    });
}

fn directive(directive: Pair<Rule>, state: &ParseState) -> ParseResult<Directive> {
    let dir = match directive.as_rule() {
        Rule::option => option_directive(directive)?,
        Rule::plugin => plugin_directive(directive)?,
        Rule::custom => custom_directive(directive, state)?,
        Rule::open => open_directive(directive, state)?,
        Rule::close => close_directive(directive, state)?,
        Rule::commodity_directive => commodity_directive(directive, state)?,
        Rule::note => note_directive(directive, state)?,
        Rule::pad => pad_directive(directive, state)?,
        Rule::query => query_directive(directive, state)?,
        Rule::balance => balance_directive(directive, state)?,
        Rule::event => event_directive(directive, state)?,
        Rule::document => document_directive(directive, state)?,
        Rule::price => price_directive(directive, state)?,
        Rule::transaction => transaction_directive(directive, state)?,
        _ => panic!("unexpected directive {:#?}", directive),
    };
    Ok(dir)
}

// 2014-05-05 * "Transfer from Savings account"
//   Assets:MyBank:Checking            -400.00 USD
//   ! Assets:MyBank:Savings

/*
 2014-10-05 * "Costco" "Shopping for birthday"
 Liabilities:CreditCard:CapitalOne         -45.00          USD
 Assets:AccountsReceivable:John            ((40.00/3) + 5) USD
 Assets:AccountsReceivable:Michael         40.00/3         USD
 Expenses:Shopping
*/
fn transaction_directive<'i>(
    directive: Pair<'i, Rule>,
    state: &ParseState,
) -> ParseResult<Directive> {
    let _span = directive.as_span();
    let source = directive.as_str();
    let mut pairs = directive.into_inner();

    let date = date(pairs.next().unwrap())?;
    let flag: char = pairs.next().unwrap().as_str().chars().nth(0).unwrap();

    let pair = pairs.next().ok_or_else(|| {
        ParseError::invalid_state_with_span(stringify!((payee, narration)), _span.clone())
    })?;
    let (payee, narration) = {
        let span = pair.as_span();
        let mut inner = pair.into_inner();
        let first = inner
            .next()
            .map(get_quoted_str)
            .transpose()?
            .ok_or_else(|| ParseError::invalid_state_with_span("payee or narration", span))?;
        let second = inner.next().map(get_quoted_str);
        if let Some(second) = second {
            (Some(first), second?)
        } else {
            (None, first)
        }
    };

    let (mut tags, mut links) = match pairs.peek() {
        Some(ref p) if p.as_rule() == Rule::tags_links => {
            let pair = pairs.next().ok_or_else(|| {
                ParseError::invalid_state_with_span(stringify!($field), _span.clone())
            })?;
            {
                tags_links(pair)?
            }
        }
        _ => (HashSet::new(), HashSet::new()),
    };

    let pair = pairs.next().ok_or_else(|| {
        ParseError::invalid_state_with_span(stringify!((meta, postings)), _span.clone())
    })?;
    let (meta, postings) = {
        let mut postings: Vec<Posting> = Vec::new();
        let mut tx_meta = Metadata::new();
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::posting => {
                    postings.push(posting(p, state)?);
                }
                Rule::key_value => {
                    let (k, v) = meta_kv_pair(p, state)?;
                    tx_meta.insert(k.to_string(), v);
                }
                Rule::tag => {
                    let tag = (&p.as_str()[1..]).into();
                    tags.insert(tag);
                }
                Rule::link => {
                    let link = (&p.as_str()[1..]).into();
                    links.insert(link);
                }
                rule => {
                    unimplemented!("rule {:?}", rule);
                }
            }
        }
        for tag in state.get_pushed_tags() {
            tags.insert(tag.to_string());
        }
        (tx_meta, postings)
    };

    Ok(Directive::Transaction(Transaction {
        date,
        flag,
        payee,
        narration,
        tags,
        links,
        meta,
        postings,
    }))
}

fn meta_kv<'i>(pair: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Metadata> {
    debug_assert!(pair.as_rule() == Rule::eol_kv_list);
    pair.into_inner()
        .map(|p| meta_kv_pair(p, state))
        .map(|p| match p {
            Ok((k, v)) => Ok((k.to_string(), v)),
            Err(e) => Err(e),
        })
        .collect::<ParseResult<Metadata>>()
}

fn meta_kv_pair<'i>(
    pair: Pair<'i, Rule>,
    state: &ParseState,
) -> ParseResult<(Cow<'i, str>, String)> {
    debug_assert!(pair.as_rule() == Rule::key_value);
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    let key = inner
        .next()
        .ok_or_else(|| ParseError::invalid_state_with_span("metadata key", span.clone()))?
        .as_str();

    let value_pair = inner
        .next()
        .and_then(|p| p.into_inner().next())
        .ok_or_else(|| ParseError::invalid_state_with_span("metadata value", span))?;

    let value = match value_pair.as_rule() {
        Rule::quoted_str => get_quoted_str(value_pair)?,
        _ => value_pair.as_str().to_string(),
    };

    Ok((key.into(), value))
}

fn optional_rule<'i>(rule: Rule, pairs: &mut Pairs<'i, Rule>) -> Option<Pair<'i, Rule>> {
    match pairs.peek() {
        Some(ref p) if p.as_rule() == rule => pairs.next(),
        _ => None,
    }
}

fn flag(pair: Pair<'_, Rule>) -> ParseResult<char> {
    Ok(pair.as_str().chars().nth(0).unwrap())
}

fn amount<'i>(x: Pair<'i, Rule>) -> ParseResult<Amount> {
    let span = x.as_span();
    let mut tokens = x.into_inner();
    Ok(Amount {
        number: match tokens.peek() {
            Some(token) => {
                if token.as_rule() == Rule::num_expr {
                    tokens.next();
                    Some(num_expr(token)?)
                } else {
                    None
                }
            }
            None => {
                return Err(ParseError::invalid_input_with_span(
                    "missing unit for amount",
                    span,
                ));
            }
        },
        currency: tokens.next().unwrap().as_str().to_string(),
    })
}

// Assets:Account          CAD @ 1.25 USD
// Assets:Account          CAD {100.00 USD} @ 110.00 USD
// Assets:Account                        HOOL {100.00 # 9.95 USD}
// Assets:ETrade:IVV                     -10 IVV {183.07 USD}
// Assets:ETrade:IVV                     -10 IVV {183.07 USD} @ 197.90 USD
// Assets:OANDA:GBPounds                 23391.81 GBP @ 1.71 USD
// Assets:BofA:Checking                  8450.00 USD
// Assets:AccountsReceivable
fn posting<'i>(pair: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Posting> {
    let source = pair.as_str().to_string();
    let span = pair.as_span();
    let mut inner = pair.into_inner();
    let flag = optional_rule(Rule::txn_flag, &mut inner)
        .map(flag)
        .transpose()?;

    let account = inner
        .next()
        .map(|p| account(p))
        .transpose()?
        .ok_or_else(|| ParseError::invalid_state_with_span("account", span))?;

    let (units, cost, price) = parse_post_price(&mut inner)?;

    Ok(Posting {
        account,
        units,
        cost,
        price,
        flag,
        metadata: Metadata::new(),
        source,
    })
}

fn parse_post_price<'i>(
    inner: &mut Pairs<'i, Rule>,
) -> ParseResult<(Option<Amount>, Option<PostingCost>, Option<Amount>)> {
    let units = optional_rule(Rule::incomplete_amount, inner)
        .map(amount)
        .transpose()?;

    let cost = optional_rule(Rule::cost_spec, inner)
        .map(cost_spec)
        .transpose()?;

    let price_anno = optional_rule(Rule::price_annotation, inner)
        .map(price_annotation)
        .transpose()?;

    let price = match (price_anno, &units) {
        (Some((total, price_anno)), Some(unit)) => match (price_anno.number, unit.number) {
            (Some(price_number), Some(unit_number)) => {
                if total {
                    Some(Amount {
                        number: Some(price_number / unit_number),
                        currency: price_anno.currency,
                    })
                } else {
                    Some(Amount {
                        number: Some(price_number),
                        currency: price_anno.currency,
                    })
                }
            }
            _ => None,
        },
        _ => None,
    };

    match cost {
        None => {}
        Some(cost) => {
            return Ok((units, Some(PostingCost::CostSpec(cost)), price));
        }
    }

    Ok((units, None, price))
}

fn price_annotation<'i>(pair: Pair<'i, Rule>) -> ParseResult<(bool, Amount)> {
    debug_assert!(pair.as_rule() == Rule::price_annotation);
    let span = pair.as_span();
    let inner = pair
        .into_inner()
        .next()
        .ok_or_else(|| ParseError::invalid_state_with_span("price annotation", span.clone()))?;
    let is_total = inner.as_rule() == Rule::price_annotation_total;
    let amount = amount(
        inner
            .into_inner()
            .next()
            .ok_or_else(|| ParseError::invalid_state_with_span("incomplete amount", span))?,
    )?;
    Ok((is_total, amount))
}

fn cost_spec<'i>(pair: Pair<'i, Rule>) -> ParseResult<CostSpec> {
    debug_assert!(pair.as_rule() == Rule::cost_spec);
    let mut amount = (None, None, None);
    let mut date_ = None;
    let mut label = None;
    let mut merge = None;
    let span = pair.as_span();
    let inner = pair
        .into_inner()
        .next()
        .ok_or_else(|| ParseError::invalid_state_with_span("cost spec component", span))?;
    let typ = inner.as_rule();
    for p in inner.into_inner() {
        match p.as_rule() {
            Rule::date => date_ = Some(date(p)?),
            Rule::quoted_str => label = Some(get_quoted_str(p)?),
            Rule::compound_amount => {
                amount = compound_amount(p)?;
            }
            Rule::asterisk => {
                merge = Some(true);
            }
            _ => unimplemented!(),
        }
    }
    if typ == Rule::cost_spec_total {
        if amount.1.is_some() {
            panic!("Per-unit cost may not be specified using total cost");
        }
        amount = (None, amount.0, amount.2);
    }

    Ok(CostSpec {
        number_per: amount.0,
        number_total: amount.1,
        currency: amount.2.map(|s| s.into()),
        date: date_,
        label,
        merge,
    })
}

fn compound_amount<'i>(
    pair: Pair<'i, Rule>,
) -> ParseResult<(Option<Decimal>, Option<Decimal>, Option<Cow<'i, str>>)> {
    let mut number_per = None;
    let mut number_total = None;
    let mut currency = None;
    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::num_expr => {
                let num = Some(num_expr(p)?);
                if number_per.is_none() {
                    number_per = num;
                } else {
                    number_total = num;
                }
            }
            Rule::commodity => {
                currency = Some(p.as_str().into());
            }
            _ => unimplemented!(),
        }
    }
    Ok((number_per, number_total, currency))
}

fn tags_links<'i>(pair: Pair<'i, Rule>) -> ParseResult<(HashSet<String>, HashSet<String>)> {
    let (mut tags, mut links) = (HashSet::new(), HashSet::new());
    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::tag => {
                let tag = (&p.as_str()[1..]).into();
                tags.insert(tag);
            }
            Rule::link => {
                let link = (&p.as_str()[1..]).into();
                links.insert(link);
            }
            rule => {
                unimplemented!("rule {:?}", rule);
            }
        }
    }
    Ok((tags, links))
}

// 2014-07-09 price USD  1.08 CAD
fn price_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let source = directive.as_str();

    let mut pairs = directive.into_inner();

    Ok(Directive::Price(Price {
        date: date(pairs.next().unwrap())?,
        currency: pairs.next().unwrap().as_str().to_string(),
        amount: amount(pairs.next().unwrap())?,
        meta: Metadata::new(),
    }))
}

fn num_expr(pair: Pair<'_, Rule>) -> ParseResult<Decimal> {
    Decimal::try_from(pair.as_str().trim())
        .or_else(|e| Err(ParseError::decimal_parse_error(e, pair.as_span())))
}

fn commodity_directive<'i>(
    directive: Pair<'i, Rule>,
    state: &ParseState,
) -> ParseResult<Directive> {
    let source = directive.as_str();

    let mut pairs = directive.into_inner();

    Ok(Directive::Commodity(Commodity {
        date: date(pairs.next().unwrap())?,
        currency: pairs.next().unwrap().as_str().to_string(),
        meta: meta_kv(pairs.next().unwrap(), state)?,
    }))
}

// YYYY-MM-DD pad Account AccountPad
fn pad_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let source = directive.as_str();
    let mut pairs = directive.into_inner();

    Ok(Directive::Pad(Pad {
        date: date(pairs.next().unwrap())?,
        meta: Metadata::new(),
        account: account(pairs.next().unwrap())?,
        source_account: account(pairs.next().unwrap())?,
    }))
}

// 2022-01-01 balance Assets:CC:Federal:PreTax401k  0 DEFCCY
fn balance_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let source = directive.as_str();
    let span = directive.as_span();
    let mut pairs = directive.into_inner();

    Ok(Directive::Balance(Balance {
        date: date(pairs.next().unwrap())?,
        meta: Metadata::new(),
        tolerance: None,
        account: account(pairs.next().unwrap())?,
        amount: amount(pairs.next().ok_or_else(|| {
            ParseError::invalid_input_with_span("missing amount".to_string(), span)
        })?)?,
        diff_amount: None,
    }))
}

// 2014-07-09 query "france-balances" "
//   SELECT account, sum(position) WHERE ‘trip-france-2014’ in tags"
fn query_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let mut pairs = directive.into_inner();

    Ok(Directive::Query(Query {
        date: date(pairs.next().unwrap())?,
        meta: Metadata::new(),
        name: get_quoted_str(pairs.next().unwrap())?,
        query_string: get_quoted_str(pairs.next().unwrap())?,
    }))
}

// 2014-07-09 event "location" "Paris, France"
fn event_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let source = directive.as_str();
    let mut pairs = directive.into_inner();

    Ok(Directive::Event(Event {
        date: date(pairs.next().unwrap())?,
        meta: Metadata::new(),
        name: get_quoted_str(pairs.next().unwrap())?,
        description: get_quoted_str(pairs.next().unwrap())?,
    }))
}

// 2013-11-03 document Liabilities:CreditCard "/home/joe/stmts/apr-2014.pdf"
fn document_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let source = directive.as_str();
    let mut pairs = directive.into_inner();
    let mut doc = Document {
        meta: Metadata::new(),
        date: date(pairs.next().unwrap())?,
        account: account(pairs.next().unwrap())?,
        filename: get_quoted_str(pairs.next().unwrap())?,
        tags: HashSet::new(),
        link: HashSet::new(),
    };

    parse_tag_links(&mut pairs, &mut doc.tags, &mut doc.link)?;

    Ok(Directive::Document(doc))
}

fn close_directive(directive: Pair<'_, Rule>, _state: &ParseState<'_>) -> ParseResult<Directive> {
    let mut pairs = directive.into_inner();

    Ok(Directive::Close(Close {
        date: date(pairs.next().unwrap())?,
        account: account(pairs.next().unwrap())?,
        meta: Metadata::new(),
    }))
}

fn option_directive<'i>(directive: Pair<'i, Rule>) -> ParseResult<Directive> {
    let mut pairs = directive.into_inner();

    let name = get_quoted_str(pairs.next().unwrap())?;
    let value = get_quoted_str(pairs.next().unwrap())?;

    Ok(Directive::Option(Opt {
        name,
        value,
        meta: Metadata::new(),
    }))
}

// 2020-02-01 note Liabilities:CreditCard:CapitalOne "你好"
// 2013-05-18 note Assets:US:BestBank  "Blah." ^984446a67382 #something
// 2013-05-18 note Assets:US:BestBank  "Blah." #something ^984446a67382
fn note_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let mut pairs = directive.into_inner();
    let mut note = Note {
        meta: Metadata::new(),
        date: date(pairs.next().unwrap())?,
        account: account(pairs.next().unwrap())?,
        comment: get_quoted_str(pairs.next().unwrap())?,
        tags: HashSet::new(),
        link: HashSet::new(),
    };

    parse_tag_links(&mut pairs, &mut note.tags, &mut note.link)?;

    return Ok(Directive::Note(note));
}

fn parse_tag_links(
    directive: &mut Pairs<'_, Rule>,
    tags: &mut HashSet<String>,
    links: &mut HashSet<String>,
) -> ParseResult<()> {
    match directive.peek() {
        Some(ref p) => {
            if p.as_rule() != Rule::tags_links {
                return Ok(());
            }

            for entry in p.clone().into_inner() {
                match entry.as_rule() {
                    Rule::link => {
                        links.insert(entry.into_inner().next().unwrap().as_str().to_string());
                    }
                    Rule::tag => {
                        tags.insert(entry.into_inner().next().unwrap().as_str().to_string());
                    }
                    _ => panic!("unexpected entry {:?}", entry),
                }
            }

            Ok(())
        }
        None => Ok(()),
    }
}

fn custom_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let source = directive.as_str();
    let mut pairs = directive.into_inner();
    Ok(Directive::Custom(Custom {
        meta: Metadata::new(),
        date: date(pairs.next().unwrap())?,
        name: get_quoted_str(pairs.next().unwrap())?,
        values: {
            match pairs.peek() {
                None => Vec::new(),
                Some(ref p) => {
                    if pairs.peek().unwrap().as_rule() == Rule::custom_value_list {
                        pairs
                            .peek()
                            .unwrap()
                            .into_inner()
                            .map(|p| may_quoted_str(p))
                            .collect::<ParseResult<Vec<_>>>()?
                    } else {
                        Vec::new()
                    }
                }
            }
        },
    }))
}

fn may_quoted_str<'i>(pair: Pair<'i, Rule>) -> ParseResult<String> {
    debug_assert!(
        pair.as_rule() == Rule::quoted_str || pair.as_rule() == Rule::unquoted_str,
        "{:#?}",
        pair
    );
    if pair.as_rule() == Rule::quoted_str {
        return get_quoted_str(pair);
    }

    return Ok(pair.as_str().into());
}

fn get_quoted_str<'i>(pair: Pair<'i, Rule>) -> ParseResult<String> {
    debug_assert!(pair.as_rule() == Rule::quoted_str, "{:#?}", pair);
    let span = pair.as_span();
    Ok(pair
        .into_inner()
        .next()
        .ok_or_else(|| ParseError::invalid_state_with_span("quoted string", span))?
        .as_str()
        .into())
}

fn plugin_directive<'i>(directive: Pair<'i, Rule>) -> ParseResult<Directive> {
    let mut paris = directive.into_inner();

    let name = get_quoted_str(paris.next().unwrap())?;
    let value = paris.next().map(get_quoted_str).transpose()?;

    Ok(Directive::Plugin(Plugin {
        module: name,
        config: value,
    }))
}

fn date<'i>(pair: Pair<'i, Rule>) -> ParseResult<NaiveDate> {
    let mut pairs = pair.into_inner();

    let year = pairs.next().unwrap().as_str().parse().unwrap();
    pairs.next();
    let mon = pairs.next().unwrap().as_str().parse().unwrap();
    pairs.next();
    let day = pairs.next().unwrap().as_str().parse().unwrap();

    Ok(NaiveDate::from_ymd_opt(year, mon, day).unwrap())

    // NaiveDate::from_ymd_opt(pair.as_str()).ok_or_else(|| ParseError {
    //     kind: ParseErrorKind::InvalidParserState {
    //         message: "invalid date".to_string(),
    //     },
    //     location: pair.as_span().start_pos().line_col(),
    //     source: pair.as_str().into(),
    // })
}

fn open_directive<'i>(directive: Pair<'i, Rule>, state: &ParseState) -> ParseResult<Directive> {
    let span = directive.as_span();

    let mut pairs = directive.into_inner();

    Ok(Directive::Open(Open {
        meta: Metadata::new(),
        date: date(pairs.next().unwrap())?,
        account: account(pairs.next().unwrap())?,
        currencies: match pairs.peek() {
            Some(ref p) => {
                if p.as_rule() == Rule::commodity_list {
                    pairs
                        .next()
                        .ok_or_else(|| {
                            ParseError::invalid_state_with_span(
                                stringify!(currencies),
                                span.clone(),
                            )
                        })?
                        .into_inner()
                        .map(|x| x.as_str().to_string())
                        .collect()
                } else {
                    Vec::new()
                }
            }
            None => Vec::new(),
        },
        booking: match pairs.peek() {
            Some(ref p) => {
                if p.as_rule() == Rule::quoted_str {
                    let f = {
                        |p: Pair<'i, _>| -> ParseResult<Option<data::Booking>> {
                            let span = p.as_span();
                            get_quoted_str(p)?
                                .try_into()
                                .map_err(|_| {
                                    ParseError::invalid_input_with_span(
                                        format!("unknown booking method {}", span.as_str()),
                                        span,
                                    )
                                })
                                .map(Some)
                        }
                    };
                    let pair = pairs.next().ok_or_else(|| {
                        ParseError::invalid_state_with_span(stringify!(booking), span.clone())
                    })?;
                    f(pair)?
                } else {
                    None
                }
            }
            None => None,
        },
    }))
}

fn account<'i>(pair: Pair<'i, Rule>) -> ParseResult<String> {
    // debug_assert!(pair.as_rule() == Rule::account);
    // let span = pair.as_span();
    // let mut inner = pair.into_inner();
    // let first_pair = inner
    //     .next()
    //     .ok_or_else(|| ParseError::invalid_state_with_span("first part of account name", span))?;
    let first = pair.as_str();
    return Ok(first.into());
    //     return Ok(state
    //         .root_names
    //         .iter()
    //         .filter(|(_, ref v)| *v == first)
    //         .map(|(k, _)| k.clone())
    //         .next()
    //         .ok_or_else(|| {
    //             pest::error::Error::new_from_span(
    //                 pest::error::ErrorVariant::CustomError {
    //                     message: "Invalid root account".to_string(),
    //                 },
    //                 first_pair.as_span(),
    //             )
    //         })?);
}
