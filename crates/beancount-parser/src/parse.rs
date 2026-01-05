use tree_sitter::Node;
use serde_json::from_str as parse_json;

use crate::ast::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub filename: String,
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.filename, self.line, self.column, self.message
        )
    }
}

impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

fn meta(node: Node, filename: &str) -> Meta {
    let p = node.start_position();
    Meta {
        filename: filename.to_owned(),
        line: p.row + 1,
        column: p.column + 1,
    }
}

fn parse_error(node: Node, filename: &str, message: impl Into<String>) -> ParseError {
    let p = node.start_position();
    ParseError {
        filename: filename.to_owned(),
        line: p.row + 1,
        column: p.column + 1,
        message: message.into(),
    }
}

fn slice<'a>(node: Node, source: &'a str) -> &'a str {
    &source[node.start_byte()..node.end_byte()]
}

fn span(node: Node) -> Span {
    Span::from_range(node.start_byte(), node.end_byte())
}

/// Split a basic amount string (`"NUMBER CURRENCY"`) into its components.
pub fn parse_amount_tokens<'a>(raw: &'a str) -> Option<(&'a str, &'a str)> {
    let mut parts = raw.split_whitespace();
    let number = parts.next()?;
    let currency = parts.next()?;
    Some((number, currency))
}

fn parse_string_value(node: Node, raw: &str, filename: &str) -> Result<String> {
    parse_json::<String>(raw)
        .map_err(|err| parse_error(node, filename, format!("invalid string: {}", err)))
}

fn collect_key_values<'a>(node: Node, source: &'a str, filename: &str) -> Result<Vec<KeyValue<'a>>> {
    let mut cursor = node.walk();
    let mut key_values = Vec::new();

    for child in node.named_children(&mut cursor) {
        if child.kind() == "key_value" {
            key_values.push(parse_key_value(child, source, filename)?);
        }
    }

    Ok(key_values)
}

fn parse_tags_links<'a, I>(groups: I) -> (Vec<&'a str>, Vec<&'a str>)
where
    I: IntoIterator<Item = &'a str>,
{
    let mut tags = Vec::new();
    let mut links = Vec::new();

    for group in groups {
        for token in group.split_whitespace() {
            if let Some(tag) = token.strip_prefix('#') {
                tags.push(tag);
            } else if let Some(link) = token.strip_prefix('^') {
                links.push(link);
            }
        }
    }

    tags.sort_unstable();
    tags.dedup();
    links.sort_unstable();
    links.dedup();

    (tags, links)
}

fn field_text<'a>(node: Node, field: &str, source: &'a str) -> Option<&'a str> {
    node.child_by_field_name(field).map(|n| slice(n, source))
}

fn required_field_text<'a>(
    node: Node,
    field: &str,
    source: &'a str,
    filename: &str,
) -> Result<&'a str> {
    field_text(node, field, source).ok_or_else(|| {
        parse_error(
            node,
            filename,
            format!("missing field `{}` in `{}`", field, node.kind()),
        )
    })
}

fn first_named_child_text<'a>(node: Node, source: &'a str) -> Option<&'a str> {
    let mut cursor = node.walk();
    node.named_children(&mut cursor).next().map(|n| slice(n, source))
}

pub fn parse_directives<'a>(
    root: Node,
    source: &'a str,
    filename: String,
) -> Result<Vec<Directive<'a>>> {
    // The grammar's root rule name is `file`. If callers pass a different node
    // (e.g. a single directive node), return a structured error.
    if root.kind() != "file" {
        let p = root.start_position();
        return Err(ParseError {
            filename,
            line: p.row + 1,
            column: p.column + 1,
            message: format!("expected root node kind `file`, got `{}`", root.kind()),
        });
    }

    let mut cursor = root.walk();
    root.named_children(&mut cursor)
        .map(|node| parse_top_level(node, source, &filename))
        .collect::<Result<Vec<_>>>()
}

fn parse_top_level<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    match node.kind() {
        // entries
        "open" => parse_open(node, source, filename),
        "close" => parse_close(node, source, filename),
        "balance" => parse_balance(node, source, filename),
        "pad" => parse_pad(node, source, filename),
        "transaction" => parse_transaction(node, source, filename),
        "document" => parse_document(node, source, filename),
        "note" => parse_note(node, source, filename),
        "event" => parse_event(node, source, filename),
        "price" => parse_price(node, source, filename),
        "commodity" => parse_commodity(node, source, filename),
        "query" => parse_query(node, source, filename),
        "custom" => parse_custom(node, source, filename),

        // directives
        "option" => parse_option(node, source, filename),
        "include" => parse_include(node, source, filename),
        "plugin" => parse_plugin(node, source, filename),
        "pushtag" => parse_pushtag(node, source, filename),
        "poptag" => parse_poptag(node, source, filename),
        "pushmeta" => parse_pushmeta(node, source, filename),
        "popmeta" => parse_popmeta(node, source, filename),

        // Known non-directive top-level nodes.
        "section" | "comment" => Ok(raw(node, source, filename)),

        other => Err(parse_error(
            node,
            filename,
            format!("unknown directive node kind `{}`", other),
        )),
    }
}

fn raw<'a>(node: Node, source: &'a str, filename: &str) -> Directive<'a> {
    Directive::Raw(Raw {
        meta: meta(node, filename),
        kind: node.kind(),
        span: span(node),
        text: slice(node, source),
    })
}

fn parse_open<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    let date = required_field_text(node, "date", source, filename)?;
    let account = required_field_text(node, "account", source, filename)?;

    let opt_booking = field_text(node, "opt_booking", source);
    let comment = field_text(node, "comment", source);
    let key_values = collect_key_values(node, source, filename)?;

    // NOTE: `currency` is a token, so it is not a named node in this grammar.
    // To avoid losing it, we parse currencies from the raw text of the field.
    let currencies = node
        .child_by_field_name("currencies")
        .map(|curr_node| parse_currencies_from_text(slice(curr_node, source)))
        .unwrap_or_default();

    Ok(Directive::Open(Open {
        meta: meta(node, filename),
        span: span(node),
        date,
        account,
        currencies,
        opt_booking,
        comment,
        key_values,
    }))
}

fn parse_currencies_from_text<'a>(text: &'a str) -> Vec<&'a str> {
    // Currency token regex in grammar:
    // [A-Z]([A-Z0-9\'\._\-]{0,22}[A-Z0-9])?
    // Here we do a best-effort scan that matches the common case.
    let mut out = Vec::new();
    let bytes = text.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        let b = bytes[i];
        if !b.is_ascii_uppercase() {
            i += 1;
            continue;
        }

        let start = i;
        i += 1;
        while i < bytes.len() {
            let b = bytes[i];
            if b.is_ascii_uppercase()
                || b.is_ascii_digit()
                || matches!(b, b'\'' | b'.' | b'_' | b'-')
            {
                i += 1;
            } else {
                break;
            }
        }

        // Keep only if ends with [A-Z0-9] per grammar.
        if i > start {
            let last = bytes[i - 1];
            if (last.is_ascii_uppercase() || last.is_ascii_digit())
                && let Some(s) = text.get(start..i)
            {
                out.push(s);
            }
        }
    }

    out
}

fn parse_close<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Close(Close {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        account: required_field_text(node, "account", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_balance<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Balance(Balance {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        account: required_field_text(node, "account", source, filename)?,
        amount: parse_amount_from_node(node, source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_pad<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Pad(Pad {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        account: required_field_text(node, "account", source, filename)?,
        from_account: required_field_text(node, "from_account", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_commodity<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Commodity(Commodity {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        currency: required_field_text(node, "currency", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_price<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Price(Price {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        currency: required_field_text(node, "currency", source, filename)?,
        amount: parse_amount_field(node, "amount", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_event<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Event(Event {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        event_type: required_field_text(node, "type", source, filename)?,
        desc: required_field_text(node, "desc", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_query<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Query(Query {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        name: required_field_text(node, "name", source, filename)?,
        query: required_field_text(node, "query", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_note<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Note(Note {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        account: required_field_text(node, "account", source, filename)?,
        note: required_field_text(node, "note", source, filename)?,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_document<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    let tags_links = field_text(node, "tags_links", source);
    let (tags, links) = tags_links
        .map(|group| parse_tags_links([group]))
        .unwrap_or_else(|| (Vec::new(), Vec::new()));

    Ok(Directive::Document(Document {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        account: required_field_text(node, "account", source, filename)?,
        filename: required_field_text(node, "filename", source, filename)?,
        tags_links,
        tags,
        links,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_custom<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    // `custom_value_list` is modeled as repeat1(custom_value) in the grammar.
    // Collect all `custom_value` named children.
    let mut cursor = node.walk();
    let values = node
        .named_children(&mut cursor)
        .filter(|n| n.kind() == "custom_value")
        .map(|n| slice(n, source))
        .collect::<Vec<_>>();

    Ok(Directive::Custom(Custom {
        meta: meta(node, filename),
        span: span(node),
        date: required_field_text(node, "date", source, filename)?,
        name: required_field_text(node, "name", source, filename)?,
        values,
        comment: field_text(node, "comment", source),
        key_values: collect_key_values(node, source, filename)?,
    }))
}

fn parse_option<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Option(OptionDirective {
        meta: meta(node, filename),
        span: span(node),
        key: required_field_text(node, "key", source, filename)?,
        value: required_field_text(node, "value", source, filename)?,
    }))
}

fn parse_include<'a>(node: Node, source: &'a str, meta_filename: &str) -> Result<Directive<'a>> {
    // include: seq("include", $.string, $._eol)
    // It's not a field, so take the 1st named child (string).
    let mut cursor = node.walk();
    let filename = node
        .named_children(&mut cursor)
        .find(|n| n.kind() == "string")
        .map(|n| slice(n, source))
        .ok_or_else(|| parse_error(node, meta_filename, "missing string"))?;

    Ok(Directive::Include(Include {
        meta: meta(node, meta_filename),
        span: span(node),
        filename,
    }))
}

fn parse_plugin<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    // plugin: seq("plugin", $.string, $._eol) | seq("plugin", $.string, $.string, $._eol)
    let mut cursor = node.walk();
    let mut strings = node
        .named_children(&mut cursor)
        .filter(|n| n.kind() == "string")
        .map(|n| slice(n, source));

    let name = strings
        .next()
        .ok_or_else(|| parse_error(node, filename, "missing plugin name"))?;
    let config = strings.next();

    Ok(Directive::Plugin(Plugin {
        meta: meta(node, filename),
        span: span(node),
        name,
        config,
    }))
}

fn parse_pushtag<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    let mut cursor = node.walk();
    let tag = node
        .named_children(&mut cursor)
        .find(|n| n.kind() == "tag")
        .map(|n| slice(n, source))
        .ok_or_else(|| parse_error(node, filename, "missing tag"))?;

    Ok(Directive::Pushtag(TagDirective {
        meta: meta(node, filename),
        span: span(node),
        tag,
    }))
}

fn parse_poptag<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    let mut cursor = node.walk();
    let tag = node
        .named_children(&mut cursor)
        .find(|n| n.kind() == "tag")
        .map(|n| slice(n, source))
        .ok_or_else(|| parse_error(node, filename, "missing tag"))?;

    Ok(Directive::Poptag(TagDirective {
        meta: meta(node, filename),
        span: span(node),
        tag,
    }))
}

fn parse_pushmeta<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    Ok(Directive::Pushmeta(Pushmeta {
        meta: meta(node, filename),
        span: span(node),
        key_value: first_named_child_text(node, source)
            .ok_or_else(|| parse_error(node, filename, "missing key_value"))?,
    }))
}

fn parse_popmeta<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    // popmeta: seq("popmeta", $.key, ":", $._eol)
    let mut cursor = node.walk();
    let key = node
        .named_children(&mut cursor)
        .find(|n| n.kind() == "key")
        .map(|n| slice(n, source))
        .ok_or_else(|| parse_error(node, filename, "missing key"))?;

    Ok(Directive::Popmeta(Popmeta {
        meta: meta(node, filename),
        span: span(node),
        key,
    }))
}

fn parse_key_value<'a>(node: Node, source: &'a str, filename: &str) -> Result<KeyValue<'a>> {
    let mut cursor = node.walk();
    let mut key = None;
    let mut value = None;

    for child in node.named_children(&mut cursor) {
        match child.kind() {
            "key" => key = Some(slice(child, source)),
            "value" => {
                let mut inner = child.walk();
                let string_child = child
                    .named_children(&mut inner)
                    .find(|n| n.kind() == "string");

                let parsed = if let Some(str_node) = string_child {
                    KeyValueValue::String(parse_string_value(
                        str_node,
                        slice(str_node, source),
                        filename,
                    )?)
                } else {
                    KeyValueValue::Raw(slice(child, source))
                };

                value = Some(parsed);
            }
            _ => {}
        }
    }

    Ok(KeyValue {
        meta: meta(node, filename),
        span: span(node),
        key: key.ok_or_else(|| parse_error(node, filename, "missing key"))?,
        value: value.ok_or_else(|| parse_error(node, filename, "missing value"))?,
    })
}

fn parse_compound_amount<'a>(node: Node, source: &'a str) -> CostAmount<'a> {
    CostAmount {
        per: field_text(node, "per", source).map(|t| t.trim()),
        total: field_text(node, "total", source).map(|t| t.trim()),
        currency: field_text(node, "currency", source).map(|t| t.trim()),
    }
}

fn parse_cost_spec<'a>(node: Node, source: &'a str, filename: &str) -> Result<CostSpec<'a>> {
    let raw = slice(node, source);
    let is_total = raw.trim_start().starts_with("{{");

    let mut amount = None;
    let mut date = None;
    let mut label = None;
    let mut merge = false;

    if let Some(list_node) = node.child_by_field_name("cost_comp_list") {
        let mut cursor = list_node.walk();
        for comp in list_node.named_children(&mut cursor) {
            let comp_text = slice(comp, source).trim();
            if comp_text == "*" {
                merge = true;
                continue;
            }

            let mut inner = comp.walk();
            if let Some(child) = comp.named_children(&mut inner).next() {
                match child.kind() {
                    "compound_amount" if amount.is_none() => {
                        amount = Some(parse_compound_amount(child, source));
                    }
                    "date" if date.is_none() => {
                        date = Some(slice(child, source));
                    }
                    "string" if label.is_none() => {
                        label = Some(parse_string_value(child, slice(child, source), filename)?);
                    }
                    _ => {}
                }
            }
        }

        let mut tokens = list_node.walk();
        for child in list_node.children(&mut tokens) {
            if child.kind() == "*" || child.kind() == "asterisk" {
                merge = true;
            }
        }
    }

    Ok(CostSpec {
        raw,
        amount,
        date,
        label,
        merge,
        is_total,
    })
}

fn parse_posting<'a>(node: Node, source: &'a str, filename: &str) -> Result<Posting<'a>> {
    let amount_raw = field_text(node, "amount", source).or_else(|| {
        let mut cursor = node.walk();
        node.named_children(&mut cursor)
            .find(|n| n.kind() == "incomplete_amount")
            .map(|n| slice(n, source))
    });

    let price_operator = {
        let mut cursor = node.walk();
        node.named_children(&mut cursor)
            .find(|n| matches!(n.kind(), "at" | "atat"))
            .map(|n| slice(n, source))
    };

    let price_annotation = field_text(node, "price_annotation", source)
        .map(|raw| parse_amount_value(raw, node, filename))
        .transpose()?;
    let amount = amount_raw
        .map(|raw| parse_amount_value(raw, node, filename))
        .transpose()?;

    let cost_spec = node
        .child_by_field_name("cost_spec")
        .map(|cost_node| parse_cost_spec(cost_node, source, filename))
        .transpose()?;

    Ok(Posting {
        meta: meta(node, filename),
        span: span(node),
        opt_flag: field_text(node, "optflag", source),
        account: required_field_text(node, "account", source, filename)?,
        amount,
        cost_spec,
        price_operator,
        price_annotation,
        comment: field_text(node, "comment", source),
    })
}

fn parse_amount_from_node<'a>(node: Node, source: &'a str, filename: &str) -> Result<Amount<'a>> {
    let raw = field_text(node, "amount", source)
        .or_else(|| first_named_child_text(node, source))
        .ok_or_else(|| parse_error(node, filename, "missing amount"))?;
    parse_amount_value(raw, node, filename)
}

fn parse_amount_field<'a>(
    node: Node,
    field: &str,
    source: &'a str,
    filename: &str,
) -> Result<Amount<'a>> {
    let raw = required_field_text(node, field, source, filename)?;
    parse_amount_value(raw, node, filename)
}

fn parse_amount_value<'a>(
    raw: &'a str,
    node: Node,
    filename: &str,
) -> Result<Amount<'a>> {
    if let Some((number, currency)) = parse_amount_tokens(raw) {
        Ok(Amount { raw, number, currency })
    } else {
        Err(parse_error(node, filename, "invalid amount"))
    }
}

fn parse_transaction<'a>(node: Node, source: &'a str, filename: &str) -> Result<Directive<'a>> {
    // We keep this intentionally shallow for now: ensure the node has a date and narration.
    // Different grammar versions may or may not expose field names; we support both fields and heuristics.

    let date = field_text(node, "date", source)
        .or_else(|| {
            let mut cursor = node.walk();
            node.named_children(&mut cursor)
                .find(|n| n.kind() == "date")
                .map(|n| slice(n, source))
        })
        .ok_or_else(|| parse_error(node, filename, "missing date"))?;

    let txn = field_text(node, "txn", source)
        .or_else(|| field_text(node, "flag", source))
        .or_else(|| {
            let mut cursor = node.walk();
            node.named_children(&mut cursor)
                .find(|n| n.kind() == "txn")
                .map(|n| slice(n, source))
        });

    let payee_raw = field_text(node, "payee", source);
    let narration_raw = field_text(node, "narration", source);

    let (payee_raw, narration_raw) = match (payee_raw, narration_raw) {
        // If grammar provided fields, use them (even if narration missing).
        (p @ Some(_), n) | (p @ None, n @ Some(_)) => (p, n),
        // Heuristic: take string children. If there are 2, assume payee+narration; if 1, narration only; if none, allow missing narration.
        (None, None) => {
            let mut cursor = node.walk();
            let mut strings = node
                .named_children(&mut cursor)
                .filter(|n| n.kind() == "string")
                .map(|n| slice(n, source));
            let first = strings.next();
            let second = strings.next();
            match (first, second) {
                (Some(n), None) => (None, Some(n)),
                (Some(p), Some(n)) => (Some(p), Some(n)),
                (None, None) => (None, None),
                _ => return Err(parse_error(node, filename, "invalid transaction description")),
            }
        }
    };

    let payee = payee_raw
        .map(|raw| parse_string_value(node, raw, filename))
        .transpose()?;
    let narration = narration_raw
        .map(|raw| parse_string_value(node, raw, filename))
        .transpose()?;

    let mut tags_links_lines = Vec::new();
    let mut comments = Vec::new();
    let mut key_values = Vec::new();
    let mut postings = Vec::new();

    {
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "tags_links" => tags_links_lines.push(slice(child, source)),
                "comment" => comments.push(slice(child, source)),
                "key_value" => key_values.push(parse_key_value(child, source, filename)?),
                "posting" => postings.push(parse_posting(child, source, filename)?),
                _ => {}
            }
        }
    }
    let tags_links_inline = field_text(node, "tags_links", source);
    let mut tags_links_sources = tags_links_lines.clone();
    if let Some(inline) = tags_links_inline {
        tags_links_sources.push(inline);
    }

    let (tags, links) = parse_tags_links(tags_links_sources);

    Ok(Directive::Transaction(Transaction {
        meta: meta(node, filename),
        span: span(node),
        date,
        txn,
        payee,
        narration,
        tags_links: tags_links_inline.or_else(|| tags_links_lines.first().cloned()),
        tags,
        links,
        comment: field_text(node, "comment", source).or_else(|| comments.first().cloned()),
        tags_links_lines,
        comments,
        key_values,
        postings,
    }))
}
