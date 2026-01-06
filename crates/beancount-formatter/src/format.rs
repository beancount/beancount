use anyhow::{Context, Result};
use tree_sitter::{Node, Parser, Point};

use beancount_parser::ast::{self, Directive};
use beancount_parser::parse_directives;

use crate::configuration::{Configuration, NewLineKind};

/// Simple string writer to avoid building large intermediate vectors before concatenation.
struct Writer {
    buf: String,
}

fn format_open(writer: &mut Writer, d: &ast::Open<'_>, config: &Configuration) {
    let comment_col = config.line_width as usize;
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("open".to_string()),
        Some(to_part(d.account)),
    ]);
    line = align_trailing(line, format_currencies(&d.currencies), comment_col);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, true);
    }
    writer.write_str(&line);
}

fn format_close(writer: &mut Writer, d: &ast::Close<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("close".to_string()),
        Some(to_part(d.account)),
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_balance(writer: &mut Writer, d: &ast::Balance<'_>, config: &Configuration) {
    let comment_col = config.line_width as usize;
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("balance".to_string()),
        Some(to_part(d.account)),
    ]);
    let trailing = Some(format!(
        "{} {}",
        compact_ws(d.amount.number),
        d.amount.currency
    ));
    line = align_trailing(line, trailing, comment_col);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, true);
    }
    writer.write_str(&line);
}

fn format_pad(writer: &mut Writer, d: &ast::Pad<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("pad".to_string()),
        Some(to_part(d.account)),
        Some(to_part(d.from_account)),
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_commodity(writer: &mut Writer, d: &ast::Commodity<'_>, config: &Configuration) {
    let comment_col = config.line_width as usize;
    let mut line = join_parts([Some(to_part(d.date)), Some("commodity".to_string())]);
    line = align_trailing(line, Some(to_part(d.currency)), comment_col);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, true);
    }
    writer.write_str(&line);
}

fn format_price(writer: &mut Writer, d: &ast::Price<'_>, config: &Configuration) {
    let comment_col = config.line_width as usize;
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("price".to_string()),
        Some(to_part(d.currency)),
    ]);
    let trailing = Some(format!(
        "{} {}",
        compact_ws(d.amount.number),
        d.amount.currency
    ));
    line = align_trailing(line, trailing, comment_col);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, true);
    }
    writer.write_str(&line);
}

fn format_event(writer: &mut Writer, d: &ast::Event<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("event".to_string()),
        Some(to_part(d.event_type)),
        Some(to_part(d.desc)),
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_query(writer: &mut Writer, d: &ast::Query<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("query".to_string()),
        Some(to_part(d.name)),
        Some(to_part(d.query)),
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_note(writer: &mut Writer, d: &ast::Note<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("note".to_string()),
        Some(to_part(d.account)),
        Some(to_part(d.note)),
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_document(writer: &mut Writer, d: &ast::Document<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("document".to_string()),
        Some(to_part(d.account)),
        Some(to_part(d.filename)),
        d.tags_links.map(|t| t.trim().to_string()),
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_custom(writer: &mut Writer, d: &ast::Custom<'_>, config: &Configuration) {
    let mut line = join_parts([
        Some(to_part(d.date)),
        Some("custom".to_string()),
        Some(to_part(d.name)),
        if d.values.is_empty() {
            None
        } else {
            Some(
                d.values
                    .iter()
                    .map(|v| v.raw.trim())
                    .collect::<Vec<_>>()
                    .join(" "),
            )
        },
    ]);
    if let Some(comment) = &d.comment {
        line = append_comment(line, &format_comment(comment), config, false);
    }
    writer.write_str(&line);
}

fn format_option(writer: &mut Writer, d: &ast::OptionDirective<'_>) {
    let line = join_parts([
        Some("option".to_string()),
        Some(to_part(d.key)),
        Some(to_part(d.value)),
    ]);
    writer.write_str(&line);
}

fn format_include(writer: &mut Writer, d: &ast::Include<'_>) {
    let line = join_parts([Some("include".to_string()), Some(to_part(d.filename))]);
    writer.write_str(&line);
}

fn format_plugin(writer: &mut Writer, d: &ast::Plugin<'_>) {
    let line = join_parts([
        Some("plugin".to_string()),
        Some(to_part(d.name)),
        d.config.map(|c| c.trim().to_string()),
    ]);
    writer.write_str(&line);
}

fn format_pushtag(writer: &mut Writer, d: &ast::TagDirective<'_>) {
    let line = join_parts([Some("pushtag".to_string()), Some(to_part(d.tag))]);
    writer.write_str(&line);
}

fn format_poptag(writer: &mut Writer, d: &ast::TagDirective<'_>) {
    let line = join_parts([Some("poptag".to_string()), Some(to_part(d.tag))]);
    writer.write_str(&line);
}

fn format_pushmeta(writer: &mut Writer, d: &ast::PushMeta<'_>) {
    let line = join_parts([
        Some("pushmeta".to_string()),
        Some(normalize_key_value(d.key_value)),
    ]);
    writer.write_str(&line);
}

fn format_popmeta(writer: &mut Writer, d: &ast::PopMeta<'_>) {
    let line = join_parts([
        Some("popmeta".to_string()),
        Some(format!("{}:", to_part(d.key))),
    ]);
    writer.write_str(&line);
}

impl Writer {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            buf: String::with_capacity(capacity),
        }
    }

    fn write_str(&mut self, piece: &str) {
        self.buf.push_str(piece);
    }

    fn finish(self) -> String {
        self.buf
    }
}

struct FormatterContext<'a> {
    config: &'a Configuration,
    writer: Writer,
}

impl<'a> FormatterContext<'a> {
    fn new(config: &'a Configuration, capacity: usize) -> Self {
        Self {
            config,
            writer: Writer::with_capacity(capacity),
        }
    }

    fn finish(self) -> String {
        self.writer.finish()
    }

    fn write(&mut self, piece: &str) {
        self.writer.write_str(piece);
    }

    fn format_span(&mut self, span: ast::Span, full_source: &str) {
        let slice = &full_source[span.start..span.end];
        self.write(&normalize_indentation(slice, self.config.indent_width));
        // normalize_indentation already wrote trailing newlines; caller adds newline.
        if self.writer.buf.ends_with('\n') {
            self.writer.buf.pop();
        }
    }

    fn format_directive(&mut self, dir: &Directive<'a>, full_source: &str) {
        match dir {
            Directive::Open(d) => format_open(&mut self.writer, d, self.config),
            Directive::Close(d) => format_close(&mut self.writer, d, self.config),
            Directive::Balance(d) => format_balance(&mut self.writer, d, self.config),
            Directive::Pad(d) => format_pad(&mut self.writer, d, self.config),
            Directive::Transaction(d) => self.format_transaction(d, full_source),
            Directive::Commodity(d) => format_commodity(&mut self.writer, d, self.config),
            Directive::Price(d) => format_price(&mut self.writer, d, self.config),
            Directive::Event(d) => format_event(&mut self.writer, d, self.config),
            Directive::Query(d) => format_query(&mut self.writer, d, self.config),
            Directive::Note(d) => format_note(&mut self.writer, d, self.config),
            Directive::Document(d) => format_document(&mut self.writer, d, self.config),
            Directive::Custom(d) => format_custom(&mut self.writer, d, self.config),
            Directive::Option(d) => format_option(&mut self.writer, d),
            Directive::Include(d) => format_include(&mut self.writer, d),
            Directive::Plugin(d) => format_plugin(&mut self.writer, d),
            Directive::Pushtag(d) => format_pushtag(&mut self.writer, d),
            Directive::Poptag(d) => format_poptag(&mut self.writer, d),
            Directive::Pushmeta(d) => format_pushmeta(&mut self.writer, d),
            Directive::Popmeta(d) => format_popmeta(&mut self.writer, d),
            Directive::Raw(d) => self.format_span(d.span, full_source),
        }
    }

    fn format_transaction(&mut self, txn: &ast::Transaction<'a>, full_source: &str) {
        let txn_text = &full_source[txn.span.start..txn.span.end];
        let mut lines: Vec<String> = txn_text
            .replace("\r\n", "\n")
            .lines()
            .map(|l| l.to_string())
            .collect();

        let has_posting_comments = txn.postings.iter().any(|p| p.comment.is_some());

        let mut header_parts: Vec<String> = Vec::new();
        header_parts.push(txn.date.trim().to_string());
        if let Some(flag) = &txn.txn {
            header_parts.push(flag.trim().to_string());
        }
        if let Some(payee) = &txn.payee {
            header_parts.push(payee.trim().to_string());
        }
        if let Some(narr) = &txn.narration {
            header_parts.push(narr.trim().to_string());
        }
        if let Some(tags) = &txn.tags_links {
            header_parts.push(tags.trim().to_string());
        }
        let mut header_line = header_parts.join(" ");
        if let Some(comment) = &txn.comment {
            header_line = append_comment(header_line, &format_comment(comment), self.config, false);
        }
        lines[0] = header_line;

        let mut posting_line_indices = Vec::new();

        if has_posting_comments {
            let mut min_indent = usize::MAX;

            for posting in &txn.postings {
                let offset = posting.span.start.saturating_sub(txn.span.start);
                let line_idx = count_newlines_up_to(txn_text, offset);
                posting_line_indices.push(line_idx);
                if let Some(line) = lines.get(line_idx) {
                    let indent = leading_indent_width(line, self.config.indent_width);
                    min_indent = min_indent.min(indent);
                }
            }

            if min_indent == usize::MAX {
                min_indent = (self.config.indent_width as usize) * 2;
            }

            for (posting, &line_idx) in txn.postings.iter().zip(posting_line_indices.iter()) {
                let flag = posting.opt_flag.map(str::trim);
                let account = posting.account.trim();
                let trailing = if let Some(amount) = posting.amount.as_ref() {
                    let mut parts =
                        vec![format!("{} {}", compact_ws(amount.number), amount.currency)];
                    if let Some(cost) = posting.cost_spec.as_ref() {
                        parts.push(compact_ws(cost.raw));
                    }
                    if let Some(price_op) = posting.price_operator {
                        parts.push(price_op.trim().to_string());
                    }
                    if let Some(price_ann) = posting.price_annotation.as_ref() {
                        parts.push(format!(
                            "{} {}",
                            compact_ws(price_ann.number),
                            price_ann.currency
                        ));
                    }
                    Some(parts.join(" "))
                } else {
                    None
                };

                let mut line = String::new();
                line.push_str(&" ".repeat(min_indent));
                if let Some(f) = flag {
                    line.push_str(f);
                    line.push(' ');
                }
                line.push_str(account);

                line = align_trailing(line, trailing, self.config.line_width as usize);

                if let Some(comment) = &posting.comment {
                    line = append_comment(line, &format_comment(comment), self.config, true);
                }

                if let Some(slot) = lines.get_mut(line_idx) {
                    *slot = line;
                }
            }
        }

        for (idx, line) in lines.iter_mut().enumerate().skip(1) {
            if has_posting_comments && posting_line_indices.contains(&idx) {
                continue;
            }
            *line = normalize_indentation(line, self.config.indent_width);
        }

        self.write(&lines.join("\n"));
    }
}

pub fn format(path: Option<&str>, source_text: &str, config: &Configuration) -> Result<String> {
    format_content(path, source_text, config)
}

fn format_content(
    path: Option<&str>,
    content: &str,
    formatting_config: &Configuration,
) -> Result<String> {
    let path = path.unwrap_or("<memory>");

    // The parser expects a trailing newline; append one if it's missing.
    let content = if content.ends_with('\n') || content.ends_with("\r\n") {
        content.to_string()
    } else {
        format!("{}\n", content)
    };

    let mut parser = Parser::new();

    parser
        .set_language(&tree_sitter_beancount::language())
        .context("Failed to load beancount grammar")?;

    let tree = parser
        .parse(&content, None)
        .ok_or_else(|| anyhow::anyhow!("Failed to parse {}", path))?;

    if tree.root_node().has_error() {
        let error_message = describe_parse_errors(tree.root_node(), &content);
        return Err(anyhow::anyhow!(
            "Failed to parse {}: {}",
            path,
            error_message
        ));
    }

    let root = tree.root_node();

    let directives =
        parse_directives(root, &content, path.to_string()).map_err(anyhow::Error::new)?;

    let newline = match formatting_config.new_line {
        NewLineKind::LF => "\n",
        NewLineKind::CRLF => "\r\n",
    };

    let mut ctx = FormatterContext::new(formatting_config, content.len());
    for dir in &directives {
        ctx.format_directive(dir, &content);
        ctx.write(newline);
    }

    // From this point on we only normalize newline style; the per-node formatter
    // should not add extra trailing newlines beyond what we explicitly wrote.
    let mut formatted = ctx.finish();

    if newline == "\r\n" {
        // Convert lone LF to CRLF, but don't double-convert existing CRLF.
        formatted = formatted.replace("\r\n", "\n");
        formatted = formatted.replace("\n", "\r\n");
    } else {
        // Normalize any CRLF sequences back to LF.
        formatted = formatted.replace("\r\n", "\n");
    }

    // Collapse multiple trailing newlines down to a single newline token.
    let had_trailing_newline = formatted.ends_with(newline);
    formatted = formatted.trim_end_matches(newline).to_string();
    if had_trailing_newline {
        formatted.push_str(newline);
    }

    // Always ensure a single trailing newline for downstream consumers.
    if newline == "\r\n" {
        if !formatted.ends_with("\r\n") {
            formatted.push_str("\r\n");
        }
    } else if !formatted.ends_with('\n') {
        formatted.push('\n');
    }

    Ok(formatted)
}

/// Build a concise error summary from tree-sitter error nodes, including row/col info.
fn describe_parse_errors(root: Node, text: &str) -> String {
    let mut messages = Vec::new();
    let mut stack = vec![root];

    while let Some(node) = stack.pop() {
        if node.is_error() || node.is_missing() {
            let span = format_point_range(node.start_position(), node.end_position());
            let snippet = slice_text(node, text).trim();
            if node.is_missing() {
                messages.push(format!("missing {:?} at {}", node.kind(), span));
            } else {
                messages.push(format!("error at {} near {:?}", span, snippet));
            }
        }

        let mut cursor = node.walk();
        if cursor.goto_first_child() {
            loop {
                stack.push(cursor.node());
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
    }

    if messages.is_empty() {
        "unknown parse error".to_string()
    } else {
        messages.join("; ")
    }
}

fn format_point_range(start: Point, end: Point) -> String {
    if start == end {
        format!("{}:{}", start.row + 1, start.column + 1)
    } else {
        format!(
            "{}:{}-{}:{}",
            start.row + 1,
            start.column + 1,
            end.row + 1,
            end.column + 1
        )
    }
}

fn slice_text<'a>(node: Node, text: &'a str) -> &'a str {
    let start = node.start_byte();
    let end = node.end_byte();
    &text[start..end]
}

/// Normalizes tabs to spaces (respecting indent width) outside of string literals and trims trailing whitespace per line.
fn normalize_indentation(text: &str, indent_width: u8) -> String {
    let mut out = String::with_capacity(text.len());

    for (i, line) in text.replace("\r\n", "\n").lines().enumerate() {
        if i > 0 {
            out.push('\n');
        }

        // Expand tabs outside of string literals, then trim trailing whitespace.
        let expanded = expand_tabs_outside_strings(line, indent_width);
        let trimmed = expanded.trim_end();
        out.push_str(trimmed);
    }

    out
}

/// Expand tabs to spaces while skipping tabs that appear inside string literals.
/// Leading tabs expand to the configured indent width; tabs elsewhere become a single space.
fn expand_tabs_outside_strings(line: &str, indent_width: u8) -> String {
    let indent = " ".repeat(indent_width as usize);
    let mut out = String::with_capacity(line.len());
    let mut in_string = false;
    let mut escape = false;
    let mut at_line_start = true;

    for ch in line.chars() {
        if in_string {
            out.push(ch);
            if escape {
                escape = false;
                continue;
            }
            match ch {
                '\\' => escape = true,
                '"' => in_string = false,
                _ => {}
            }
            at_line_start = false;
            continue;
        }

        match ch {
            '"' => {
                in_string = true;
                out.push(ch);
                at_line_start = false;
            }
            '\t' => {
                if at_line_start {
                    out.push_str(&indent);
                } else {
                    out.push(' ');
                }
            }
            _ => {
                out.push(ch);
                at_line_start = false;
            }
        }
    }

    out
}

fn count_newlines_up_to(text: &str, offset: usize) -> usize {
    text.as_bytes()
        .iter()
        .take(offset.min(text.len()))
        .filter(|b| **b == b'\n')
        .count()
}

fn leading_indent_width(line: &str, indent_width: u8) -> usize {
    let mut width = 0usize;
    for ch in line.chars() {
        match ch {
            ' ' => width += 1,
            '\t' => width += indent_width as usize,
            _ => break,
        }
    }
    width
}

fn join_parts(parts: impl IntoIterator<Item = Option<String>>) -> String {
    let mut out = Vec::new();
    for p in parts.into_iter().flatten() {
        if !p.is_empty() {
            out.push(p);
        }
    }
    out.join(" ")
}

fn to_part(text: &str) -> String {
    text.trim().to_string()
}

fn compact_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn normalize_key_value(text: &str) -> String {
    let mut parts = text.splitn(2, ':');
    let key = parts.next().unwrap_or("").trim();
    let value = parts.next().unwrap_or("").trim();
    if value.is_empty() {
        format!("{}:", key)
    } else {
        format!("{}: {}", key, value)
    }
}

fn append_comment(mut line: String, comment: &str, config: &Configuration, align: bool) -> String {
    let trimmed = line.trim_end().to_string();
    let base_len = trimmed.len();
    let target = config.line_width as usize;

    line = trimmed;
    if align && base_len < target {
        line.push_str(&" ".repeat(target - base_len));
    } else if !line.ends_with(' ') {
        line.push(' ');
    }

    line.push_str(comment);
    line
}

fn align_trailing(mut base: String, trailing: Option<String>, comment_col: usize) -> String {
    if let Some(value) = trailing {
        let value_len = value.len();
        let target_end = comment_col.saturating_sub(2);
        let desired_start = target_end.saturating_sub(value_len.saturating_sub(1));
        let start = desired_start.max(base.len().saturating_add(1));

        if base.len() < start {
            base.push_str(&" ".repeat(start - base.len()));
        }
        base.push_str(&value);
    }

    base
}
fn format_currencies(currencies: &[&str]) -> Option<String> {
    if currencies.is_empty() {
        return None;
    }
    Some(
        currencies
            .iter()
            .map(|c| c.trim())
            .collect::<Vec<_>>()
            .join(" "),
    )
}

fn format_comment(raw: &str) -> String {
    let trimmed = raw.trim();
    let without_semicolon = trimmed.strip_prefix(';').unwrap_or(trimmed).trim_start();
    if without_semicolon.is_empty() {
        ";".to_string()
    } else {
        format!("; {}", without_semicolon)
    }
}
