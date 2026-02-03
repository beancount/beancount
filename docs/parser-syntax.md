# Rust Parser Syntax Reference

A concise reference for the Rust (chumsky) Beancount parser. Useful when adding directives, tweaking whitespace handling, or validating test fixtures.

## General rules
- The file is parsed as a sequence of directives separated by `\n` or end-of-file; the dispatcher consumes the trailing line ending for each directive.
- Directives start at column 0; indentation is reserved for key/value blocks that follow certain directives.
- Blank lines are skipped. Whole-line comments begin with optional space then `;` or `#` and are parsed as `Comment` directives.
- Inline comments use optional space followed by `;` and run to the end of the line. All single-line directives that opt into `inline_comment_parser` accept them: include, option, plugin, pushtag, poptag, pushmeta, popmeta, open, close, balance, pad, commodity, price, event, query, note, document, custom. Transaction parsing keeps its own rules.
- Tags/links on a line are parsed as part of the directive when present (for example documents) and are not treated as comments.

## Tokens and helpers
- Date: `YYYY-MM-DD`.
- Bare string: any non-whitespace run (accounts, currencies, plugin names, tags, etc.).
- Quoted string: double-quoted; content is preserved verbatim.
- Key/value line: `key: value` with leading indentation; value may be quoted, unquoted, boolean-like, date-like, or empty. Used by price/note/document/custom blocks.
- Inline comment span begins after optional space and `;`, ends before `\n`.

## Directive shapes (single line unless noted)
- Include: `include "glob" [; comment]`
- Option: `option "name" "value" [; comment]`
- Plugin: `plugin "module" ["config"] [; comment]`
- Tag stack: `pushtag #tag [; comment]` / `poptag #tag [; comment]`
- Meta stack: `pushmeta key: value? [; comment]` / `popmeta key: [; comment]` (value types: quoted/unquoted/bool/date; value may be omitted on pushmeta)
- Open/Close: `DATE open Account [Currency...] [; comment]`, `DATE close Account [; comment]`
- Balance: `DATE balance Account NUMBER Currency [; comment]`
- Pad: `DATE pad Account Account [; comment]`
- Commodity: `DATE commodity Currency [; comment]`
- Price: `DATE price BaseCurrency NUMBER QuoteCurrency [; comment]` followed by optional indented key/value block
- Event: `DATE event "name" "value" [; comment]`
- Query: `DATE query "name" "query text" [; comment]`
- Note: `DATE note Account "text" [; comment]` plus optional indented key/value block
- Document: `DATE document Account Filename [tags/links]? [; comment]` plus optional indented key/value block
- Custom: `DATE custom "name" <values...> [; comment]` plus optional indented key/value block; values may be account, string, number expression, bool, date, or amount
- Transactions/Prices/Raw blocks maintain their own internal grammar; see the parser modules for full details.

## Inline comment semantics
- Inline comments are attached to the directive node and can be surfaced to Python via `_parser_rust` bindings.
- Because the dispatcher consumes the trailing line ending, individual directive parsers should not call `line_end()` unless they also parse trailing blocks (e.g., price/note/document/custom handle the inline comment before the dispatcher consumes the newline).
