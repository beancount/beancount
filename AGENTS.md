# Beancount Parser Project

This project is replacing the original C parser with a Rust-based parser while maintaining the same Python API.

## Overview

Beancount is a double-entry accounting system that uses text files as input. This fork replaces the C parser with a Rust implementation based on tree-sitter.

## Project Structure

### Python Package (`beancount/`)

- `core/` - Core data types and utilities (accounts, amounts, inventory, positions, etc.)
  - `data.py` - Defines Python data structures that mirror the Rust `CoreDirective` types
  - Other modules: `account.py`, `amount.py`, `inventory.py`, `position.py`, `number.py`, etc.
- `parser/` - Parser interface
  - `_rust.pyi` - Type stubs for the Rust parser Python bindings
  - `_parser.pyi` - Type stubs for the parser interface
- `loader.py` - High-level file loading interface
- `ops/` - Operations on parsed data
- `scripts/` - CLI tools (bean-check, bean-doctor, bean-example, bean-format)
- `tools/` - Additional utilities

### Rust Workspace (`crates/`)

The Rust code is organized as a Cargo workspace with four crates:

1. **`crates/tree-sitter/`** - Tree-sitter grammar for Beancount
   - Contains the grammar definition (grammar.js) and generated files
   - Provides low-level parsing into tree-sitter syntax tree
   - Published as `beancount-tree-sitter` crate

2. **`crates/parser/`** - Core parsing logic
   - Converts tree-sitter nodes to internal AST
   - Defines `CoreDirective` types (Rust equivalents of Python data structures)
   - Dependencies: `beancount-tree-sitter`, `tree-sitter`, `chrono`, `rust_decimal`, `ropey`

3. **`crates/parser-py/`** - Python bindings
   - Uses PyO3 to expose Rust parser to Python
   - Converts `CoreDirective` to Python objects defined in `beancount/core/data.py`
   - Compiled to `beancount.parser._parser_rust`
   - **Do not add Rust tests here** - test through Python instead

4. **`crates/chumsky/`** - Experimental Chumsky-based parser
   - Alternative parser implementation using the Chumsky parser combinator library
   - Depends on `beancount-parser`

## Development Workflow

### Building the Rust Parser

Since Rust is a compiled language, you must rebuild after any Rust code changes:

```bash
maturin develop
```

This compiles the `crates/parser-py/` crate and installs it as `beancount.parser._parser_rust`.

### Type Stubs

When updating function signatures in `crates/parser-py/`, you **must** update the corresponding type stub files:
- `beancount/parser/_rust.pyi`
- `beancount/parser/_parser.pyi`

### Testing

- **Rust tests**: Run with `cargo test` in the respective crate directories
- **Parser-py tests**: Write and run Python tests only (requires Python linkage)
- **Python tests**: Run with `pytest` after running `maturin develop`

### Linting and Formatting

- **Rust**: Use `cargo fmt` and `cargo clippy`
- **Python**: Use `ruff` for linting and formatting (configured in `pyproject.toml`)
- **Python types**: Use `mypy` for type checking

## Key Files

- `Cargo.toml` - Workspace configuration
- `pyproject.toml` - Python project configuration, dependencies, and tool settings
- `crates/tree-sitter/grammar.js` - Beancount grammar definition
- `beancount/core/data.py` - Python data structures that must match Rust `CoreDirective`

## Important Notes

1. Always run `maturin develop` after modifying Rust code before testing in Python
2. The parser must maintain API compatibility with the original C parser
3. Python data structures in `beancount/core/data.py` must remain compatible with Rust `CoreDirective`
4. Type stub files must be kept in sync with the actual Rust/Python bindings
