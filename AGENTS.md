# Beancount Parser Project

This project is replacing the original C parser with a Rust-based parser while maintaining the same Python API.

## Overview

Beancount is a double-entry accounting system that uses text files as input. This fork replaces the C parser with a Rust implementation based on Chumsky.

### Instruction highlights (from .github/copilot-instructions.md)

- Keep the Python API identical to upstream while swapping the parser implementation to Rust.
- `crates/parser/` provides the Rust parser and produces Rust core directives.
- `crates/core/` hosts the Rust core directive/data model used by the parser and Python bindings.
- `crates/parser-py/` converts `beancount_core::Directive` into Python types from `beancount/core/data.py`; add tests in Python (not Rust) because it links to Python.
- After Rust changes, run `maturin develop` before testing in Python.
- When updating `crates/parser-py/` signatures, update the stubs `beancount/parser/_rust.pyi` and `beancount/parser/_parser.pyi`.

## Project Structure

### Python Package (`beancount/`)

- `core/` - Core data types and utilities (accounts, amounts, inventory, positions, etc.)
  - `data.py` - Defines Python data structures that mirror Rust `beancount_core::Directive` types
  - Other modules: `account.py`, `amount.py`, `inventory.py`, `position.py`, `number.py`, etc.
- `parser/` - Parser interface
  - `_rust.pyi` - Type stubs for the Rust parser Python bindings
  - `_parser.pyi` - Type stubs for the parser interface
- `loader.py` - High-level file loading interface
- `ops/` - Operations on parsed data
- `scripts/` - CLI tools (bean-check, bean-doctor, bean-example, bean-format)
- `tools/` - Additional utilities

### Rust Workspace (`crates/`)

The Rust code is organized as a Cargo workspace with three crates:

1. **`crates/core/`** - Rust core data model and helpers
  - Defines the core `Directive` enum and related types used throughout the Rust implementation.
  - Contains helpers like directive normalization and transaction posting inference.
  - Rust booking lives in `crates/core/src/booking.rs` and is invoked via the core loader APIs
    `load_file_and_book()` / `load_string_and_book()` in `crates/core/src/loader.rs`.
  - The Rust loader auto-detects encrypted input and decrypts via `crates/core/src/encryption.rs`
    (no `SourceReader` abstraction).
  - Depends on `crates/parser/` for shared AST/span/meta types (`beancount_parser::ast`).

2. **`crates/parser/`** - Rust parser implementation
  - Parses Beancount input into Rust core directives.
  - Owns the parsing logic and AST definitions used across the Rust codebase.

3. **`crates/parser-py/`** - Python bindings
   - Uses PyO3 to expose Rust parser to Python
  - Converts `beancount_core::Directive` to Python objects defined in `beancount/core/data.py`
   - Compiled to `beancount.parser._parser_rust`
   - **Do not add Rust tests here** - test through Python instead

## Development Workflow

### Building the Rust Parser

Since Rust is a compiled language, you must rebuild after any Rust code changes:

```bash
maturin develop --uv
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
- `crates/core/src/loader.rs` - Rust recursive loader, including `load_file_and_book` / `load_string_and_book`
- `crates/core/src/booking.rs` - Rust booking implementation used by the loader "load + book" APIs
- `crates/core/src/encryption.rs` - Rust encrypted-file detection and `gpg --decrypt` helper used by the loader
- `crates/core/src/core.rs` - Core directive/data model used by Rust + Python bindings
- `crates/core/src/inference.rs` - Transaction posting inference helpers
- `crates/parser/src/lib.rs` - Parser crate entrypoint
- `crates/parser-py/src/lib.rs` - PyO3 bindings and conversion into Python objects
- `beancount/core/data.py` - Python data structures that must match Rust `beancount_core::Directive`

## Important Notes

1. Always run `maturin develop` after modifying Rust code before testing in Python
2. Setting `BEANCOUNT_RUST_LOAD_AND_BOOK=1` makes the Python loader use the Rust "load + book" path
  (via the Rust binding entrypoints `load_file_and_book` / `load_string_and_book`).
3. The parser must maintain API compatibility with the original C parser
4. Python data structures in `beancount/core/data.py` must remain compatible with Rust `beancount_core::Directive`
5. Type stub files must be kept in sync with the actual Rust/Python bindings
