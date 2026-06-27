# `bin/` Directory

This directory contains executable scripts that serve as the command-line interface (CLI) entry points for various Beancount utilities. These scripts are typically installed into the user's `PATH` when the Beancount package is installed.

Most scripts here are thin wrappers that import functionality from the `beancount` Python package (specifically `beancount.scripts` or `beancount.tools`) and execute it.

## Utilities

### `bean-check`
**Source:** `beancount/scripts/check.py`

This is the primary validation tool for Beancount ledgers. It reads a Beancount input file, parses it, runs the loader and validation logic, and reports any errors found. It ensures that the ledger is syntactically correct and transactionally balanced.

### `bean-doctor`
**Source:** `beancount/scripts/doctor.py`

A debugging and diagnostic tool intended for developers or users troubleshooting issues. It can dump internal parser and lexer states and provide other introspection capabilities to help identify bugs or parsing problems.

### `bean-example`
**Source:** `beancount/scripts/example.py`

Generates a sample Beancount ledger file populated with synthetic transaction data. This is useful for:
- New users to see what a complete ledger looks like.
- Testing performance or functionality with a known dataset.
- Generating tutorials or documentation examples.

### `bean-format`
**Source:** `beancount/scripts/format.py`

A code formatter for Beancount files. It aligns transaction postings, numbers, and currencies to specific columns to improve readability. It is similar in spirit to tools like `gofmt` or `black` but for Beancount ledger files.

### `treeify`
**Source:** `beancount/tools/treeify.py`

A standalone utility that reads a text stream, identifies a column containing hierarchical identifiers (e.g., `Assets:US:Bank:Checking`), and renders that hierarchy as an ASCII tree structure. It is useful for visualizing account hierarchies from textual reports.

---
*Note: These scripts rely on the `beancount` package being available in the Python environment.*
