# Beancount Source Code Documentation

## Overview

This directory (`beancount/`) contains the source code for the Beancount double-entry bookkeeping system. Beancount is a text-based tool that processes a list of financial transactions and produces various reports.

The codebase is organized into a core data model, a parser for the Beancount syntax, a loader that orchestrates the processing pipeline, and a set of operations and plugins that manipulate and validate the data.

## Key Concepts & Architecture

The typical data flow in Beancount is:
1.  **Loading**: `loader.py` reads the input file(s).
2.  **Parsing**: `parser/` converts the text into a list of Python objects (Directives).
3.  **Booking**: `parser/booking.py` interpolates missing amounts (e.g., filling in the other leg of a split).
4.  **Transformation**: Plugins (`plugins/`) and standard operations (`ops/`) modify or validate the stream of directives.
5.  **Realization** (Optional): `core/realization.py` converts the linear list of transactions into a tree of balances (Accounts).

## Directory Structure

### Top-Level Modules
*   **`api.py`**: The public entry point for the library. It exposes the main symbols and functions intended for external use (e.g., `load_file`).
*   **`loader.py`**: Orchestrates the loading process. It handles file I/O, encryption, caching, inclusion of other files, and calls the parser and validation steps.

### Subdirectories

#### `core/` - The Data Model
Contains the fundamental data structures and types used throughout the system.
*   **`data.py`**: Defines the immutable NamedTuple classes for directives: `Transaction`, `Posting`, `Balance`, `Open`, `Close`, `Price`, etc.
*   **`amount.py`**: Defines the `Amount` class (number + currency).
*   **`inventory.py`**: Manages collections of positions (aggregations of amounts/costs).
*   **`position.py`**: Defines `Cost` and `Position`.
*   **`number.py`**: Decimal arithmetic utilities.
*   **`account.py`**: Utilities for manipulating account names (strings like "Assets:Bank").

#### `parser/` - Syntax Parsing
Handles the reading of Beancount syntax files.
*   **`grammar.y` / `lexer.l`**: The grammar and lexer definitions (likely compiled to C).
*   **`parser.py`**: Python interface to the parser.
*   **`printer.py`**: Logic to format directives back into Beancount syntax string.
*   **`booking.py`**: Logic for "booking" transactions—calculating missing numbers based on the accounting equation (Sum of Postings = 0).

#### `ops/` - Operations
Contains business logic and common operations performed on the directive stream.
*   **`balance.py`**: Verifies `balance` assertions against calculated running balances.
*   **`validation.py`**: Runs sanity checks on the data (e.g., data types, schema validation).
*   **`pad.py`**: Implements the `pad` directive logic (filling gaps).
*   **`documents.py`**: Manages `document` directives.

#### `plugins/` - Extension System
A collection of built-in plugins that can be enabled to perform additional checks or transformations.
*   Examples: `check_commodity`, `auto_accounts`, `nounused` (checks for unused accounts).

#### `utils/`
General utility functions used across the project (dates, file system, etc.).

#### `projects/`, `scripts/`, `tools/`
Miscellaneous scripts and tools for maintenance, benchmarking, or experimental features.

## Main Entry Points
For developers using Beancount as a library:
*   `beancount.loader.load_file(filename)`: Parses a file and returns `(entries, errors, options_map)`.
*   `beancount.api`: A convenience module exporting common types and functions.
