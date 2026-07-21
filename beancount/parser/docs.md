# Beancount Parser Documentation

## Overview

The `beancount.parser` directory contains the core components responsible for reading Beancount syntax input files and transforming them into internal Python data structures (directives). It utilizes a hybrid architecture, combining a performant C-based lexer and parser with high-level Python logic for "booking" and data completion.

## Architecture

The parsing process follows a multi-stage pipeline:

```mermaid
graph LR
    Input[Input File] --> Lexer
    Lexer[Lexer (Flex/C)] --> Parser
    Parser[Parser (Bison/C)] --> Incomplete[Incomplete Entries]
    Incomplete --> Booking[Booking Logic (Python)]
    Booking --> Complete[Complete Entries]
```

1.  **Lexing & Parsing (C Extension)**: The raw text is tokenized and parsed using a C extension generated from `lexer.l` (Flex) and `grammar.y` (Bison). This stage produces "incomplete" Python objects where some fields (like inferred amounts or costs) may be marked as `MISSING`.
2.  **Booking (Python)**: The incomplete entries are passed through the booking logic. This stage resolves missing information by:
    *   Matching postings against existing inventory lots.
    *   Interpolating missing numbers (e.g., calculating price from cost and total value).
    *   Converting `CostSpec` (fuzzy matching) to `Cost` (concrete lots).

## Key Modules

### Core Parsing
*   **`parser.py`**: The main public interface for the package. It provides functions like `parse_file` and `parse_string` which coordinate the C parser and the booking process.
*   **`grammar.y`**: The Bison grammar file defining the syntax of the Beancount language.
*   **`lexer.l`**: The Flex lexer file defining the tokens (keywords, numbers, strings).
*   **`_parser.pyi`**: Type stub for the compiled C extension module.

### Booking & Logic
*   **`booking.py`**: Orchestrates the booking process. It validates entries and invokes the detailed logic in `booking_full.py`.
*   **`booking_full.py`**: Contains the complex algorithms for inventory matching and arithmetic interpolation to resolve `MISSING` values in transactions.

### Helpers
*   **`printer.py`**: Provides functionality to reverse the process—rendering internal directive objects back into valid Beancount syntax strings.
*   **`options.py`**: Defines the available options (e.g., `title`, `operating_currency`, `plugin`) that can be set in a Beancount file, including their default values and validation logic.
*   **`cmptest.py`**: Utilities for comparing entries in tests, handling the idiosyncrasies of floating-point comparison and incomplete objects.

## Concepts

*   **Incomplete Entries**: Transactions produced immediately after the C parsing stage may contain `MISSING` sentinel values. These represent data that the user omitted (e.g., "sell all shares" or "calculate price automatically") and must be inferred.
*   **Directives**: The internal representation of Beancount statements, such as `Transaction`, `Open`, `Close`, `Price`, etc.
*   **Options Map**: A dictionary containing configuration values derived from the input file, managed by `options.py`.
