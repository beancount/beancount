# Beancount Core Documentation

## Overview

The `beancount/core` directory contains the fundamental data structures and logic for the Beancount double-entry bookkeeping system. It defines the "AST" (Abstract Syntax Tree) for accounting data, the basic numeric types, and the core algorithms for aggregation, balancing, and realization of accounts.

This module is designed to be the robust, type-safe foundation upon which parsers, reports, and plugins are built. It favors immutable data structures (mostly `NamedTuple`s) and functional processing patterns.

## Key Concepts & Data Structures

### Basic Types
*   **`Amount`** (`amount.py`): The atomic unit of value, consisting of a `Decimal` number and a currency string (e.g., `100.00 USD`).
*   **`Position`** (`position.py`): Represents a holding within an account. It consists of `units` (an `Amount`) and an optional `cost` (`Cost` object). This structure allows tracking of lots for capital gains calculations.
*   **`Inventory`** (`inventory.py`): A collection of `Position`s. It acts as a "bag" of assets/liabilities and supports arithmetic operations (add, subtract, multiply) and aggregation. It is the primary structure used to represent account balances.

### Directives (The AST)
Defined in `data.py`, these immutable objects represent the parsed content of a Beancount input file. Key directives include:
*   **`Transaction`**: The central element, containing metadata (date, payee, narration) and a list of `Posting`s.
*   **`Posting`**: A single leg of a transaction, referencing an account and an amount (and optionally cost/price).
*   **`Open` / `Close`**: Directives to manage account lifecycles.
*   **`Balance`**: Assertion that an account matches a specific amount at a given date.
*   **`Commodity`**, **`Pad`**, **`Note`**, **`Event`**, **`Query`**, **`Price`**, **`Document`**, **`Custom`**: Other supporting directives.

### Account Management
*   **`account.py`**: Provides utilities for manipulating account names, which are colon-separated strings (e.g., `Assets:Bank:Checking`). Includes validation, parent/child traversal, and component extraction.
*   **`account_types.py`**: Defines the standard root account types (Assets, Liabilities, Equity, Income, Expenses) and logic to classify accounts (e.g., determining if an account is on the Balance Sheet or Income Statement).

### Realization & Balances
*   **`realization.py`**: Contains the logic to transform a flat list of directives into a hierarchical tree of `RealAccount` objects. This process "realizes" the ledger, computing the final `Inventory` (balance) for every account based on its transactions. This tree structure is the basis for generating reports.

### Numeric Handling & Conversion
*   **`number.py`**: Manages `Decimal` precision, quantization, and rounding logic to ensure arithmetic correctness essential for accounting.
*   **`convert.py`**: Functions to convert `Position`s or `Inventory`s into different units (e.g., converting "units at cost" to "market value") using a price map.

### Data Extraction
*   **`getters.py`**: Utility functions to traverse lists of entries and extract specific information, such as all unique accounts, active years, tags, or commodities.

## Diagram: Data Flow

```mermaid
graph TD
    Input[Plain Text File] --> Parser
    Parser --> Entries[List of Directives (data.py)]
    Entries --> Realization[realization.realize()]
    Realization --> RealAccountTree[Tree of RealAccount objects]

    subgraph Core Structures
    RealAccountTree -- contains --> Inventory
    Inventory -- contains --> Position
    Position -- contains --> Amount
    end
```

## Intention

The code in this directory aims to be:
1.  **Correct**: Strictly adhering to double-entry accounting rules.
2.  **Immutable**: Directives are `NamedTuple`s, preventing accidental modification during processing.
3.  **Agnostic**: Separated from the parsing logic (in `beancount/parser`) and output formatting, serving as a clean intermediate representation.
