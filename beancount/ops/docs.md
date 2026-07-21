# Beancount Operations (`beancount.ops`)

This directory contains modules that perform high-level operations on lists of Beancount entries (directives). While `beancount.core` defines the fundamental data structures, `beancount.ops` provides the machinery to manipulate, validate, transform, and analyze streams of these entries.

These operations typically take a list of entries (and often an options map) as input and return a modified list of entries, errors, or derived data structures.

## Key Concepts and Modules

The operations can be broadly categorized into the following areas:

### Validation and Integrity
These modules ensure the consistency and correctness of the ledger.

- **`validation.py`**: Performs final sanity checks on entries (e.g., ensuring accounts are open before use, checking for duplicate directives). These run after plugins have processed the entries.
- **`balance.py`**: Implements the logic for `balance` assertions. It verifies that the running balance of an account matches the asserted amount at a specific point in time.
- **`pad.py`**: Implements the `pad` directive logic. It automatically inserts transactions to bring an account's balance to a target value, often used to correct imbalances or initialize accounts.

### Transformation and Summarization
These modules modify the stream of entries to simplify or summarize data.

- **`summarize.py`**: Provides functionality to "clamp" a ledger to a specific time window. It can replace all history prior to a start date with synthetic "Opening Balance" transactions, effectively truncating the history while preserving account balances.
- **`compress.py`**: Compresses sequences of similar transactions into single summary transactions. This is useful for reducing the volume of data (e.g., aggregating daily interest payments into a monthly entry).

### Filtering and Querying
Utilities for extracting specific subsets of data or metadata.

- **`basicops.py`**: Contains basic filtering and aggregation tools, such as filtering entries by tag or link, and grouping entries.
- **`find_prices.py`**: Analyzes transactions to determine which currencies require price fetching (e.g., currencies held at cost or converted).
- **`lifetimes.py`**: Computes the time intervals during which specific commodities are held. This helps in optimizing price fetching by only querying prices for relevant periods.

### External Resources
- **`documents.py`**: Handles the automatic discovery and association of external document files (PDFs, images) with Beancount transactions based on file system paths and rules.

## Usage Context

The functions in this package are primarily used by the Beancount loader (`beancount.loader`) and various tools (like the report generator or the price fetcher) to prepare the raw parsed data for consumption. They represent the "business logic" layer that operates on the raw data structures defined in `core`.
