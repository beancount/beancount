# Beancount Projects

This directory contains standalone tools and scripts built on top of the Beancount core library. These "projects" utilize Beancount's data structures and loading mechanisms to perform specific tasks, such as exporting data for external analysis or reporting.

## Contents

### `export.py` - Portfolio Export Tool

The primary component in this directory is the `export.py` script. Its main purpose is to export Beancount ledger data into CSV format, specifically structured for import into spreadsheet applications (like Google Sheets) for portfolio tracking and analysis.

**Key Features:**

*   **Data Aggregation:** It loads a Beancount file and aggregates data from various sources:
    *   **Postings/Assets:** Enumerates current positions (assets and liabilities), calculating balances by account.
    *   **Account Metadata:** Extracts account-level attributes (e.g., tax status, liquidity), supporting inheritance of metadata from parent accounts.
    *   **Commodity Metadata:** Extracts commodity-level attributes (e.g., asset class, strategy, issuer).
    *   **Market Data:** Extracts the latest prices and exchange rates for relevant commodities and currencies.
*   **Table Generation:** It can generate and output individual CSV tables for each of the above categories.
*   **Unified Export:** It joins all the extracted data into a single, comprehensive master table.
*   **Filtering:** specific rows or options (derivatives) can be filtered out based on metadata tags (e.g., `export: "ignore"`).

**Intended Workflow:**

The output of this script is designed to populate a spreadsheet where users can build custom reports or dashboards. For example, the `upload-to-sheets` tool (mentioned in the source) could be used to push this CSV data to a Google Sheet, where it can be combined with live market data functions (like `GOOGLEFINANCE`).

**Usage:**

The script is a CLI tool built with `click`.

```bash
python3 -m beancount.projects.export [OPTIONS] FILENAME
```

Common options include:
*   `-o`, `--output`: Path to write the final joined CSV table.
*   `-c`, `--output_commodities`: Path to write the commodities table.
*   `-a`, `--output_accounts`: Path to write the accounts table.
*   `-C`, `--currency`: Specify the main operating currency.
