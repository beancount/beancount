# Beancount Simple Examples

This directory contains introductory example files for [Beancount](https://beancount.github.io/docs/), a double-entry bookkeeping computer language. These files are designed to demonstrate syntax, features, and best practices for structuring a ledger.

## Contents

### `basic.beancount`
This file serves as a comprehensive demonstration of Beancount's features. It includes a wide variety of transaction types and scenarios, making it a useful reference for syntax and usage.

**Key Features Demonstrated:**
- **Multi-currency and Commodities:** Handling USD, CAD, and stock tickers like AAPL and EWJ.
- **Transaction Types:** Salary income, daily expenses, ATM withdrawals, mortgage payments, credit card usage, and stock trading (buying, selling, dividends).
- **Directives:** Usage of `open`, `pad`, `balance`, `price`, `event`, and options.
- **Tags:** Demonstrates `pushtag` and `poptag` to group related transactions (e.g., a trip).
- **Comments:** Extensive comments explaining the logic behind specific entries.

### `starter.beancount`
This file is intended as a template for new users to copy and customize when starting their own Beancount ledger. It focuses on providing a clean, logical structure rather than showing every possible feature.

**Key Features Demonstrated:**
- **Hierarchical Structure:** Adopts a pragmatic account hierarchy: `Type:Country:Institution:Account` (e.g., `Assets:US:BofABank:Checking`).
- **Initialization Strategy:** Shows how to use `pad` directives against `Equity:Opening-Balances` to set initial balances without entering historical data.
- **Organization:** Uses Emacs org-mode style headings to organize the file by institution and account type.
- **Guidance:** Contains instructional comments guiding the user on how to adapt the file (e.g., defining options, setting up assets/liabilities, uncommenting expense categories).

## Main Concepts

Both files illustrate core Beancount concepts:

1.  **Account Lifecycle:** Accounts must be opened (`open`) before use.
2.  **Double-Entry:** Every transaction must balance (sum to zero).
3.  **Commodities:** Accounts can hold any commodity (currencies, shares, property).
4.  **Assertions:** The `balance` directive is used to assert the quantity of a commodity in an account at a specific point in time, ensuring data integrity.
5.  **Padding:** The `pad` directive helps in initializing balances or filling gaps by automatically inserting a transaction to match a subsequent `balance` assertion.

## Usage

- **For Learning:** Review `basic.beancount` to see examples of how to record specific financial events.
- **For Starting:** Copy `starter.beancount` to a new file (e.g., `my-ledger.beancount`), clear the example data, and begin defining your own accounts and opening balances.
