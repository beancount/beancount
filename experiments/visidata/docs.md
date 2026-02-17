# VisiData Plugin for Beancount

This directory contains an experimental plugin designed to integrate [Beancount](https://beancount.github.io/) with [VisiData](https://www.visidata.org/), a terminal interface for exploring and arranging tabular data.

## Purpose

The goal of this experiment is to allow users to open `.beancount` files directly in VisiData, visualizing the ledger entries as a structured spreadsheet. This facilitates quick data inspection, filtering, and analysis using VisiData's powerful keystroke-driven interface.

## Contents

- **`beancount_visidata_plugin.py`**: The core Python script that implements the VisiData plugin.

## Key Concepts

### `BeancountSheet`
The plugin defines a `BeancountSheet` class (inheriting from `visidata.Sheet`) which serves as the interface between the two systems. It handles:
- **Parsing**: Loading the Beancount file using `beancount.loader`.
- **Row Generation**: Iterating through transactions and their individual postings to create rows.
- **Column Definitions**: Mapping Beancount attributes (e.g., `date`, `payee`, `narration`, `account`, `number`, `currency`, `cost`) to typed VisiData columns.

### Source Navigation
The plugin includes a custom command (bound to `^O`) intended to open the source file in an external editor (specifically configured for Emacs via an `en` launcher) at the exact line number of the selected transaction.

## Status

This is an experimental feature. The code includes comments indicating future work such as:
- Adding custom aggregators for financial data.
- Implementing "balance" columns.
- Supporting "group-by" operations for reporting.
