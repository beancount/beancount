# Tutorial Examples Output Directory

This directory contains reference output files corresponding to the [Beancount Tutorial](http://furius.ca/beancount/doc/tutorial).

## Purpose

These files serve as a "golden" reference for users following the tutorial. They demonstrate the expected output of various Beancount tools (like `bean-report` and `bean-query`) when run against the tutorial's example ledger. By comparing their own results with these files, users can verify they are performing the steps correctly.

## Content Overview

The files are named to reflect the command or report they represent. Most are text-based output files (`.output`).

### Key Files

-   **Report Outputs:**
    -   `balances.output`: Expected output for the balances report.
    -   `journal.output`: Expected output for the journal report, showing transaction history.
    -   `networth.output`: Expected output for a net worth report.
    -   `holdings.output`: Expected output for holdings reports.
    -   `balsheet.output`: Expected output for the balance sheet.

-   **Specific Scenarios:**
    -   `invest.output` & `invest-with-cost.output`: Examples involving investment tracking and cost basis.
    -   `balances-tree.output`: Balance reports formatted as a tree.
    -   `events.output`: Output showing event entries.

## Usage

These files are not intended to be executed. They are static text files used for verification and as examples of what Beancount's reporting tools produce. Users should refer to the official tutorial documentation for the commands that generate these outputs.
