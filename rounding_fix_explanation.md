# Technical Report: Rounding Inconsistency in Transaction Interpolation

## Overview
In Beancount, users can leave one "leg" of a transaction blank (without an amount). Beancount automatically calculates or "interpolates" the missing value so that the transaction balances to zero. 

A bug was identified where this interpolation process used inconsistent rounding, leading to unbalanced accounts in reports (e.g., `bean-query` or `bean-report`).

## The Problem

### 1. Inferred Precision
Beancount looks at the numbers provided in a transaction to guess the "tolerance" (the allowed rounding error). 
- `4.8 EUR` implies a precision of 1 decimal place (0.1), resulting in a tolerance of **0.05**.
- `2.97 EUR` implies a precision of 2 decimal places (0.01), resulting in a tolerance of **0.005**.

### 2. The Conflicting Goals of Tolerances
Tolerances in Beancount serve two conflicting purposes:
1.  **Validation:** When checking if a transaction balances, Beancount should be **permissive** (using the *largest* tolerance) to accommodate small rounding errors from external sources.
2.  **Interpolation:** When filling in a missing number, Beancount should be **precise** (using the *smallest* tolerance) to match the most detailed information provided.

### 3. The Bug
Prior to this fix, Beancount used a single tolerance for both purposes: the **Maximum** (loosest) one.

In the following example:
```beancount
2026-04-28 * "Test"
  Expenses:Test 4.8 EUR    ; Implies 0.05 tolerance
  Expenses:Test 2.97 EUR   ; Implies 0.005 tolerance
  Assets:Test              ; Missing amount
```
Beancount chose **0.05** as the tolerance. When calculating the missing amount (`4.8 + 2.97 = 7.77`), it applied the 0.05 tolerance, which caused it to round `7.77` to `7.8`. 

This resulted in an internally unbalanced transaction:
- Total Expenses: `7.77 EUR`
- Total Assets: `-7.8 EUR`
- **Resulting Error:** `0.03 EUR` left "hanging" in the account balances.

## The Proposed Fix

The fix introduces a "Dual-Tolerance" logic during the booking process.

### 1. Refactored Tolerance Inference
The `infer_tolerances` function was updated to support a `mode` parameter:
- `mode="max"` (Default): Returns the loosest tolerance (best for validation).
- `mode="min"`: Returns the finest tolerance (best for interpolation).

### 2. Precise Interpolation
The interpolation logic in `beancount/parser/booking_full.py` was modified to:
1.  Calculate the **minimum** tolerance seen in the transaction.
2.  Use this precise tolerance to quantize the missing amount. 
    *   In the example above, it now uses **0.005**, so `7.77` remains `7.77`.

### 3. Permissive Validation
The transaction metadata still records the **maximum** tolerance. This ensures that:
- The transaction still passes the standard "does it balance?" check (which allows for errors up to 0.05).
- Existing user files that rely on loose balancing do not suddenly start failing.

## Result
After the fix, the transaction is correctly resolved as:
```beancount
2026-04-28 * "Test"
  Expenses:Test   4.80 EUR
  Expenses:Test   2.97 EUR
  Assets:Test    -7.77 EUR
```
The accounts now balance perfectly to zero, and reports like `balances` reflect the correct, precise amounts for every account.
