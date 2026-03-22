# Beancount Core: Go Conversion Plan

This document outlines the detailed plan to port the core logic of Beancount from Python and C to Go. The goal is to maintain the high-level abstractions of the original implementation while leveraging Go's performance, type safety, and concurrency primitives.

---

## 1. Numerical Foundation (`number` and `decimal`)

In Python, Beancount uses the `decimal` module for arbitrary-precision arithmetic. In Go, we must choose a library that provides similar precision and control over rounding.

### Implementation Details:
- **Library Choice:** `github.com/ericlagergren/decimal`. It is high-performance and supports contexts (precision, rounding modes) similar to Python's `decimal` module.
- **Type Alias:** Define a `Decimal` type to wrap the library's decimal type for consistency.
- **Helper Functions:** Implement `D(s string) Decimal` to mirror Python's `D()` for easy construction from strings (handling commas and whitespace).

```go
package core

import "github.com/ericlagergren/decimal"

type Decimal = *decimal.Big

func D(s string) Decimal {
    // Implement parsing logic to handle commas and optional signs
    // similar to beancount/core/number.py
}
```

---

## 2. Fundamental Types (`amount`, `position`, `cost`)

These are the building blocks of every transaction.

### Amount (`amount.go`)
- **Structure:** `struct { Number Decimal; Currency string }`.
- **Immutability:** Methods like `Add`, `Sub`, `Mul`, and `Div` will return *new* `Amount` instances.
- **Formatting:** Implement the `fmt.Stringer` interface to provide formatted output consistent with Beancount's display context.

### Cost and CostSpec (`position.go`)
- **Cost:** Represents a specific lot's cost: `struct { Number Decimal; Currency string; Date time.Time; Label string }`.
- **CostSpec:** Used during parsing/booking for incomplete cost specifications. Fields should be pointers (e.g., `*Decimal`, `*time.Time`) to represent "Missing" or "Null" values.

### Position (`position.go`)
- **Structure:** `struct { Units Amount; Cost *Cost }`.
- **Logic:** Methods for `Value()`, `Weight()`, and comparison (sorting keys based on currency priority).

---

## 3. Data Structures and Directives (`data.go`)

Beancount uses various "Directives" (Open, Transaction, etc.). We will use a Go interface to manage these polymorphically.

### The Directive Interface
```go
type Directive interface {
    GetDate() time.Time
    GetMeta() Meta
    // Potential helper for sorting/filtering
    Type() DirectiveType 
}
```

### Concrete Structs
Each Python `NamedTuple` becomes a Go struct:
- `Transaction`: Contains `Flag`, `Payee`, `Narration`, `Tags` (`set[string]` -> `map[string]struct{}`), `Links`, and `Postings`.
- `Posting`: Contains `Account`, `Units`, `Cost` (or `CostSpec`), `Price`, `Flag`, and `Meta`.
- `Open`, `Close`, `Balance`, `Pad`, `Note`, `Event`, `Query`, `Price`, `Document`, `Custom`.

### Metadata
- **Meta:** Represented as `map[string]any`. This handles the arbitrary key-value pairs attached to entries.

---

## 4. Inventory Management (`inventory.go`)

Inventory is a collection of positions, aggregated by currency and cost.

### Map Key Strategy
In Python, the key is `(currency, cost)`. In Go, since `Cost` contains pointers and `time.Time`, we need a value-based key for the map:
```go
type InventoryKey struct {
    Currency string
    CostHash string // Or a value-copy of the Cost struct
}
```

### Core Logic
- `AddAmount(units Amount, cost *Cost)`: Handles the logic of augmenting or reducing existing lots.
- `Reduce(reducer func(Position) Amount)`: Mirrors Python's `reduce` for calculating total units, total cost (weight), or market value.
- `Average()`: Implementation of the "Average Cost" booking method, merging lots of the same currency.

---

## 5. Account Logic (`account.go` and `account_types.go`)

Accounts are represented as strings with a colon-separated hierarchy.

### Validation and Manipulation
- **Regex:** Use Go's `regexp` package to validate account names.
- **Hierarchy:** Functions for `Parent(account)`, `Leaf(account)`, `Split(account)`, and `Join(components...)`.
- **Account Types:** A struct to hold the root names (Assets, Liabilities, etc.) and logic to determine if an account is a Balance Sheet or Income Statement account.

---

## 6. Realization and Aggregation (`realization.go`)

Realization is the process of organizing postings into a tree structure mirroring the chart of accounts.

### Tree Structure
```go
type RealAccount struct {
    Name     string
    Children map[string]*RealAccount
    Postings []TxnPosting
    Balance  Inventory
}
```
- **Aggregation:** Logic to recursively sum balances from child accounts to parents.

---

## 7. Idiomatic Go Enhancements

While staying close to the original logic, we will improve the implementation using Go's strengths:

- **Error Handling:** Replace Python exceptions with explicit `error` returns, especially in parsing and validation.
- **Concurrency:** Use `goroutines` for parallelizing operations like processing large lists of entries or generating complex reports.
- **Type Safety:** Use concrete types and interfaces to catch errors at compile-time that would be runtime errors in Python.
- **Standard Library:** Leverage `time.Time` for dates and `sort.Slice` for entry ordering.

---

## 8. Development Lifecycle & Parity Testing

To ensure the Go version is correct:
1. **Unit Tests:** Port every test from `beancount/core/*_test.py` to Go.
2. **Golden Files:** Generate output from the Python version and verify that the Go version produces byte-for-byte identical (or logically equivalent) results.
3. **Benchmarks:** Compare the performance of the Go implementation against the Python+C implementation to ensure the expected speedups are achieved.
