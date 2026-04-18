# Design Proposal: Beancount to DuckDB Adapter

This document proposes a design for converting Beancount ledgers into a DuckDB database, optimized for analytical queries and leveraging DuckDB's advanced type system.

## 1. Objectives
- **Flat Posting Table**: Provide a single, comprehensive table of postings for easy querying.
- **Rich Data Types**: Use DuckDB's `STRUCT`, `LIST`, and `JSON` types to represent complex Beancount structures.
- **Compatibility**: Maintain column name compatibility with `beanquery` where possible.
- **Performance**: Utilize DuckDB's columnar storage and vectorized execution for fast aggregations.
- **Precision**: Ensure high precision for all financial calculations using `DECIMAL(38,18)`.

## 2. Custom Types
To improve readability and enable future extensions (like custom aggregate functions), we define several custom types in DuckDB:

```sql
-- Represents a financial amount
CREATE TYPE Amount AS STRUCT(
    number DECIMAL(38,18),
    currency VARCHAR
);

-- Represents a lot cost
CREATE TYPE Cost AS STRUCT(
    number DECIMAL(38,18),
    currency VARCHAR,
    date DATE,
    label VARCHAR
);

-- Represents a position (units + cost)
CREATE TYPE Position AS STRUCT(
    units Amount,
    cost Cost
);
```

## 3. Schema Design: The `postings` Table

The `postings` table is the primary source for financial reports. It flattens the Transaction-Posting relationship.

| Column Name | DuckDB Type | Description |
| :--- | :--- | :--- |
| **Transaction Info** | | |
| `id` | VARCHAR | Unique hash of the transaction. |
| `date` | DATE | Transaction date. |
| `tx_flag` | VARCHAR | Transaction flag (e.g., '*', '!'). |
| `payee` | VARCHAR | Payee name. |
| `narration` | VARCHAR | Transaction narration. |
| `description` | VARCHAR | Combined `payee \| narration`. |
| `tags` | VARCHAR[] | List of transaction tags. |
| `links` | VARCHAR[] | List of transaction links. |
| `tx_meta` | JSON | Transaction metadata. |
| **Posting Info** | | |
| `account` | VARCHAR | Account name. |
| `number` | DECIMAL(38,18) | Number of units (shorthand for `units.number`). |
| `currency` | VARCHAR | Currency of units (shorthand for `units.currency`). |
| `posting_flag` | VARCHAR | Posting-specific flag. |
| `posting_meta` | JSON | Posting-specific metadata. |
| **Rich Objects** | | |
| `units` | Amount | Units as a struct. |
| `cost` | Cost | Cost as a struct (null if no cost). |
| `position` | Position | Position as a nested struct. |
| `price` | Amount | Attached price (null if no price). |
| `weight` | Amount | Computed weight of the posting. |
| **Context** | | |
| `filename` | VARCHAR | Source file path. |
| `lineno` | INTEGER | Line number in source file. |
| `year` | INTEGER | Extracted year from date. |
| `month` | INTEGER | Extracted month from date. |
| `day` | INTEGER | Extracted day from date. |

## 4. Derived Tables (Optional but Recommended)

To support full ledger conversion, we can also include:

- `prices`: `(date, currency, amount Amount)`
- `accounts`: `(name, open_date, close_date, currencies VARCHAR[], meta JSON)`
- `commodities`: `(currency, meta JSON)`

## 5. Improvements over Previous Work (SQLite)

1. **Vectorized Execution**: DuckDB's engine is significantly faster for large-scale aggregations (e.g., `SUM(number) GROUP BY account`).
2. **Native Arrays**: Using `VARCHAR[]` for tags and links allows for efficient filtering using DuckDB's list functions (e.g., `list_contains(tags, 'tax')`).
3. **JSON Metadata**: Storing metadata as JSON allows for flexible querying of arbitrary metadata fields without schema changes.
4. **Structured Positions**: Grouping `units` and `cost` into a `Position` struct makes it easier to pass these objects to custom UDFs or future aggregation logic.
5. **No Normalization Overheads**: By providing a flat postings table, we avoid complex joins that were required in the SQLite implementation, while still preserving the ability to query transaction-level data.

## 6. Implementation Strategy

1. **Loader**: Use `beancount.loader.load_file` to get the list of entries.
2. **Type Mapping**: Map Beancount's `Decimal` to DuckDB `DECIMAL(38,18)` and `datetime.date` to `DATE`.
3. **Batch Insertion**: Use DuckDB's Python API to perform batch insertions of posting rows.
4. **Metadata Handling**: Convert Beancount's metadata dictionaries (minus file/line info) to JSON strings for insertion.
