# Beancount DuckDB Query Examples

This guide provides a variety of "tasty" SQL examples for analyzing your Beancount data using the DuckDB adapter and macros.

## 1. Quick Start Reports
Use the built-in table macros for immediate insights.

### Standard Journal
View a formatted list of recent transactions.
```sql
SELECT * FROM bnj() ORDER BY date DESC LIMIT 20;
```

### Account Balances
Get a summary of all account balances.
```sql
SELECT * FROM bnb();
```

### Specific Account History
Focus on a single account with formatted units.
```sql
SELECT date, payee, narration, bnstr(units) as units
FROM postings
WHERE account = 'Assets:US:BofA:Checking'
ORDER BY date;
```

---

## 2. Financial Aggregations
Leverage DuckDB's vectorized engine for fast totals.

### Monthly Expenses
Total expenses grouped by month.
```sql
SELECT
    year,
    month,
    bnstr(bnsum(pos)) as total_spent
FROM postings
WHERE account LIKE 'Expenses:%'
GROUP BY year, month
ORDER BY year, month;
```

### Income vs. Expenses
A high-level summary of your cash flow.
```sql
SELECT
    CASE WHEN account LIKE 'Income:%' THEN 'Income' ELSE 'Expenses' END as type,
    bnstr(bnsum(pos)) as total
FROM postings
WHERE account LIKE 'Income:%' OR account LIKE 'Expenses:%'
GROUP BY 1;
```

### Top 10 Payees by Volume
Where does the money go?
```sql
SELECT
    payee,
    count(*) as tx_count,
    bnstr(bnsum(pos)) as total_volume
FROM postings
WHERE account LIKE 'Expenses:%'
GROUP BY payee
ORDER BY tx_count DESC
LIMIT 10;
```

---

## 3. Advanced Filtering & Regex
Use DuckDB's powerful string and list functions.

### Querying Tags
Find transactions tagged with 'vacation' or 'trip'.
```sql
SELECT date, payee, narration, tags
FROM postings
WHERE list_contains(tags, 'vacation') OR list_contains(tags, 'trip');
```

### Search by Narration (Regex)
Find any mentions of "amazon" or "google" regardless of case.
```sql
SELECT * FROM bnj()
WHERE narration ~* 'amazon|google';
```

### In-place Replacement
Explore the full table but render the complex types nicely.
```sql
SELECT * REPLACE (bnstr(units) AS units, bnstr(pos) AS pos)
FROM postings
WHERE year = 2024;
```

---

## 4. Working with Complex Types (Lots & Costs)
Query into the nested structures of Beancount positions.

### Finding Lots by Purchase Date
Find all holdings purchased before 2014.
```sql
SELECT account, bnstr(pos)
FROM postings
WHERE pos.cost.date < '2014-01-01'
  AND account LIKE 'Assets:US:ETrade:%';
```

### High-Cost Lots
Find positions where the per-unit cost was greater than 500.
```sql
SELECT account, bnstr(pos)
FROM postings
WHERE pos.cost.number > 500;
```

### Unrealized Gain Calculation (Manual)
Compare the current weight (value at cost) vs units.
```sql
SELECT
    account,
    bnstr(bnsum(pos)) as holdings,
    bnstr(bnsum(weight)) as total_cost
FROM postings
WHERE pos.cost IS NOT NULL
GROUP BY account;
```

---

## 5. Metadata & Context
Query user-defined metadata stored as JSON.

### Custom Metadata Query
Find postings where you attached a custom `invoice` or `receipt` tag.
```sql
SELECT
    date,
    payee,
    posting_meta->>'receipt' as receipt_id
FROM postings
WHERE posting_meta->>'receipt' IS NOT NULL;
```

### File Context
Trace data back to the exact line in your Beancount file.
```sql
SELECT filename, lineno, bnstr(units), account
FROM postings
WHERE number > 10000
ORDER BY number DESC;
```

---

## 6. Prices & Commodities
Analyze your price history.

### Latest Prices
Get the most recent price for each currency.
```sql
SELECT DISTINCT ON (currency)
    currency,
    date,
    bnstr(amount) as price
FROM prices
ORDER BY currency, date DESC;
```
