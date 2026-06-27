# Tools

This directory contains standalone tools and scripts that are useful for working with Beancount data or similar hierarchical text formats. These tools are designed to be independent and do not rely on the core Beancount library.

## Contents

### `treeify.py`

A script that identifies a column of text containing hierarchical identifiers (e.g., `Assets:US:Bank:Checking`) and transforms it into a tree-like ASCII structure. It preserves the vertical alignment of surrounding text.

**Key Features:**
- Detects hierarchical columns automatically or via custom patterns.
- Renders a visual tree structure using ASCII characters.
- Maintains the formatting of other columns in the file.
- Can be used as a standalone command-line tool.

**Example Usage:**

Input:
```
2014-10-19 Assets:Home:123ForestLane                    450,000.00 USD
2014-12-25 Assets:US:BofA:Checking                        5,545.01 USD
```

Output:
```
           |-- Assets
           |   |-- Home
2014-10-19 |   |   `-- 123ForestLane                    450,000.00 USD
           |   `-- US
           |       `-- BofA
2014-12-25 |           `-- Checking                       5,545.01 USD
```

### `treeify_test.py`

Contains unit tests for the `treeify.py` script, ensuring it correctly handles various edge cases, alignment scenarios, and overlapping columns.
