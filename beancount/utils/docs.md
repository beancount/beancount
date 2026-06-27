# Beancount Utilities (`beancount/utils`)

This directory contains a collection of general-purpose utility modules used throughout the Beancount project. These modules provide standalone functionality that is not specific to double-entry bookkeeping but is required for the application's infrastructure, testing, and data manipulation.

## Module Overview

### Data Structures & Algorithms
- **`bisect_key.py`**: Extends the standard `bisect` module to support custom key functions, allowing binary searches on lists of objects using specific attributes.
- **`defdict.py`**: Implements `ImmutableDictWithDefault`, a dictionary variant that provides a default value for missing keys without mutation.
- **`misc_utils.py`**: A grab-bag of algorithms including `groupby` (grouping by key), `uniquify` (removing duplicates while preserving order), `is_sorted`, and `skipiter`.

### File System & IO
- **`file_utils.py`**: Helpers for file system operations, such as enumerating files (`find_files`), guessing file formats, and manipulating file paths.
- **`encryption.py`**: Utilities to detect and read GPG-encrypted files, handling decryption transparently.
- **`pager.py`**: Provides a context manager (`ConditionalPager`) to pipe output to a system pager (like `less`) only when the output exceeds a certain length.
- **`memo.py`**: Decorators for memoizing function calls, specifically those returning file objects, using a persistent cache.
- **`import_utils.py`**: Functions to dynamically import symbols from string paths (e.g., `"package.module.Class"`).

### Formatting & Display
- **`table.py`**: A comprehensive toolkit for creating and rendering tables. It supports converting data to a standardized `Table` structure and rendering it as ASCII text, HTML, or CSV.
- **`misc_utils.py`**: Includes helpers for terminal size detection (`get_screen_width`) and string escaping.

### Testing & Development
- **`test_utils.py`**: An extensive set of utilities for unit testing, including:
  - Context managers for temporary files and directories.
  - Helpers to capture `stdout` and patch objects.
  - Custom test cases (`TmpFilesTestBase`, `ClickTestCase`).
  - Tools to find the repository root.
- **`invariants.py`**: Decorators to instrument classes with pre- and post-condition checks, useful for verifying internal state during tests.
- **`errors.cc/h`**: C++ definitions for system error status codes, bridging C++ and Python error handling.

## Usage
These utilities are intended to be imported as needed by other parts of the Beancount codebase. They are generally independent of the core Beancount model (files in `beancount/core`), making them reusable generic components.
