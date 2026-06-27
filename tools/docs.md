# Beancount Developer Tools

This directory contains internal tools used for the development, maintenance, and testing of Beancount itself. These scripts are intended for developers working on the Beancount codebase and are **not** meant to be installed, packaged, or used by end-users.

## Overview

The tools in this directory cover various aspects of the development lifecycle, including:

-   **Code Quality & analysis:** Scripts to enforce dependency constraints, check function arguments, and find missing tests.
-   **Documentation:** Utilities to generate and update documentation from source code or external documents.
-   **Code Maintenance:** Tools to update copyright headers and manage source code formatting.
-   **Code Generation:** Scripts to generate boilerplate code or regular expressions.
-   **Benchmarking:** Tools to measure performance changes across revisions.

## Tool Categories

### Code Quality & Analysis

*   `check_num_args.py`: Counts the number of arguments used to call specific functions, helping to identify potential refactoring targets or API misuse.
*   `dependency_constraints.py`: Enforces module dependency ordering to prevent circular dependencies and maintain a clean architecture (uses `snakefood`).
*   `find_missing_tests.py`: Scans the codebase to identify source files that lack corresponding test files, ensuring comprehensive test coverage.
*   `find_multiline_returns.py`: Identifies multi-line `Returns` sections in docstrings to ensure compatibility with Sphinx documentation generation.
*   `parse_pylint_error_types.py`: Extracts and summarizes error types from Pylint output.

### Documentation

*   `dump_options_map.py`: Loads and prints the options map from a Beancount file, useful for debugging configuration loading.
*   `sphinx_odt2rst.py`: Converts OpenOffice (ODT) documents to reStructuredText (RST) for integration into the Sphinx-based documentation.
*   `transform_links_in_docs.py`: Updates hyperlinks within documentation files based on a provided mapping JSON, useful for fixing broken links or migrating content.
*   `update_options.py`: Automatically updates the options documentation (e.g., in Google Docs) based on the source code definitions.
    *   `update_options_test.py`: Unit tests for `update_options.py`.

### Code Maintenance

*   `update_copyright.py`: Updates the copyright headers in source files to include the current year and ensure consistency.
*   `benchmark.py`: Estimates and compares the performance of different Beancount revisions to track regressions or improvements.
*   `github_sync.txt`: A text file containing notes/instructions on how to sync changes to the GitHub mirror (likely for historical context).

### Code Generation

*   `generate_interpolation_permutations.py`: Generates all combinations of missing accounts for testing interpolation logic.
*   `generate_unicode_category_regexps.py`: Generates Python or Lex code snippets containing regular expressions for specific Unicode categories.

## Usage

These scripts are often invoked via the root `Makefile` or manually by developers during the contribution process. They generally require the Beancount source code to be in the python path.
