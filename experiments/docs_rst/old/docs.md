# Old Documentation Conversion Tools

This directory contains experimental and legacy scripts designed to automate the downloading and conversion of Beancount's documentation from Google Docs.

## Purpose

The primary goal of these scripts is to pull documentation authored in Google Docs and convert it into offline formats like PDF, Markdown, or reStructuredText (RST). This supports the workflow of maintaining documentation in Google Docs while generating static site documentation or offline manuals.

## Main Concepts

*   **Google Drive API Integration**: The scripts use the Google Drive API (via `apiclient`) to search for, list, and export documents. Authentication is handled via service accounts.
*   **Index-Based Discovery**: The process typically starts by locating a "Beancount - Index" document and parsing it to find links to all other relevant documentation chapters.
*   **Format Conversion**: The tools support downloading documents in various formats (DOCX, PDF, ODT, HTML, TXT) and utilizing tools like `pandoc` and `pdftk` for further processing (e.g., converting DOCX to Markdown, merging multiple PDFs).

## File Descriptions

*   **`docs.py`**: A shared utility library containing the core logic for API authentication, caching, file discovery, downloading, and PDF merging.
*   **`download_docs.py`**: A script that downloads *all* linked documentation from the Beancount Index. It can export to various formats and merge PDFs into a single file.
*   **`convert_doc.py`**: A script focused on downloading and converting a *single* Google Doc. It attempts to use `pandoc` with a custom filter to generate Markdown.
*   **`convert_filter_docx.py`**: A simple `pandoc` filter (used by `convert_doc.py`) intended to customize the conversion process (e.g., handling blockquotes).
*   **`docs_test.py`**: Unit tests for the documentation utilities.
