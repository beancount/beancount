# Documentation Conversion Tools (`experiments/docs_rst`)

This directory contains scripts and tools to automate the downloading and conversion of Beancount documentation from Google Docs to reStructuredText (RST). This process is intended to be run before each release to publish the documentation to a static website.

## Workflow

The workflow is orchestrated by the `Makefile` and consists of three main steps:

1.  **Download**: Fetch the latest versions of the documentation from Google Drive.
2.  **Convert**: Transform the downloaded documents (primarily `.docx`) into reStructuredText format.
3.  **Copy**: Move the converted files to the target static site generator source directory.

## Key Files

*   **`download_docs.py`**:
    *   Connects to the Google Drive API to download documentation files.
    *   Fetches documents in various formats (HTML, TXT, ODT, DOCX, EPUB) to determine the best source for conversion.
    *   Handles file renaming and organization into a local directory.

*   **`convert_docs.py`**:
    *   Performs the conversion of downloaded documents to RST.
    *   Includes specialized logic to parse `.docx` XML directly (using `zipfile` and `BeautifulSoup`) to correctly extract code blocks and fixed-width text, which are often mishandled by standard converters like Pandoc.
    *   Post-processes the output to fix indentation, blockquotes, and other formatting artifacts.

*   **`copy_docs.py`**:
    *   Maps specific Google Doc IDs (folder-like hashes) to their corresponding destination paths in the documentation structure (e.g., `users/installation.rst`).
    *   Copies the converted files to the external `beancount-docs` repository or directory.

*   **`notes-about-formats.txt`**:
    *   Contains analysis and observations regarding the quality of different export formats from Google Docs.
    *   Concludes that `docx` is the most suitable format for extracting structure, despite some issues that require custom handling.

*   **`Makefile`**:
    *   Provides `download`, `convert`, and `copy` targets to run the respective Python scripts.
    *   Defines configuration variables like `CONVERT_DOCS` (download location) and `RST_DOCS` (destination for converted files).

## Usage

The intended usage is via `make`:

```bash
make download   # Download docs from Google Drive
make convert    # Convert downloaded docs to RST
make copy       # Copy converted docs to the static site repo
```
