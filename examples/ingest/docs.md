# Beancount Ingestion Examples

This directory provides example artifacts and data structures used for regression testing within the Beancount ingestion framework. It serves as a reference for organizing test data to verify the correctness of document importers.

## Purpose

The primary goal of this directory is to demonstrate how to separate test data from importer implementation logic. By storing "golden" files—representing the expected output of various ingestion stages—developers can ensure that changes to importer code do not introduce regressions in text extraction or account identification.

## Directory Structure

The directory is organized hierarchically to mirror the structure of potential importers:

*   **`office/`**: A category for office-related document importers.
    *   **`importers/`**: Contains subdirectories for individual importer test cases.
        *   **`acme/`**: A concrete example for a hypothetical "Acme Bank" importer.

## Key Artifacts & Concepts

The examples utilize specific file extensions to represent different stages of the ingestion testing process:

*   **`.extract` Files** (e.g., `acmebank1.pdf.extract`)
    *   **Content:** The raw text content extracted from a source document (like a PDF).
    *   **Usage:** Validates the underlying text extraction logic. Comparing new extraction results against this file helps detect changes in how text is parsed from the source format.

*   **`.file_account` Files** (e.g., `acmebank1.pdf.file_account`)
    *   **Content:** The target Beancount account name (e.g., `Assets:US:AcmeBank`).
    *   **Usage:** Verifies the "filing" logic. It ensures that the importer correctly identifies which account a document belongs to, based on its filename or content.

## Usage

These files are typically used in conjunction with a regression testing harness. The harness runs the importers against source documents (which may or may not be included in the repository) and compares the generated output against these expected artifacts. Any discrepancy signals a potential regression in the importer's behavior.
