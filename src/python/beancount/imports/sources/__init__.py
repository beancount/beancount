"""Importers for various sources of data.

Each of the modules must provide two resources:

1. A 'CONFIG' dict that declares the list of options required for the importer
   configuration to be provided with, and their meaning;

2. An 'import_file' function that accepts

   - The filename to be imported;

   - The configuration dictionary provided by the user's config (this must
     match the list of parameters declared in the 'CONFIG' above).

   This function must return a list of imported entries.

The importer will attempt de-duplication and auto-categorization by itself.
"""
