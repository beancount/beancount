#!/usr/bin/env python3
"""Alternative import configuration as Python script.

Instead of running the bean-identify, bean-extract and bean-file tools, you can
instead write a single Python script and call
beancount.ingest.scripts_utils.ingest(). Calling this function invokes a script
runner that accepts one of three subcommands: identify, extract or file,
corresponding to the bean-identify, bean-extract and bean-file programs.

Calling the runner yourself from Python also allows you to provide filter
functions, or "hooks" to post-process the list of extracted entities. We've put
an example below that doesn't do much, but you could do various things, e.g.
make your own heuristic to detect duplicate entries, merge entries from multiple
files, reorder the entries in your favorite order, clean up your payee names,
add some metadata, etc. These hooks run on the final output and independently of
the importers.

Note that this script is otherwise identical to the "example.import" file in
this directory.
"""

# Insert our custom importers path here.
# (In practice you might just change your PYTHONPATH environment.)
import sys
from os import path
sys.path.insert(0, path.join(path.dirname(__file__)))

from importers.utrade import utrade_csv
from importers.acme import acme_pdf

from beancount.core import data
from beancount.ingest import scripts_utils
from beancount.ingest import extract
from beancount.ingest.importers import ofx


# Setting this variable provides a list of importer instances.
CONFIG = [
    utrade_csv.Importer("USD",
                        "Assets:US:UTrade",
                        "Assets:US:UTrade:Cash",
                        "Income:US:UTrade:{}:Dividend",
                        "Income:US:UTrade:{}:Gains",
                        "Expenses:Financial:Fees",
                        "Assets:US:BofA:Checking"),

    ofx.Importer("379700001111222",
                 "Liabilities:US:CreditCard",
                 "bofa"),

    acme_pdf.Importer("Assets:US:AcmeBank"),
]


# Override the header on extracted text (if desired).
extract.HEADER = ';; -*- mode: org; mode: beancount; coding: utf-8; -*-\n'


def clean_up_descriptions(extracted_entries):
    """Example filter function; clean up cruft from narrations.

    Args:
      extracted_entries: A list of directives.
    Returns:
      A new list of directives with possibly modified payees and narration
      fields.
    """
    clean_entries = []
    for entry in extracted_entries:
        if isinstance(entry, data.Transaction):
            if entry.narration and " / " in entry.narration:
                left_part, _ = entry.narration.split(" / ")
                entry = entry._replace(narration=left_part)
            if entry.payee and " / " in entry.payee:
                left_part, _ = entry.payee.split(" / ")
                entry = entry._replace(payee=left_part)
        clean_entries.append(entry)
    return clean_entries

def process_extracted_entries(extracted_entries_list, ledger_entries):
    """Example filter function; clean up cruft from narrations.

    Args:
      extracted_entries_list: A list of (filename, entries) pairs, where
        'entries' are the directives extract from 'filename'.
      ledger_entries: If provided, a list of directives from the existing
        ledger of the user. This is non-None if the user provided their
        ledger file as an option.
    Returns:
      A possibly different version of extracted_entries_list, a list of
      (filename, entries), to be printed.
    """
    return [(filename, clean_up_descriptions(entries))
            for filename, entries in extracted_entries_list]

# Invoke the script.
scripts_utils.ingest(CONFIG, detect_duplicates_func=process_extracted_entries)
