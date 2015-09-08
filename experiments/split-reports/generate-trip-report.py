#!/usr/bin/env python3
"""Generate a trip report and upload it to a Google Doc.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import logging
import re
import subprocess
import tempfile
import csv
import itertools


QUERY_EXPENSES = """
  SELECT
     date, payee, narration, PARENT(account),
     number, currency,
     NUMBER(CONVERT(position, "USD")), CURRENCY(CONVERT(position, "USD"))
  WHERE account ~ "{participant}"
    AND number >= 0
"""

QUERY_CONTRIBUTIONS = """
  SELECT
     date, payee, narration, account,
     number, currency,
     NUMBER(CONVERT(position, "USD")), CURRENCY(CONVERT(position, "USD"))
  WHERE account ~ "{participant}"
    AND number < 0
"""


def get_columns_from_underlines(line):
    index = 0;
    columns = []
    for field in re.findall(r'\-+', line):
        end = index + len(field)
        columns.append((index, end))
        index = end + 1
    return columns


def table_to_csv(table_string):
    """Convert a formatted table to CSV rows."""
    lines = iter(table_string.splitlines())
    next(lines)
    columns = get_columns_from_underlines(next(lines))
    rows = []
    for line in lines:
        row = []
        for start, end in columns:
            field = line[start:end]
            row.append(field.strip())
        rows.append(row)
    return rows


def get_participants(filename):
    for line in open(filename):
        match = re.search(r'plugin\s+".*\.split_expenses"\s+"(.*)"', line)
        if match:
            return match.group(1).strip().split()


def query_to_csv(sql, beancount_filename):
    pipe = subprocess.Popen(['bean-query', beancount_filename, sql],
                            stdout=subprocess.PIPE)
    out, _ = pipe.communicate()
    rows = table_to_csv(out.decode('utf8'))
    csvfile = tempfile.NamedTemporaryFile(mode='w', suffix='.csv')
    # Write out the CSV file.
    writer = csv.writer(csvfile)
    writer.writerows(rows)
    csvfile.flush()
    return csvfile


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('docid', help='Spreadsheet document id')
    parser.add_argument('filename', help='Filename')
    args = parser.parse_args()

    participants = get_participants(args.filename)
    logging.info("Participants: %s", participants)

    tmpfiles = []
    upload_arguments = []
    for participant in participants:
        logging.info("Processing %s", participant)
        tmpfiles.append(
            query_to_csv(QUERY_EXPENSES.format(participant=participant),
                         args.filename))
        tmpfiles.append(
            query_to_csv(QUERY_CONTRIBUTIONS.format(participant=participant),
                         args.filename))

    # Upload it to a Google sheet.
    command = (['upload-csv-to-google-sheet', args.docid] +
               ['{}:{}'.format(csvfile.name, index)
                for index, csvfile in enumerate(tmpfiles, 1)])
    logging.info(str(command))
    return_value = subprocess.call(command)
    assert return_value == 0


if __name__ == '__main__':
    main()


# TODO: Make this into a single report, separating credits and debits.
# TODO: Extend upload script to be able to reset the sheet names from its arguments.
# TODO: Idea: add current location as a column.
