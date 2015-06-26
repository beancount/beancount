#!/usr/bin/env python
"""Publish a CSV file to a Google Docs Spreadsheet sheet.

Note: This script has to run under Python 2.x because gdata (the old
"gdata-python-client" XML-based APIs) has not yet been ported to Python 3, and
it looks abandoned by Google in favor of the discovery apis (i.e.,
"google-api-python-client").

Beancount is only implemented using Python 3.x, so we will support exporting the
list of holdings to a temporary CSV file and uploading it automatically to a
spreadsheet using a variant of this script. The CSV data format should be enough
of a bridge for us to get our work done.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import csv
import codecs
import datetime
import logging
import functools
import urllib
import os
import shutil
import tempfile
import re
import subprocess
import StringIO
from os import path
from decimal import Decimal as D

import gdata.spreadsheets.client
import gdata.spreadsheets.data
import gdata.gauth
import gdata

import gauth2


def get_spreadsheets_client(args):
    """Connect and create a SpreadsheetsClient object.

    Args:
      args: An argparse values object.
    Returns:
      An instance of SpreadsheetsClient.
    """
    # Connect, with authentication.
    scopes = ['https://spreadsheets.google.com/feeds']
    http, credentials = gauth2.get_authenticated_http(scopes, args)

    # Create a spreadsheet client.
    client = gdata.spreadsheets.client.SpreadsheetsClient()
    client.auth_token = gdata.gauth.OAuth2TokenFromCredentials(credentials)

    return client


def csv_unicode_reader(source, **kwargs):
    """Temporarily encode unicode source to UTF8 because CSV does not support unicode.

    Args:
      source: An iterable sequence of unicode objects.
      **kwargs: Arguments for csv.reader().
    Yields:
      Rows of unicode objects.
    """
    lineiter = (line.encode('utf-8') for line in source)
    csv_reader = csv.reader(lineiter, **kwargs)
    for row in csv_reader:
        yield [cell.decode('utf-8') for cell in row]


def csv_to_batch_update(filename, batch):
    """Convert a CSV file to a batch update.

    Args:
      filename: A string, the CSV filename to open and parse.
      batch: A CellsFeed instance.
    """
    # Open the CSV file and loop over the values, creating a batch request.
    csvfile = codecs.open(filename, 'r', encoding='utf-8-sig')
    for irow, row in enumerate(csv_unicode_reader(csvfile), 1):
        logging.info("Row: %s", row)
        for icol, value in enumerate(row, 1):
            batch.add_set_cell(irow, icol, value)


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = gauth2.get_argparser(description=__doc__.strip())

    parser.add_argument('filename', action='store',
                        help="CSV file to upload")

    parser.add_argument('docid', action='store',
                        help="Spreadsheets doc id to update")

    parser.add_argument('--worksheet', action='store',
                        help="Specific worksheet to update (default is the first one)")

    args = parser.parse_args()
    docid = args.docid

    client = get_spreadsheets_client(args)

    # Get the worksheet id.
    query = (gdata.spreadsheets.client.WorksheetQuery(args.worksheet)
             if args.worksheet
             else None)
    worksheets = client.get_worksheets(docid, query=query)
    wsid = worksheets.entry[0].get_worksheet_id()

    # Open the CSV file and loop over the values, creating a batch request.
    batch = gdata.spreadsheets.data.build_batch_cells_update(docid, wsid)
    csv_to_batch_update(args.filename, batch)
    client.batch(batch, force=True)


if __name__ == '__main__':
    main()
