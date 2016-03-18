#!/usr/bin/env python3
"""Publish a list of Beancount holdings to a Google Docs Spreadsheet.

This is intended to publish the list of holdings to a spreadsheet that allows on
to make planning decisions for making changes to a portfolio.

Note: This script has to run under Python 2.x, but Beancount is only implemented
using Python 3.x, so we export the list of Beancount holdings via subprocess and
import them in this script.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import datetime
import logging
import sys
from pprint import pprint
try:
    import StringIO as io
except ImportError:
    import io

import gdrive
from apiclient.http import MediaInMemoryUpload

from beancount import loader
from beancount.reports import holdings_reports
from beancount.reports import table


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = gdrive.get_argparser(description=__doc__.strip())

    parser.add_argument('filename', help="Beancount input file")
    parser.add_argument('docid', help="Document ID")

    parser.add_argument('-o', '--output', action='store',
                        default=datetime.date.today().strftime('beancount.%Y-%m-%d.pdf'),
                        help="Where to write out the collated PDF file")

    args = parser.parse_args()

    # Load the file.
    entries, unused_errors, options_map = loader.load_file(
        args.filename,
        log_timings=logging.info,
        log_errors=sys.stderr)

    # Generate a report.
    holdings_list = holdings_reports.report_holdings(
        None, False, entries, options_map,
        aggregation_key=lambda holding: holding.currency)

    oss = io.StringIO()
    table.table_to_csv(holdings_list, file=oss)
    csv_contents = oss.getvalue()

    # Connect, with authentication.
    # Check https://developers.google.com/drive/scopes for all available scopes.
    scopes = [
        'https://www.googleapis.com/auth/drive',
        ]
    http = gdrive.get_authenticated_http(" ".join(scopes), args)

    # Access the drive API.
    drive = gdrive.discovery.build('drive', 'v2', http=http)

    # Get the document and replace it.
    metadata = drive.files().get(fileId=args.docid).execute()
    upload = MediaInMemoryUpload(csv_contents.encode('utf-8'),
                                 mimetype=metadata['mimeType'])
    metadata = drive.files().update(fileId=args.docid,
                                    media_body=upload).execute()
    pprint(metadata)


if __name__ == '__main__':
    main()
