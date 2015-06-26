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
import logging
import os
from os import path

import oauth2client.client
from oauth2client import tools
from oauth2client.file import Storage
import httplib2

import gdata.spreadsheets.client
import gdata.spreadsheets.data
import gdata.gauth


#-------------------------------------------------------------------------------
# gauth.py


DEFAULT_SECRETS_FILENAME = os.environ.get('GOOGLE_APIS', None)
DEFAULT_STORAGE_FILENAME = path.join(os.environ['HOME'], '.oauth2-google-api')


def get_argparser(**kwds):
    """Create an argument parser for connnecting to the Google Drive API.

    You may further add arguments to this.

    Args:
      parser: An instance of an argparse parser.
    Returns:
      A suitable ArgumentParser object.
    """
    parser = argparse.ArgumentParser(parents=[tools.argparser], **kwds)

    parser.add_argument('--secrets', action='store',
                        default=DEFAULT_SECRETS_FILENAME,
                        help="Secrets filename")

    parser.add_argument('--storage', action='store',
                        default=DEFAULT_STORAGE_FILENAME,
                        help="Storage filename")

    return parser


def get_authenticated_http(scopes, args):
    """Authenticate via oauth2 and cache credentials to a file.

    If the credentials are already available in the 'storage' cache file, this
    function will not require user interaction, it will simply return the cached
    credentials; otherwise, it opens up a browser window for the user to accept
    the access and obtain the credentials.

    Args:
      scopes: A string or a list of strings, the scopes to get credentials for.
      args: An argparse option values object, as retrurned by parse_args().
        This arguments value object must include attributes for secrets_filename
        and storage_filename as per get_argparser().
    Returns:
      An authenticated http client object, from which you can use the Google
      APIs.
    """
    # secrets_filename: A string, the filename that contains information
    #   identifying the client application and secret (Note: this is not the
    #   credentials/token).
    secrets_filename = args.secrets

    # storage_filename: A string, a path to the filename where to cache the
    #   credentials between runs.
    storage_filename = args.storage

    # Create a flow from a secrets file.
    scope = ' '.join(scopes) if isinstance(scopes, list) else scopes
    flow = oauth2client.client.flow_from_clientsecrets(secrets_filename, scope)
    flow.redirect_uri = oauth2client.client.OOB_CALLBACK_URN

    # Create a transport, disable SSL certificates, which fails to validate.
    http = httplib2.Http()

    # Create a storage to cache the credentials for future runs, and look it up.
    storage = Storage(storage_filename)
    credentials = storage.get()
    if credentials is None:
        # Save and restore the logger level, because the flow somehow overrides it.
        saved_log_level = logging.getLogger().level
        try:
            # If the credentials haven't been found, run the flow. This will pop-up
            # a web browser window for you to accept.
            credentials = tools.run_flow(flow, storage, args, http=http)
        finally:
            logging.getLogger().setLevel(saved_log_level)

    # Authorize using the transport and return it.
    credentials.authorize(http)

    # Refresh the access token if necessary.
    if credentials.access_token_expired:
        credentials.refresh(http)

    return http, credentials


# gauth.py
#-------------------------------------------------------------------------------


def get_spreadsheets_client(args):
    """Connect and create a SpreadsheetsClient object.

    Args:
      args: An argparse values object.
    Returns:
      An instance of SpreadsheetsClient.
    """
    # Connect, with authentication.
    scopes = ['https://spreadsheets.google.com/feeds']
    http, credentials = get_authenticated_http(scopes, args)

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

    parser = get_argparser(description=__doc__.strip())

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

    logging.info("Done.")


if __name__ == '__main__':
    main()
