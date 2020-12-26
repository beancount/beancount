#!/usr/bin/env python3
"""Publish a CSV file to a Google Spreadsheet sheet.

This is a convenient script to update an existing Google Spreadsheet document
with the contents of a CSV file.

NOTE: This script replaces and obsoletes 'upload-csv-to-google-sheet'.

For example, this invocation creates a new Google Sheet doc and upload the
contents of the CSV files to sheets named 'apples' and 'oranges':

  upload-to-sheets apples.csv oranges.csv

You can override the name of the sheets created by appending a colon and the
name, like this:

  upload-to-sheets Apples:apples.csv Oranges:oranges.csv

If you'd like to upload the sheets in an existing document, provide it as an
option:

  upload-to-sheets --docid="1xcCjHM...j1ubo0Y09DfGn8HRMLY" apples.csv oranges.csv

Note that if you do this and there are existing sheets with the same names, e.g.
"apples", the contents of these sheets will be replaced by the uploaded
contents. All the other sheets will remain untouched. This is designed so that
you can manually craft a custom spreadsheet and upload only some of it sheets
with contents derived from another program (e.g. Beancount).

This script only requires the latest and official Google client API libraries
(it does not need gdata nor Python wrappers for sheets). It uses the v4 Sheets
API (current as of 2013-2017-12-15). You will need to have an installation of the
following libraries for this to work:

 * google-api-python-client (Google Python client API)
 * oauth2client
 * httplib2

Moreover, you will need to enable the Google Sheets API in the developer console
and download the Client Secrets that Google provides to ~/.google-apis.json. (You
can override this location with the GOOGLE_APIS environment variable.)
"""
__copyright__ = "Copyright (C) 2013-2020  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import csv
import json
import logging
import pickle
import re
import string
import sys
from os import path
from typing import List, Optional

from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport import requests as grequests
from googleapiclient import discovery
from googleapiclient import errors


# The name of a sheet left as the unique sheet temporarily, while creating a new
# spreadsheet.
EMPTY_SHEET_TITLE = '__EMPTY__'


def get_credentials(scopes: List[str],
                    secrets_filename: Optional[str] = None,
                    storage_filename: Optional[str] = None):
    """Authenticate via oauth2 and return credentials."""

    logging.getLogger('googleapiclient.discovery_cache').setLevel(logging.ERROR)

    import __main__  # pylint: disable=import-outside-toplevel
    cache_dir = path.expanduser(path.join("~/.google", path.basename(__main__.__file__)))
    if secrets_filename is None:
        secrets_filename = "{}.json".format(cache_dir)
    if storage_filename is None:
        storage_filename = "{}.cache".format(cache_dir)

    # Load the secrets file, to figure if it's for a service account or an OAUTH
    # secrets file.
    secrets_info = json.load(open(secrets_filename))
    if secrets_info.get("type") == "service_account":
        # Process service account flow.
        # pylint: disable=import-outside-toplevel
        import google.oauth2.service_account as sa
        credentials = sa.Credentials.from_service_account_info(
            secrets_info, scopes=scopes)
    else:
        # Process OAuth flow.
        credentials = None
        if path.exists(storage_filename):
            with open(storage_filename, 'rb') as token:
                credentials = pickle.load(token)
        # If there are no (valid) credentials available, let the user log in.
        if not credentials or not credentials.valid:
            if credentials and credentials.expired and credentials.refresh_token:
                credentials.refresh(grequests.Request())
            else:
                flow = InstalledAppFlow.from_client_secrets_file(
                    secrets_filename, scopes)
                credentials = flow.run_console()
            # Save the credentials for the next run
            with open(storage_filename, 'wb') as token:
                pickle.dump(credentials, token)

    return credentials


def pop_alist(items, key, default=None):
    """Remove the first found element in a multi-association list.

    Args:
      items: An iterable sequence of pairs of items.
      key: A key for those pairs.
    Returns:
      The value associated with the first matching key in the list, or the
      default value if not found.
    """
    for index, item in enumerate(items):
        if item[0] == key:
            del items[index]
            return item[1]
    return default


def get_alpha_column(column):
    """Given a numerical column number, return its equivalent letter code.

                               div / mod 26
    A = 0                  0      0     0
    Z = 25                 0      0    25
    AA = 26                0      1     0
    AZ = 51                0      1    25
    BA = 52                0      2     0
    BZ = 77                0      2    25
    CA = 78                0      3     0
    ZZ = 701               0     25    25
    AAA = 702              1      0     0

    1-26 -> A-Z
    27 -> AA
    52 -> AZ
    53 -> BA

    Args:
      column: An integer, the column number. Note: The first column is "0".
    Returns:
      A string. This can possibly be more than one character, if column > 26.
    """
    letters = []
    while column >= 0:
        letters.append(string.ascii_uppercase[column % 26])
        column = column // 26 - 1
    return ''.join(reversed(letters))


def sheet_range(nrows, ncols, title=None):
    """Build up the full range of a sheet for some size of rows and columns.

    Args:
      title: A string, the name of the sheet in the doc.
      nrows: An integer, the number of rows to resize to.
      ncols: An integer, the number of columns to resize to.
    Returns:
      A string representing the full range of this sheet.
    """
    if title is None:
        return 'A1:{}{}'.format(get_alpha_column(ncols-1), nrows)
    else:
        return '{}!A1:{}{}'.format(title, get_alpha_column(ncols-1), nrows)


def create_doc(service):
    """Create a new spreadsheet with an empty initial sheet.

    The empty initial sheet gets deleted later. We cannot create a new
    spreadsheet without at least a single sheet in it.

    Args:
      service: A Google sheets v4 API Resource object.
    Returns:
      A string, the id of the spreadsheet drive document. Something that
      looks like '1xcCjHM-Tjvkwq3R5NuHCv0dGj1ubo0Y09DfGn8HRMLY'.
    """
    resp = service.spreadsheets().create(body={
        'sheets': {'properties': {'title': EMPTY_SHEET_TITLE}}}).execute()
    return resp['spreadsheetId']


class Doc:
    "A wrapper for a particular document. This just keeps common immutable arguments."

    def __init__(self, service, docid, min_rows):
        self.service = service
        self.docid = docid
        self.min_rows = min_rows

    def delete_empty_sheets(self):
        """Remove empty sheets created only temporarily."""
        requests = [{'deleteSheet': {'sheetId': sheet_id}}
                    for title, sheet_id in self.get_sheets()
                    if title == EMPTY_SHEET_TITLE]
        self.service.spreadsheets().batchUpdate(
            spreadsheetId=self.docid,
            body={'requests': requests}).execute()

    def get_sheets(self):
        """Get the sheet titles and ids of the given spreadsheet.

        Returns:
          A list of (title string, sheet-id integer) pairs, in the order that they appear in
          the document.
        """
        resp = self.service.spreadsheets().get(spreadsheetId=self.docid).execute()
        return [(sheet['properties']['title'], sheet['properties']['sheetId'])
                for sheet in resp['sheets']]

    def add_sheet(self, title):
        """Create a new sheet in an existing doc.

        Args:
          title: A string, the name of the new sheet to create.
        Returns:
          An integer, the sheet-id of the newly created sheet.
        """
        # Note: the 'index' options appears not to work. It always prepends at
        # the beginning of the sheets list.
        requests = [{'addSheet': {'properties': {'title': title}}}]
        resp = self.service.spreadsheets().batchUpdate(
            spreadsheetId=self.docid,
            body={'requests': requests}).execute()
        return resp['replies'][0]['addSheet']['properties']['sheetId']

    def get_sheet_size(self, title):
        """Get the size of a spreadsheet.

        Args:
          title: A string, the name of the sheet in the doc.
        Returns:
          A pair of (num-rows, num-columns) integers, the size of the sheet.
        """
        resp = self.service.spreadsheets().get(
            spreadsheetId=self.docid,
            ranges=title).execute()
        grid_props = resp['sheets'][0]['properties']['gridProperties']
        return (grid_props['rowCount'], grid_props['columnCount'])

    def clear_sheet(self, title, nrowcols=None):
        """Clear the sheet.

        Args:
          title: A string, the name of the sheet in the doc.
          nrowcols: An optional (nrows, ncols) pair of integers. If you don't specify it,
            it will be requested.
        """
        if nrowcols is None:
            nrows, ncols = self.get_sheet_size(title)
        else:
            nrows, ncols = nrowcols
        srange = sheet_range(nrows, ncols, title)
        resp = self.service.spreadsheets().values().clear(
            spreadsheetId=self.docid,
            range=srange,
            body={}).execute()

    def resize_sheet(self, sheet_id, title, nrows, ncols):
        """Update the size of a sheet.

        Args:
          sheet_id: An integer, the id of the sheet in the doc.
          title: A string, the name of the sheet in the doc.
          nrows: An integer, the number of rows to resize to.
          ncols: An integer, the number of columns to resize to.
        """
        requests = [{'updateSheetProperties': {
            'properties': {
                'sheetId': sheet_id,
                'title': title,  # Required, unfortunately.
                'gridProperties': {'rowCount': nrows,
                                   'columnCount': ncols}},
            'fields': 'gridProperties(rowCount, columnCount)'}}]
        self.service.spreadsheets().batchUpdate(
            spreadsheetId=self.docid,
            body={'requests': requests}).execute()

    def auto_resize_sheet(self, sheet_id):
        """Auto-resize the spreadsheet based on the current data in it.

        Args:
          sheet_id: An integer, the id of the sheet in the doc.
        """
        requests = [{'autoResizeDimensions': {
            'dimensions': {'sheetId': sheet_id,
                           'dimension': 'COLUMNS'}}}]
        self.service.spreadsheets().batchUpdate(
            spreadsheetId=self.docid,
            body={'requests': requests}).execute()

    def update_sheet(self, sheet_id, title, filename):
        """Clear and replace the data in a sheet with that of a file.

        Args:
          sheet_id: An integer, the id of the sheet in the doc.
          title: A string, the name of the sheet in the doc.
          filename: A string, the path to the CSV filename to load and upload.
        """
        # Load the CSV file into an array of rows.
        with open(filename, 'r', encoding='utf-8') as csvfile:
            rows = list(csv.reader(csvfile))

        nrows = len(rows)
        ncols = max(len(row) for row in rows) if rows else 0

        nrows = self.min_rows if self.min_rows and nrows < self.min_rows else nrows
        size = (nrows, ncols)

        current_size = self.get_sheet_size(title)
        if size != current_size:
            # Note: Sizing down the sheet also deletes the values from the cells removed
            # automatically.
            self.resize_sheet(sheet_id, title, nrows, ncols)

        # Clear the remaining contents.
        self.clear_sheet(title, (nrows, ncols))

        # Upload the new values.
        #
        # FIXME: Using an "updateCells" request in order to be able to set not only
        # the values would be an improvement and allow for much more control over
        # the formatting of numbers. It would be better not to use USER_ENTERED.
        srange = sheet_range(nrows, ncols, title)
        resp = self.service.spreadsheets().values().update(
            spreadsheetId=self.docid,
            range=srange,
            valueInputOption='USER_ENTERED',
            body={'range': srange,
                  'values': rows}).execute()

        # Make sure the newly created sheet looks good by default, by sizing the
        # columns to fit the newly updated data.
        self.auto_resize_sheet(sheet_id)

    def set_title(self, doctitle):
        """Set the document's title (not the sheet title, the document's).

        Args:
          doctitle: A string, the name of the document.
        """
        requests = [{'updateSpreadsheetProperties': {
            'properties': {'title': doctitle},
            'fields': 'title'}}]
        self.service.spreadsheets().batchUpdate(
            spreadsheetId=self.docid,
            body={'requests': requests}).execute()


def _main():
    parser = argparse.ArgumentParser(description=__doc__.strip(),
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('filenames', nargs='*', action='store',
                        help=("CSV filenames[:name] to upload. "
                              "If 'name' is not provided, infer from the filename."))

    parser.add_argument('--docid', '--doc', '--id', '-d', dest='docid', action='store',
                        help="Spreadsheets doc id to update")

    parser.add_argument('--title', '-t', action='store', dest='doctitle',
                        help="Set or update the spreadsheet's title")

    parser.add_argument('--verbose', '-v', action='store_true',
                        help="Print out the log")

    parser.add_argument('--min-rows', action='store', type=int, default=0,
                        help=("Minimum number rows to resize uploaded sheet to. "
                              "This is useful when another sheet feeds from the uploaded "
                              "one, which otherwise automatically renumbers its "
                              "references to rows beyond it if they existed, to avoid "
                              "most such resizing woes."))

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO if args.verbose else logging.WARNING,
                        format='%(levelname)-8s: %(message)s')

    # Parse out the doc id, in case the user provided a full URL.
    if args.docid:
        match = re.match('https://docs.google.com/spreadsheets/d/([^/]+)(/|$)', args.docid)
        if match:
            args.docid = match.group(1)

    # Discover the service.
    creds = get_credentials('https://www.googleapis.com/auth/spreadsheets')
    url = 'https://sheets.googleapis.com/$discovery/rest?version=v4'
    service = discovery.build('sheets', 'v4', credentials=creds)

    # Figure out what the name mappings should be, from the filenames (or
    # explicitly).
    new_sheets = []
    for filename in args.filenames:
        match = re.match('(.*):(.*)$', filename)
        if match:
            sheet_name, filename = (match.group(1), match.group(2))
            # Support inverted sheets name labels.
            if not path.exists(filename) and path.exists(sheet_name):
                sheet_name, filename = filename, sheet_name
        else:
            sheet_name = path.splitext(path.basename(filename))[0]
        new_sheets.append((sheet_name, filename))
    logging.info("Sheets to create or update: %s", new_sheets)

    # Get or create the spreadsheet.
    if args.docid:
        created = False
        docid = args.docid
    else:
        created = True
        docid = create_doc(service)
        logging.info("Created doc: https://docs.google.com/spreadsheets/d/%s/", docid)

    match_names_and_upload_sheets(service, docid, new_sheets, args.min_rows)

    # Clean up temporary sheets created for new documents only.
    doc = Doc(service, docid, args.min_rows)
    if created:
        doc.delete_empty_sheets()

    # Set the document title (if requested).
    if args.doctitle:
        doc.set_title(args.doctitle)

    print("https://docs.google.com/spreadsheets/d/{}/".format(docid))


def match_names_and_upload_sheets(service, docid, new_sheets, min_rows):
    """Match sheet names and upload their attendant file contents."""

    doc = Doc(service, docid, min_rows)

    # Get the existing sheets within (this also validates the existence of the
    # document).
    existing_sheets = doc.get_sheets()
    logging.info("Existing sheets: %s", existing_sheets)

    # Create new or match against existing sheets if necessary. This essentially
    # pairs up spreadsheets from the input to sheet-ids in the doc.
    sheets_alist = []
    for title, filename in new_sheets:
        sheet_id = pop_alist(existing_sheets, title)
        if sheet_id is None:
            logging.info("Creating sheet '%s'", title)
            sheet_id = doc.add_sheet(title)
        else:
            logging.info("Reusing sheet '%s' with id '%s'", title, sheet_id)
        sheets_alist.append((sheet_id, title, filename))

    # Clear and replace the data in the given sheet with that of the given
    # filename.
    for sheet_id, title, filename in sheets_alist:
        doc.update_sheet(sheet_id, title, filename)


def main():
    try:
        _main()
        sys.exit(0)
    except errors.Error as exc:
        logging.fatal(str(exc))
        sys.exit(1)


if __name__ == '__main__':
    main()
