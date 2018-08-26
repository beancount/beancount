#!/usr/bin/env python3
"""Extract and convert tagged transactions from a Google Spreadsheet.

This script reads a Beancount file, extracts all transactions with a tag, and
rewrites them to convert its inflows to a single account.
"""

from os import path
from typing import Dict, List, Tuple, Generator, Optional, Any
import argparse
import collections
import datetime
import logging
import os
import pprint
import re
import sys
import typing

from oauth2client import client
from oauth2client import tools
from oauth2client import file
import httplib2
from apiclient import discovery
from apiclient import errors

from dateutil import parser

from beancount import loader
from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import amount
from beancount.core import account
from beancount.core import flags
from beancount.parser import printer
from beancount.tools import sheets_upload
from beancount.ingest.importers import csv


Sheet = typing.NamedTuple("Sheet", [
    ("docid", str),
    ("id", int),
    ("name", str)])



def get_credentials(scopes, args) -> Tuple[client.OAuth2Credentials, httplib2.Http]:
    """Authenticate via oauth2 and cache credentials to a file (or refresh them).

    Args:
      scopes: A string or a list of strings, the scopes to get credentials for.
      args: An argparse option values object.
    Returns:
      An authenticated http client object.
    """
    # Silence annoying error about file_cache version.
    # See, for example, this: https://github.com/google/google-api-python-client/issues/299
    logging.getLogger('googleapiclient.discovery_cache').setLevel(logging.ERROR)

    # Location of your secrets file.
    secrets_filename = os.environ.get('GOOGLE_APIS',
                                      path.expanduser('~/.google-apis.json'))
    # Location to store credentials for reuse between invocations.
    storage_filename = os.environ.get('GOOGLE_STORAGE',
                                      path.expanduser('~/.google-storage.json'))

    flow = client.flow_from_clientsecrets(secrets_filename, scope=scopes)
    storage = file.Storage(storage_filename)
    credentials = storage.get()
    if not credentials or credentials.invalid:
        credentials = tools.run_flow(flow, storage, args)
        storage.put(credentials)
    http = httplib2.Http()
    credentials.authorize(http)
    if credentials.access_token_expired:
        credentials.refresh(http)
    return credentials, http


def get_sheets(service: discovery.Resource, docid: str) -> List[Sheet]:
    "Get the sheet titles and ids of the given spreadsheet."
    resp = service.spreadsheets().get(spreadsheetId=docid).execute()
    return [Sheet(docid, sheet['properties']['sheetId'], sheet['properties']['title'])
            for sheet in resp['sheets']]


def select_sheet(service: discovery.Resource, docid: str, sheet_re: str) -> Sheet:
    "Select a particular sheet from a list."
    sheets = get_sheets(service, docid)
    if len(sheets) > 1 and sheet_re:
        regexp = '.*{}.*'.format(re.escape(sheet_re))
        sheets = [sheet
                  for sheet in sheets
                  if (re.match(regexp, sheet.name, re.I) or
                      re.match(regexp, str(sheet.id), re.I))]
    if len(sheets) != 1:
        raise SystemExit("Could not match a single unambiguous sheet: {}".format(sheets))
    return sheets[0]


def get_sheet_size(service: discovery.Resource, sheet: Sheet):
    "Get the size of a spreadsheet."
    resp = service.spreadsheets().get(
        spreadsheetId=sheet.docid,
        ranges=sheet.name).execute()
    grid_props = resp['sheets'][0]['properties']['gridProperties']
    return (grid_props['rowCount'], grid_props['columnCount'])


def iter_sheet(service: discovery.Resource, sheet: Sheet) -> Generator[List[str],
                                                                       None ,None]:
    "Iterate over the contents of a particular sheet."
    size = get_sheet_size(service, sheet)
    resp = service.spreadsheets().values().batchGet(
        spreadsheetId=sheet.docid,
        ranges=sheets_upload.sheet_range(size[0], size[1], sheet.name)).execute()
    for row in resp['valueRanges'][0]['values']:
        yield row


def parse_date(string: str) -> Optional[datetime.date]:
    "Parse a date string, if possible."
    try:
        return parser.parse(string).date()
    except ValueError:
        pass


def list_get(elemlist, index, default=None):
    try:
        return elemlist[index]
    except IndexError:
        return default


def infer_columns(rows: List[str], num_rows: int=16) -> Dict[int,int]:
    "Infer the column types of a spreadsheet."
    header = rows[0]
    head = rows[1:num_rows]
    coltypes = []
    coltypes_inferred = []
    for colindex in range(len(header)):
        colheader = header[colindex]
        coltype = None
        if re.match(r'.*\bdate\b', colheader, re.I):
            coltype = csv.Col.DATE
        elif re.match(r'.*\b(payee|provider)\b', colheader, re.I):
            coltype = csv.Col.PAYEE
        elif re.match(r'.*\b(narration|description)\b', colheader, re.I):
            coltype = csv.Col.NARRATION
        elif re.match(r'.*\b(account|category)\b', colheader, re.I):
            coltype = csv.Col.ACCOUNT
        elif re.match(r'.*\bamount\b', colheader, re.I):
            coltype = csv.Col.AMOUNT
        coltypes.append(coltype)

        coltype = None
        column = [list_get(row, colindex) for row in head]
        if coltype is None:
            if all(isinstance(value, datetime.date)
                   for value in column):
                coltype = csv.Col.DATE
            elif all(re.match(r'\$?[0-9,]+(\.[0-9]+)', value) is not None
                     for value in column
                     if value is not None):
                coltype = csv.Col.AMOUNT
            elif all(re.match(account.ACCOUNT_RE, value) is not None
                     for value in column
                     if value is not None):
                coltype = csv.Col.ACCOUNT
        coltypes_inferred.append(coltype)


    coltypemap = {}
    for coltype in [csv.Col.DATE, csv.Col.PAYEE, csv.Col.NARRATION,
                    csv.Col.ACCOUNT, csv.Col.AMOUNT]:
        try:
            coltypemap[coltype] = coltypes.index(coltype)
        except IndexError:
            try:
                coltypemap[coltype] = coltypes_inferred.index(coltype)
            except IndexError:
                pass

    return coltypemap


def create_entry(row: List[str], coltypes: Dict[int,int], inflows_account: str,
                 currency: str, docid: str, index: int,
                 default_account: str='Expenses:Uncategorized'):
    "Create a new entry based on the row contents and inferred types."
    date = parse_date(list_get(row, coltypes[csv.Col.DATE], ''))
    number_orig_str = list_get(row, coltypes[csv.Col.AMOUNT], '')
    number_str = number_orig_str.strip('$ ')
    if not date:
        logging.error("Invalid row: {r}", row)
        return
    try:
        number = (D(number_str)
                  if number_str and number_str != '?'
                  else ZERO)
    except ValueError as exc:
        raise ValueError("{} (for {})".format(exc, number_str))
    if not number_str:
        logging.warning("Zero amount: %s", row)

    account = default_account
    if coltypes.get(csv.Col.ACCOUNT):
        account = row[coltypes.get(csv.Col.ACCOUNT)] or account

    payee = None
    if coltypes.get(csv.Col.PAYEE):
        payee = row[coltypes.get(csv.Col.PAYEE)]
    narration = None
    if coltypes.get(csv.Col.NARRATION):
        narration = row[coltypes.get(csv.Col.NARRATION)]

    postings = []
    txn = data.Transaction(data.new_metadata(docid, index),
                           date,
                           flags.FLAG_OKAY,
                           payee,
                           narration,
                           data.EMPTY_SET,
                           data.EMPTY_SET,
                           postings)
    units = amount.Amount(number, 'USD')
    postings.append(data.Posting(account, units, None, None, None, None))
    postings.append(data.Posting(inflows_account, -units, None, None, None, None))
    return txn


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('docid', action='store',
                        help="Google Sheets document id")

    parser.add_argument('inflows_account', action='store',
                        help="Inflows account to use for the rewritten transactions")

    parser.add_argument('--sheet', dest='sheet_re', action='store',
                        help="Google Sheets sheets name regexp")

    parser.add_argument('--uncategorized_account', action='store',
                        default='Expenses:Uncategorized',
                        help="Name of uncategorized account if missing")

    parser.add_argument('-t', '--tag', action='store',
                        help="Tag all entries with the given tag string")

    args = parser.parse_args()

    _, http = get_credentials('https://www.googleapis.com/auth/spreadsheets', args)
    url = 'https://sheets.googleapis.com/$discovery/rest?version=v4'
    service = discovery.build('sheets', 'v4', http=http, discoveryServiceUrl=url)

    # Select the sheet.
    sheet = select_sheet(service, args.docid, args.sheet_re)
    logging.info("Selected sheet: {.name}".format(sheet))

    # Read contents and categorize them.
    rows = list(iter_sheet(service, sheet))
    coltypes = infer_columns(rows)

    rowiter = iter(rows)
    next(rowiter)
    entries = []
    for index, row in enumerate(rowiter):
        if not list(filter(None, row)):
            continue
        entry = create_entry(row, coltypes, args.inflows_account, 'USD', args.docid, index,
                             args.uncategorized_account)
        if entry is not None:
            entries.append(entry)

    if args.tag:
        print('pushtag #{}'.format(args.tag))
        print()
    printer.print_entries(entries, file=sys.stdout)
    if args.tag:
        print()
        print('poptag #{}'.format(args.tag))


if __name__ == '__main__':
    main()
