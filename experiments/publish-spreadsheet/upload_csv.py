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
import collections
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
import gdata.gauth
import gdata

import gauth2 as gauth


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = gauth.get_argparser(description=__doc__.strip())
    parser.add_argument('filename', action='store', help="CSV file to upload")
    parser.add_argument('docid', action='store', help="Spreadsheets doc id to update")
    args = parser.parse_args()

    # Connect, with authentication.
    scopes = ['https://spreadsheets.google.com/feeds']
    http, credentials = gauth.get_authenticated_http(scopes, args)

    client = gdata.spreadsheets.client.SpreadsheetsClient()
    client.auth_token = gdata.gauth.OAuth2TokenFromCredentials(credentials)

    feed = client.get_spreadsheets()
    print type(feed.entry)
    # print feed.entry[0].id
    # for entry in feed.entry:
    #     print entry.title
    ## s = client.get_worksheets()

    # print type(s)
    # print s.find_worksheets_feed()

    # spreadsheet_id = s.entry[0].id.text.rsplit('/',1)[1]
    # print spreadsheet_id
    #entry = client.GetSpreadsheets(args.docid)





if __name__ == '__main__':
    main()
