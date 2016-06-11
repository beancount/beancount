#!/usr/bin/env python3
"""Kicking the tires on gspread: https://github.com/burnash/gspread

'gspread' supports Python3, and thus may be a better candidate for automating
uploads and getting rid of the Google Finance API usage. It's simpler too, and
it's also actively maintained, unlike the Google API which appears to have been
abandoned.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import bisect
import csv
import codecs
import logging
import os
import re
import unittest
import json
from os import path

import oauth2client.client
from oauth2client import tools
from oauth2client.file import Storage
from oauth2client import service_account
import gspread


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser()
    parser.add_argument('docid', action='store',
                        help="Spreadsheets doc id to update")
    args = parser.parse_args()
    docid = args.docid

    # Connect, with authentication.
    scopes = ['https://spreadsheets.google.com/feeds']
    json_filename = path.join(os.environ['HOME'], '.google-apis-service-account.json')
    credentials = service_account.ServiceAccountCredentials.from_json_keyfile_name(
        json_filename, scopes)
    gc = gspread.authorize(credentials)

    # Access some document and print out something from them.
    logging.info('Document id: "%s"', args.docid)
    doc = gc.open_by_key(args.docid)
    print("Title: {}".format(doc))
    for index, sheet in enumerate(doc.worksheets()):
        print("Sheet {}: {}".format(index, sheet.title))


if __name__ == '__main__':
    main()
