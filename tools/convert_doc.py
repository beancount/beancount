#!/usr/bin/env python3
"""Download and convert a single Google Docs to a nice Markdown doc.

This downloads one or more exports of a Google Docs document and converts it
through Pandoc and possibly other tools in order to produce the most faithful
conversion to Markdown. Unfortunately, Google's Markdown export is insufficient.
"""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime
import logging
import os
import shutil
import tempfile
import subprocess
import re
import pickle
import hashlib
import shelve
import tempfile
import io
from os import path

from apiclient import discovery
import httplib2
from oauth2client import service_account

# Local imports
import docs


def pandoc(filename, informat):
    cwd = path.dirname(path.abspath(__file__))
    command = [
        'pandoc', '-f', informat, '-t', 'markdown',
        '--filter', path.join(cwd, 'convert_filter_docx.py'),
        filename]
    print(' '.join(command))
    return subprocess.check_output(command).decode('utf8')


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('docid', action='store',
                        default=None,
                        help="The Google doc id of the input document")

    parser.add_argument('output', action='store',
                        default=None,
                        help="Where to write out the output files")

    parser.add_argument('--cache', action='store',
                        help="Service cache, to work offline.")

    args = parser.parse_args()

    # Connect, with authentication.
    def get_service():
        scopes = ['https://www.googleapis.com/auth/drive']
        _, http = docs.get_auth_via_service_account(scopes)
        service = discovery.build('drive', 'v3', http=http)
        return service.files()
    files = (docs.Cache(args.cache, get_service)
             if args.cache
             else get_service())

    # Allocate a temporary directory for the output.
    os.makedirs(args.output, exist_ok=True)
    tmpdir = path.join(tempfile.gettempdir(), 'convert_doc')
    os.makedirs(tmpdir, exist_ok=True)

    # Download the docs.
    filenames = docs.download_docs(files, {args.docid}, tmpdir, 'docx')
    #native = pandoc(filenames[0], 'docx')
    filenames = docs.download_docs(files, {args.docid}, tmpdir, 'odt')
    filenames = docs.download_docs(files, {args.docid}, tmpdir, 'txt')
    filenames = docs.download_docs(files, {args.docid}, tmpdir, 'pdf')
    filenames = docs.download_docs(files, {args.docid}, tmpdir, 'html')
    filenames = docs.download_docs(files, {args.docid}, tmpdir, 'rtf')

    ##logging.info("Output produced in {}".format(args.output))


if __name__ == '__main__':
    main()
