#!/usr/bin/env python3
"""Download a document in various formats and compare its native Pandoc conversions.

The purpose is figure out which of the formats downloadable from Google Docs
offers the richest representation to post-process and automatically convert to
Markdown format for inclusion in the repo. Ultimately, we'd like to convert all
the Google Docs to Markdown or some other text format the open source community
finds most friendly.
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
from os import path

from apiclient import discovery
import httplib2
from oauth2client import service_account


def download_doc(files: object, docid: str,
                 extension: str, mime_type: str,
                 output_dir: str) -> str:
    metadata = files.get(fileId=docid).execute()
    name = metadata['name']
    clean_name = re.sub('_-_', '-',
                        re.sub('_+', '_',
                               re.sub('[^A-Za-z0-9=-]', '_', name)))
    filename = path.join(output_dir, '{}.{}'.format(clean_name, extension))
    if path.exists(filename) and path.getsize(filename):
        logging.warning("File already present: %s; skipping", filename)
        return filename
    with open(filename, 'wb') as outfile:
        exported = files.export(fileId=docid,
                                mimeType=mime_type).execute()
        outfile.write(exported)
    return filename


FORMATS = [
    ('html', 'text/html', 'html'),
    ('txt', 'text/plain', None),
    ('rtf', 'application/rtf', None),
    ('odt', 'application/vnd.oasis.opendocument.text', 'odt'),
    ('pdf', 'application/pdf', None),
    ('docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document', 'docx'),
    ('epub', 'application/epub+zip', 'epub'),
    ]


SERVICE_ACCOUNT_FILE = path.join(os.environ['HOME'],
                                 '.google-apis-service-account.json')

def get_auth_via_service_account(scopes):
    """Get an authenticated http object via a service account.

    Args:
      scopes: A string or a list of strings, the scopes to get credentials for.
    Returns:
      A pair or (credentials, http) objects, where 'http' is an authenticated
      http client object, from which you can use the Google APIs.
    """
    credentials = service_account.ServiceAccountCredentials.from_json_keyfile_name(
        SERVICE_ACCOUNT_FILE, scopes)
    http = httplib2.Http()
    credentials.authorize(http)
    return credentials, http


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('docid', action='store',
                        default=None,
                        help="The document id of the doc to download")

    parser.add_argument('output', action='store',
                        default=None,
                        help="Where to write out the output files")

    args = parser.parse_args()

    # Connect, with authentication.
    def get_service():
        scopes = ['https://www.googleapis.com/auth/drive']
        _, http = get_auth_via_service_account(scopes)
        service = discovery.build('drive', 'v3', http=http)
        return service.files()
    files = get_service()

    # Download the docs.
    os.makedirs(args.output, exist_ok=True)
    for extension, mime_type, pandoc_format in FORMATS:
        logging.info("Downloading to %s (%s)", extension, mime_type)
        filename = download_doc(files, args.docid, extension, mime_type, args.output)
        logging.info("File: %s", filename)

        if pandoc_format:
            native_filename = path.join(args.output, "native-from-{}".format(extension))
            logging.info("Calling pandoc for %s", native_filename)
            subprocess.check_call([
                'pandoc', '--from={}'.format(pandoc_format), '--to=native',
                filename, '--output={}'.format(native_filename)])

    logging.info("Done.")


if __name__ == '__main__':
    main()
