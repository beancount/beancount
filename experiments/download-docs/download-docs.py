#!/usr/bin/python
"""Download all the Beancount docs from my personal Google Drive and bake a nice PDF with it.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import datetime
import logging
import urllib
import os
import shutil
import tempfile
import subprocess
from os import path
from pprint import pprint as pp

from oauth2client import tools
import oauth2client.client
from oauth2client.client import OAuth2WebServerFlow
from oauth2client.file import Storage

import httplib2
import googleapiclient.discovery


def create_flow_from_file(scope):
    """Create a flow from data stored in a filename.

    Args:
      scope: A string, the scope to get credentials for.
    Returns:
      A flow object.
    """
    secrets_filename = os.environ['GOOGLE_APIS']
    flow = oauth2client.client.flow_from_clientsecrets(secrets_filename, scope)
    flow.redirect_uri = oauth2client.client.OOB_CALLBACK_URN

    # Secrets file contents look like this:
    #
    # {"installed":
    #  {"client_id": ".....",
    #   "client_secret": ".....",
    #   "auth_uri": "https://accounts.google.com/o/oauth2/auth",
    #   "token_uri": "https://accounts.google.com/o/oauth2/token",
    #   "client_email": "",
    #   "redirect_uris": ["urn: ietf: wg: oauth: 2.0: oob", "oob"],
    #   "client_x509_cert_url": "",
    #   "auth_provider_x509_cert_url": "https: //www.googleapis.com/oauth2/v1/certs"}}

    return flow


# Hardcoded secrets baked here for my native app.
CLIENT_ID     = '.....' # Removed.
CLIENT_SECRET = '.....' # Removed.

def create_flow_from_ids(scope):
    """Create a flow from hardcoded IDs.

    Args:
      scope: A string, the scope to get credentials for.
    Returns:
      A flow object.
    """
    secrets_filename = os.environ['GOOGLE_APIS']
    flow = oauth2client.client.flow_from_clientsecrets(secrets_filename, scope)
    flow.redirect_uri = oauth2client.client.OOB_CALLBACK_URN

    # Note: You could also have used 'http://localhost' to redirect.
    return OAuth2WebServerFlow(client_id=CLIENT_ID,
                               client_secret=CLIENT_SECRET,
                               scope=scope,
                               redirect_uri='urn:ietf:wg:oauth:2.0:oob')


def get_authenticated_http(scope, storage_filename, opts):
    """Authenticate via oauth2 and store to a filename.
    If credentials are already available in the storage filename, this does not
    need user interaction; otherwise, this opens up a browser window to accept
    access.

    Args:
      scope: A string, the scope to get credentials for.
      storage_filename: A string, a path to the filename where to cache the
        credentials between runs.
      opts: An argparse option values object, as retrurned by parse_args().
    Returns:
      An authenticated http client object, from which you can use the Google APIs.
    """
    flow = create_flow_from_ids(scope)

    http = httplib2.Http()
    http.disable_ssl_certificate_validation = True

    storage = Storage(storage_filename)
    credentials = storage.get()
    if credentials is None:
        credentials = tools.run_flow(flow, storage, opts, http=http)

    credentials.authorize(http)

    return http


GOOGLE_DOC_IDS = [
    ('Index'           , '1RaondTJCS_IUPBHFNdT8oqFKJjVJDsfsn6JEjBG04eA'),
    ('Motivation'      , '1e4Vz3wZB_8-ZcAwIFde8X5CjzKshE4-OXtVVHm4RQ8s'),
    ('Comparison'      , '1dW2vIjaXVJAf9hr7GlZVe3fJOkM-MtlVjvCO1ZpNLmg'),
    ('Install'         , '1FqyrTPwiHVLyncWTf3v5TcooCu9z5JRX8Nm41lVZi0U'),
    ('Tools'           , '1e44jtLyVRl2H2Pj4K3WUc66otAlTOFOc90-tsrFEQdo'),
    ('Getting-Started' , '1P5At-z1sP8rgwYLHso5sEy3u4rMnIUDDgob9Y_BYuWE'),
    ('Tutorial'        , '1G-gsmwK551lSyuHboVLW3xbLhh99JfoKIbNnZSJxteE'),
    # ('Double-Entry'  , '100tGcA4blh6KSXPRGCZpUlyxaRUwFHEvnz_k9DyZFn4'),
    # ('Inventories'   , '11a9bIoNuxpSOth3fmfuIFzlZtpTJbvw-bPaQCnezQJs'),
    # ('Equity'        , '1H6C4YNPSe8GvC9Pe4Mp84q7ErPwI2AlZrFfKuNq-TG4'),
    # ('Conversions'   , '1rW7zQyMiv8hKZ0993pwQiu5vTxzEOQf_Q67GvVk6zu0'),
    ('Syntax'          , '1wAMVrKIA2qtRGmoVDSUBJGmYZSygUaR0uOMW1GV3YE0'),
    ('Scripting'       , '1QftxNvQPdH-MikMBHupftU6F4IsNZP5FlFh1LCbVgk8'),
    ('Cookbook'        , '1Tss0IEzEyAPuKSGeNsfNgb0BfiW2ZHyP5nCFBW1uWlk'),
    ('Trading'         , '1WjARst_cSxNE-Lq6JnJ5CC41T3WndEsiMw4d46r2694'),
    ('History'         , '17wTH7aKnN_7-6nCxsOad6zIQfHwQgJUdI02RjzQuNi8'),
    # ('Design-Doc'    , '1N7HDXuNWgLG2PqFS4Kkgv5LzAAtU6c97UVNT7tdTIjA'),
    ]

OTHER_PDFS = [
    ('Cheatsheet', 'http://furius.ca/beancount/doc/cheatsheet/beancount-cheatsheet.pdf')
    ]


def download_docs(drive, tempdir):
    """Download all the Beancount documents to a temporary directory.

    Args:
      drive: A googleapiclient Drive stub.
      tempdir: A string, the name of the directory where to store the PDFs.
    Returns:
      A list of string, the names of the downloaded PDF files.
    """
    filenames = []

    # Download Google Docs documents.
    mime_type = 'application/pdf'
    for doc_name, doc_id in GOOGLE_DOC_IDS:

        metadata = drive.files().get(fileId=doc_id).execute()
        url = metadata['exportLinks'][mime_type]

        filename = path.join(tempdir, '{}.pdf'.format(doc_name))

        logging.info("Downloading {} to {}".format(doc_id, filename))
        urllib.urlretrieve(url, filename)
        filenames.append(filename)

    # Download other PDFs.
    for doc_name, url in OTHER_PDFS:
        filename = path.join(tempdir, '{}.pdf'.format(doc_name))
        logging.info("Downloading {} to {}".format(doc_id, filename))
        urllib.urlretrieve(url, filename)
        filenames.append(filename)

    return filenames


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = argparse.ArgumentParser(description=__doc__.strip(),
                                     parents=[tools.argparser])

    parser.add_argument('--storage', action='store',
                        default=path.join(os.environ['HOME'], '.oauth2-for-google-apis'),
                        help="Storage filename")

    parser.add_argument('-o', '--output', action='store',
                        default=datetime.date.today().strftime('beancount.%Y-%m-%d.pdf'),
                        help="Where to write out the collated PDF file")

    opts = parser.parse_args()

    # Connect, with authentication.
    # Check https://developers.google.com/drive/scopes for all available scopes.
    DRIVE_SCOPE = 'https://www.googleapis.com/auth/drive'
    http = get_authenticated_http(DRIVE_SCOPE, opts.storage, opts)

    # Access the drive API.
    drive = googleapiclient.discovery.build('drive', 'v2', http=http)

    try:
        # Download the docs.
        tempdir = tempfile.mkdtemp(prefix='beancount-docs.')
        all_filenames = download_docs(drive, tempdir)

        # Collate them together.
        command = ['pdftk'] + all_filenames + ['cat', 'output', opts.output]
        pipe = subprocess.Popen(command, shell=False)
        pipe.communicate()
        if pipe.returncode != 0:
            raise SystemExit("Could not produce output '{}'".format(opts.output))
    finally:
        shutil.rmtree(tempdir)

    logging.info("Output produced in {}".format(opts.output))


if __name__ == '__main__':
    main()
