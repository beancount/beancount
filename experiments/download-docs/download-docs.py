#!/usr/bin/python
"""Download all the Beancount docs from Google Drive and bake a nice PDF with it.
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

# Import oauth2 libraries.
import oauth2client.client
from oauth2client import tools
from oauth2client.file import Storage

# Import Google API client libraries.
import httplib2
try:
    from apiclient import discovery
except:
    # The name for the older version differs slightly.
    from googleapiclient import discovery


DEFAULT_SECRETS_FILENAME = os.environ.get('GOOGLE_APIS', None)
DEFAULT_STORAGE_FILENAME = path.join(os.environ['HOME'], '.oauth2-google-api')


def get_authenticated_http(scope, secrets_filename, storage_filename, opts):
    """Authenticate via oauth2 and cache credentials to a file.

    If the credentials are already available in the 'storage' cache file, this
    function will not require user interaction, it will simply return the cached
    credentials; otherwise, it opens up a browser window for the user to accept
    the access and obtain the credentials.

    Args:
      scope: A string, the scope to get credentials for.
      secrets_filename: A string, the filename that contains information
        identifying the client application and secret (Note: this is not the
        credentials/token).
      storage_filename: A string, a path to the filename where to cache the
        credentials between runs.
      opts: An argparse option values object, as retrurned by parse_args().
    Returns:
      An authenticated http client object, from which you can use the Google
      APIs.
    """
    # Create a flow from a secrets file.
    flow = oauth2client.client.flow_from_clientsecrets(secrets_filename, scope)
    flow.redirect_uri = oauth2client.client.OOB_CALLBACK_URN

    # Create a transport, disable SSL certificates, which fails to validate.
    http = httplib2.Http()
    http.disable_ssl_certificate_validation = True

    # Create a storage to cache the credentials for future runs, and look it up.
    storage = Storage(storage_filename)
    credentials = storage.get()
    if credentials is None:
        # Save and restore the logger level, because the flow somehow overrides it.
        saved_log_level = logging.getLogger().level
        try:
            # If the credentials haven't been found, run the flow. This will pop-up
            # a web browser window for you to accept.
            credentials = tools.run_flow(flow, storage, opts, http=http)
        finally:
            logging.getLogger().setLevel(saved_log_level)

    # Authorize using the transport and return it.
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

    parser.add_argument('--secrets', action='store',
                        default=DEFAULT_SECRETS_FILENAME,
                        help="Secrets filename")

    parser.add_argument('--storage', action='store',
                        default=DEFAULT_STORAGE_FILENAME,
                        help="Storage filename")

    parser.add_argument('-o', '--output', action='store',
                        default=datetime.date.today().strftime('beancount.%Y-%m-%d.pdf'),
                        help="Where to write out the collated PDF file")

    opts = parser.parse_args()

    # Connect, with authentication.
    # Check https://developers.google.com/drive/scopes for all available scopes.
    DRIVE_SCOPE = 'https://www.googleapis.com/auth/drive'
    http = get_authenticated_http(DRIVE_SCOPE, opts.secrets, opts.storage, opts)

    # Access the drive API.
    drive = discovery.build('drive', 'v2', http=http)

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
