#!/usr/bin/python
"""Download all the Beancount docs from Google Drive and bake a nice PDF with it.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import datetime
import logging
import os
import shutil
import tempfile
import subprocess
import re
from urllib import request
from os import path

from apiclient import discovery

from beancount.docs import gauth


def find_index_document(service):
    """Find the the document of Beancount index.

    Args:
      service: An API client object with Google Drive scope.
    Returns:
      A string, the document id.
    """
    query = "title = 'Beancount - Index'"
    listing = service.files().list(q=query).execute()
    for item in listing['items']:
        return item['id']


def enumerate_linked_documents(service, indexid):
    """Given a document id, enumerate the links within it.

    Args:
      service: An API client object with Google Drive scope.
      indexid: A string, a document id.
    Returns:
      A list of link strins.
    """
    doc = service.files().get(fileId=indexid).execute()
    link = doc['exportLinks']['text/html']
    contents = request.urlopen(link).read().decode('utf8')
    docids = [indexid]
    for match in re.finditer('https?://docs.google.com/document/d/([^/]+)/', contents):
        docid = match.group(1)
        if docid not in docids:
            docids.append(docid)
    return docids


def download_docs(service, docids, tempdir):
    """Download all the Beancount documents to a temporary directory.

    Args:
      service: A googleapiclient Service stub.
      docids: A list of string, the document ids to download.
      tempdir: A string, the name of the directory where to store the PDFs.
    Returns:
      A list of string, the names of the downloaded PDF files.
    """
    filenames = []
    mime_type = 'application/pdf'
    for index, docid in enumerate(docids, 1):
        # Get the document metadata.
        metadata = service.files().get(fileId=docid).execute()
        title = metadata['title']
        url = metadata['exportLinks'][mime_type]

        # Retrieve to a file.
        clean_title = re.sub('_-_', '-',
                             re.sub('_+', '_',
                                    re.sub('[^A-Za-z0-9=-]', '_', title)))
        filename = path.join(tempdir, '{}.pdf'.format(clean_title))
        logging.info('Downloading "{}" ({}) to {}'.format(title, docid, filename))
        if not path.exists(filename):
            request.urlretrieve(url, filename)

        # Check if the downloaded succeeded.
        contents = open(filename, 'rb').read(1024).decode('utf-8', 'ignore')
        is_error = re.search('<html', contents)
        if is_error:
            logging.error("Invalid download, skipping file for '{}'.".format(docid))
            continue
        filenames.append(filename)

    return filenames


def collate_filenames(filenames, output_filename):
    """Put the list of PDF filenames together into a single file.

    Args:
      filenames: A list of filename strings.
      output_filename: A string, the name of the output file.
    Raises:
      IOError: If we could not produce the merged filename.
    """
    command = ['pdftk'] + filenames + ['cat', 'output', output_filename]
    try:
        pipe = subprocess.Popen(command, shell=False)
        pipe.communicate()
    except FileNotFoundError as exc:
        raise SystemExit('pdftk is probably not installed: {}'.format(exc))
    if pipe.returncode != 0:
        raise IOError("Could not produce output '{}'".format(output_filename))



def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = gauth.get_argparser(description=__doc__.strip())

    default_path = path.abspath(datetime.date.today().strftime('beancount.%Y-%m-%d.pdf'))
    parser.add_argument('-o', '--output', action='store',
                        default=default_path,
                        help="Where to write out the collated PDF file")

    parser.add_argument('-x', '--dont-delete', action='store_true',
                        help="Don't delete the temporary files. Use this for debugging.")

    args = parser.parse_args()

    # Connect, with authentication.
    scopes = ['https://www.googleapis.com/auth/drive']
    http = gauth.get_authenticated_http(scopes, args)

    # Access the drive API.
    service = discovery.build('drive', 'v2', http=http)

    indexid = find_index_document(service)
    docids = enumerate_linked_documents(service, indexid)

    try:
        # Allocate a temporary directory.
        tempdir = path.join(tempfile.gettempdir(), 'beancount-docs')
        os.makedirs(tempdir, exist_ok=True)

        # Download the docs.
        filenames = download_docs(service, docids, tempdir)

        # Collate the files together.
        collate_filenames(filenames, args.output)
    finally:
        # Cleanup.
        if not args.dont_delete:
            shutil.rmtree(tempdir)

    logging.info("Output produced in {}".format(args.output))


if __name__ == '__main__':
    main()
