#!/usr/bin/env python3
"""Utility functions to download and convert Google Docs."""

__copyright__ = "Copyright (C) 2014-2020, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"

import hashlib
import logging
import os
import pickle
import re
import shelve
import subprocess
from os import path

import httplib2

# TODO(blais, 2023-11-18): oauth2client is deprecated.
from oauth2client import service_account


class Cache:
    """A cache for a service method for the Google Client API, like
    "service.files()". This is useful when working remotely, to avoid
    downloading the same document multiple times.
    """

    def __init__(self, filename, delegate_factory):
        self._filename = filename
        self._delegate = None
        self._delegate_factory = delegate_factory
        self._shelve = shelve.open(self._filename)

    @property
    def delegate(self):
        if self._delegate is None:
            self._delegate = self._delegate_factory()
        return self._delegate

    def __getattr__(self, name):
        return Cache.Method(self, name)

    class Method:
        def __init__(self, cache, name):
            self._cache = cache
            self._name = name

        def __call__(self, *args, **kwargs):
            key = (self._name, args, sorted(kwargs.items()))
            pickled_key = pickle.dumps(key)
            md5 = hashlib.md5()
            md5.update(pickled_key)
            digest = md5.hexdigest()
            try:
                value = self._cache._shelve[digest]
            except KeyError:
                logging.info("Cache miss for %s", digest)
                function = getattr(self._cache.delegate, self._name)
                value = function(*args, **kwargs).execute()
                self._cache._shelve[digest] = value
            return Cache.ExecuteWrapper(value)

    class ExecuteWrapper:
        def __init__(self, return_value):
            self._return_value = return_value

        def execute(self):
            return self._return_value


def find_index_document(files):
    """Find the document of Beancount index.

    Args:
      files: A Cached API client object with Google Drive scope.
    Returns:
      A string, the document id.
    """
    query = "name = 'Beancount - Index'"
    listing = files.list(q=query).execute()
    files = listing["files"]
    if len(files) != 1:
        raise ValueError(
            "Could not find the index file: {} files matched".format(len(files))
        )
    for file in files:
        return file["id"]


def enumerate_linked_documents(files, indexid):
    """Given a document id, enumerate the links within it.

    Args:
      files: A Cached API client object with Google Drive scope.
      indexid: A string, a document id.
    Returns:
      A list of link strings.
    """
    doc = files.export(fileId=indexid, mimeType="text/html").execute()
    contents = doc.decode("utf8")
    docids = [indexid]
    for match in re.finditer('https?://docs.google.com/document/d/([^/";&]+)', contents):
        docid = match.group(1)
        if docid not in docids:
            docids.append(docid)
    return docids


def download_docs(files, docids, outdir, extension):
    """Download all the Beancount documents to a temporary directory.

    Args:
      files: A Cached API client object with Google Drive scope.
      docids: A list of string, the document ids to download.
      outdir: A string, the name of the directory where to store the files.
      extension: A string, the extension of the requested documents.
    Returns:
      A list of string, the names of the downloaded files.
    """
    mime_type, _ = CONVERSION_MAP[extension]
    filenames = []
    for index, docid in enumerate(docids, 1):
        # Get the document metadata.
        metadata = files.get(fileId=docid).execute()
        name = metadata["name"]

        # Retrieve to a file.
        clean_name = re.sub(
            "_-_", "-", re.sub("_+", "_", re.sub("[^A-Za-z0-9=-]", "_", name))
        )
        filename = path.join(outdir, "{}.{}".format(clean_name, extension))
        logging.info('Exporting "{}" ({}) to {}'.format(name, docid, filename))
        with open(filename, "wb") as outfile:
            exported = files.export(fileId=docid, mimeType=mime_type).execute()
            outfile.write(exported)

        # Check if the downloaded succeeded.
        if path.getsize(filename) == 0:
            logging.error("Invalid download, skipping file for '{}'.".format(docid))
            continue
        filenames.append(filename)

    return filenames


def convert_pdf(filenames, output):
    """Process downloaded PDF files.

    Args:
      filenames: A list of filename strings.
      output_filename: A string, the name of the output file.
    """
    collate_pdf_filenames(filenames, output)


def collate_pdf_filenames(filenames, output_filename):
    """Combine the list of PDF filenames together into a single file.

    Args:
      filenames: A list of filename strings.
      output_filename: A string, the name of the output file.
    Raises:
      IOError: If we could not produce the merged filename.
    """
    command = ["pdftk"] + filenames + ["cat", "output", output_filename]
    try:
        pipe = subprocess.Popen(command, shell=False)
        pipe.communicate()
    except (FileNotFoundError, PermissionError) as exc:
        raise SystemExit("pdftk is probably not installed: {}".format(exc))
    if pipe.returncode != 0:
        raise IOError("Could not produce output '{}'".format(output_filename))


SERVICE_ACCOUNT_FILE = path.join(os.environ["HOME"], ".google-apis-service-account.json")


def get_auth_via_service_account(scopes):
    """Get an authenticated http object via a service account.

    Args:
      scopes: A string or a list of strings, the scopes to get credentials for.
    Returns:
      A pair or (credentials, http) objects, where 'http' is an authenticated
      http client object, from which you can use the Google APIs.
    """
    credentials = service_account.ServiceAccountCredentials.from_json_keyfile_name(
        SERVICE_ACCOUNT_FILE, scopes
    )
    http = httplib2.Http()
    credentials.authorize(http)
    return credentials, http


CONVERSION_MAP = {
    "html": ("text/html", None),
    "txt": ("text/plain", None),
    "rtf": ("application/rtf", None),
    "pdf": ("application/pdf", convert_pdf),
    "odt": ("application/vnd.oasis.opendocument.text", None),
    "docx": (
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        None,
    ),
}
