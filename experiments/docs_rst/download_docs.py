#!/usr/bin/env python3
"""Download a document in various formats and compare its native Pandoc conversions.

The purpose is figure out which of the formats downloadable from Google Docs
offers the richest representation to post-process and automatically convert to
Markdown format for inclusion in the repo. Ultimately, we'd like to convert all
the Google Docs to Markdown or some other text format the open source community
finds most friendly.
"""

__copyright__ = "Copyright (C) 2017-2019, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"


import argparse
import json
import logging
import os
import re
import subprocess
from os import path

import apiclient.errors
import bs4
import httplib2
from apiclient import discovery

# TODO(blais, 2023-11-18): oauth2client is deprecated.
from oauth2client import service_account


def download_doc(
    files: object, docid: str, extension: str, mime_type: str, output_dir: str
) -> str:
    metadata = files.get(fileId=docid).execute()
    name = metadata["name"]
    clean_name = re.sub("_-_", "-", re.sub("_+", "_", re.sub("[^A-Za-z0-9=-]", "_", name)))
    filename = path.join(output_dir, docid, "{}.{}".format(clean_name, extension))
    os.makedirs(path.dirname(filename), exist_ok=True)
    if path.exists(filename) and path.getsize(filename):
        logging.warning("File already present: %s; skipping", filename)
        return filename
    try:
        with open(filename, "wb") as outfile:
            exported = files.export(fileId=docid, mimeType=mime_type).execute()
            outfile.write(exported)
    except apiclient.errors.HttpError as exc:
        logging.error("Skipping; Error downloading: %s", exc)
        filename = None
    return filename


FORMATS = [
    ("html", "text/html", "html"),
    ("txt", "text/plain", None),
    ("rtf", "application/rtf", None),
    ("odt", "application/vnd.oasis.opendocument.text", "odt"),
    ("pdf", "application/pdf", None),
    (
        "docx",
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "docx",
    ),
    ("epub", "application/epub+zip", "epub"),
]


INDEX_DOCID = "1RaondTJCS_IUPBHFNdT8oqFKJjVJDsfsn6JEjBG04eA"


def get_docids_from_index(files: discovery.Resource):
    """Given a files service, get the doc list of doc ids from the index page."""
    html = files.export(fileId=INDEX_DOCID, mimeType="text/html").execute()
    doc = bs4.BeautifulSoup(html, "lxml")
    for a in doc.find_all("a"):
        href = a["href"]
        # print(href)
        match = re.search(r"document/d/(.*)/", href)
        if not match:
            continue
        # print(match.group(1))
        yield match.group(1)


SERVICE_ACCOUNT_FILE = path.join(os.environ["HOME"], ".google-apis-service-account.json")


def get_auth_via_service_account(scopes: list[str]):
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


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument(
        "--docid",
        action="store",
        default=None,
        help="The document id of the doc to download",
    )

    parser.add_argument(
        "-J",
        "--download-jsons",
        action="store_true",
        help="Download the internal representations in JSON",
    )
    parser.add_argument(
        "-C",
        "--download-conversions",
        action="store_true",
        help="Download the docs converted by Google Docs",
    )

    parser.add_argument(
        "output", action="store", default=None, help="Where to write out the output files"
    )

    args = parser.parse_args()

    # Connect, with authentication.
    def get_service():
        scopes = [
            "https://www.googleapis.com/auth/drive",
            "https://www.googleapis.com/auth/documents.readonly",
        ]
        _, http = get_auth_via_service_account(scopes)
        files = discovery.build("drive", "v3", http=http, cache_discovery=False).files()
        documents = discovery.build(
            "docs", "v1", http=http, cache_discovery=False
        ).documents()
        return files, documents

    files, documents = get_service()

    if args.docid:
        docids = [args.docid]
    else:
        docids = sorted(set(get_docids_from_index(files)))

    # Download the docs.
    os.makedirs(args.output, exist_ok=True)
    if args.download_conversions:
        for docid in docids:
            logging.info("-------------------- Document: %s", docid)
            for extension, mime_type, pandoc_format in FORMATS:
                logging.info("Downloading to %s (%s)", extension, mime_type)
                filename = download_doc(files, docid, extension, mime_type, args.output)
                logging.info("File: %s", filename)

                if pandoc_format:
                    native_filename = filename + ".pandoc"
                    logging.info("Calling pandoc for %s", native_filename)
                    try:
                        subprocess.check_call(
                            [
                                "pandoc",
                                "--from={}".format(pandoc_format),
                                "--to=native",
                                filename,
                                "--output={}".format(native_filename),
                            ]
                        )
                    except subprocess.CalledProcessError as exc:
                        logging.error("Skipping; error in Pandoc conversion: %s", exc)

    # Download the JSON API files.
    if args.download_jsons:
        for docid in docids:
            logging.info("-------------------- Document: %s", docid)
            document = documents.get(documentId=docid).execute()
            title = re.sub(
                "_-_",
                "-",
                re.sub("_+", "_", re.sub("[^A-Za-z0-9=-]", "_", document["title"])),
            )
            filename = path.join(args.output, "{}.json".format(title))
            json.dump(document, open(filename, "w"))

    logging.info("Done.")


if __name__ == "__main__":
    main()
