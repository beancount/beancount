"""Update the parts of the documentation that are auto-generated.

For example, the options documentation is a Google Doc. It can be generated from
the source code and updated automatically using this script.
"""

__copyright__ = "Copyright (C) 2015-2017, 2020-2021, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import os
import re
from os import path

import httplib2
from apiclient import discovery
from apiclient.http import MediaInMemoryUpload

# TODO(blais, 2023-11-18): oauth2client is deprecated.
from oauth2client import service_account

from beancount.parser import options
from beancount.utils import test_utils


def replace_gdocs_document(http, docid, title, contents):
    """Upload new contents for a Google Doc for a plain/text file.

    Args:
      http: An http connection object with drive credentials.
      docid: A string, the ID of the document.
      title: A string, the title of the document.
      contents: A string, the body of the document.
    """
    service = discovery.build("drive", "v3", http=http)
    media = MediaInMemoryUpload(
        contents.encode("utf8"), mimetype="text/plain", resumable=True
    )
    return (
        service.files()
        .update(fileId=docid, body={"name": title}, media_body=media)
        .execute()
    )


def get_options_docid():
    """Find the options doc id from the redirect file.

    Returns:
      The id of the doc to fix up.
    """
    htaccess = path.join(test_utils.find_repository_root(__file__), ".nginx.conf")
    with open(htaccess) as inht:
        lines = list(
            filter(
                None,
                map(
                    re.compile(r".*/doc/options.*(https?://docs.google.com/.*);").match,
                    inht.readlines(),
                ),
            )
        )
    assert len(lines) == 1
    return list(filter(None, lines[0].group(1).split("/")))[-1]


SERVICE_ACCOUNT_FILE = os.path.expanduser("~/.google-apis-service-account.json")


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


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())
    _args = parser.parse_args()

    # Find the document id.
    docid = get_options_docid()

    # Connect to the service.
    scopes = [
        "https://www.googleapis.com/auth/drive",
        "https://www.googleapis.com/auth/drive.scripts",
    ]
    _, http = get_auth_via_service_account(scopes)

    # Replace the document.
    replace_gdocs_document(
        http, docid, "Beancount - Options Reference", options.list_options()
    )


if __name__ == "__main__":
    main()
