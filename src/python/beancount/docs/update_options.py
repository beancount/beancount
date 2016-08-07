"""Update the parts of the documentation that are auto-generated.

For example, the options documentation is a Google Doc. It can be generated from
the source code and updated automatically using this script.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import logging
import re
from os import path

from apiclient import http
from apiclient import discovery
from apiclient.http import MediaInMemoryUpload

from beancount.parser import options
from beancount.utils import test_utils
from beancount.docs import gauth


def replace_gdocs_document(http, docid, title, contents):
    """Upload new contents for a Google Doc for a plain/text file.

    Args:
      http: An http connection object with drive credentials.
      docid: A string, the ID of the document.
      title: A string, the title of the document.
      contents: A string, the body of the document.
    """
    service = discovery.build('drive', 'v3', http=http)
    media = MediaInMemoryUpload(contents.encode('utf8'),
                                mimetype='text/plain',
                                resumable=True)
    return service.files().update(
        fileId=docid,
        body=dict(name=title),
        media_body=media).execute()


def get_options_docid():
    """Find the options doc id from the redirect file.

    Returns:
      The id of the doc to fix up.
    """
    htaccess = path.join(test_utils.find_repository_root(__file__), '.htaccess')
    lines = list(filter(None,
                        map(re.compile(r'RedirectMatch +/doc/options\$[\t ]+(.*)').match,
                            open(htaccess).readlines())))
    assert len(lines) == 1
    return list(filter(None, lines[0].group(1).split('/')))[-1]


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    args = parser.parse_args()

    # Find the document id.
    docid = get_options_docid()

    # Connect to the service.
    scopes = ['https://www.googleapis.com/auth/drive',
              'https://www.googleapis.com/auth/drive.scripts']
    _, http = gauth.get_auth_via_service_account(scopes)

    # Replace the document.
    replace_gdocs_document(http,
                           docid,
                           "Beancount - Options Reference",
                           options.list_options())


if __name__ == '__main__':
    main()
