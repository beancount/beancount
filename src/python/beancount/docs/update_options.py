"""Update the parts of the documentation that are auto-generated.

For example, the options documentation is a Google Doc. It can be generated from
the source code and updated automatically using this script.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import logging
import re
from os import path

from apiclient import http
from apiclient import discovery

from beancount.parser import options
from beancount.utils import test_utils
from beancount.docs import gauth


def replace_gdocs_document(connection, doc_id, title, contents):
    """Upload new contents for a Google Doc for a plain/text file.

    Args:
      connection: An http connection object with drive credentials.
      doc_id: A string, the ID of the document.
      title: A string, the title of the document.
      contents: A string, the body of the document.
    """
    # Create a media upload.
    media_body = http.MediaInMemoryUpload(
        contents.encode('utf8'), mimetype='text/plain', resumable=True)

    # Connect, with authentication.
    service = discovery.build('drive', 'v2', http=connection)
    doc = service.files().get(fileId=doc_id).execute()

    # Update the file metadata.
    doc['title'] = title
    doc['mimeType'] = 'text/plain'

    # Send the request to the API.
    updated_file = service.files().update(
        fileId=doc_id,
        body=doc,
        media_body=media_body).execute()
    return updated_file


def get_options_doc_id():
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
    parser = gauth.get_argparser(description=__doc__.strip())
    args = parser.parse_args()

    # Find the document id.
    doc_id = get_options_doc_id()

    # Connect to the service.
    scopes = ['https://www.googleapis.com/auth/drive',
              'https://www.googleapis.com/auth/drive.scripts']
    connection = gauth.get_authenticated_http(scopes, args)

    # Replace the document.
    replace_gdocs_document(connection,
                           doc_id,
                           "Beancount - Options Reference",
                           options.list_options())


if __name__ == '__main__':
    main()
