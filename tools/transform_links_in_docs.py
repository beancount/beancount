#!/usr/bin/env python3
"""Transform links embedded in docs.

The script is given a doc id and a JSON file with a mapping, like this:

  {
    "https://github.com/beancount/beancount/blob/v2/experiments/returns/config.proto":
    "https://github.com/beancount/beanlabs/blob/master/beanlabs/returns/config.proto",
    "https://github.com/beancount/beancount/blob/v2/experiments/returns/configure.py":
    "https://github.com/beancount/beanlabs/blob/master/beanlabs/returns/configure.py",
    "https://github.com/beancount/beancount/tree/v2/experiments/returns":
    "https://github.com/beancount/beanlabs/tree/master/beanlabs/returns"
    ...
  }

and replaces the links in the doc that have an entry in the mapping. Links not
matching any entry are ignored.
"""
__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
from typing import List, Optional, Dict, Any, Mapping, Iterator, Callable, Tuple
import argparse
import json
import functools
import logging
import pickle
import pprint
import re

from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport import requests as grequests
from googleapiclient import discovery


def get_credentials(scopes: List[str],
                    secrets_filename: Optional[str] = None,
                    storage_filename: Optional[str] = None):
    """Authenticate via oauth2 and return credentials."""
    logging.getLogger('googleapiclient.discovery_cache').setLevel(logging.ERROR)

    import __main__  # pylint: disable=import-outside-toplevel
    cache_basename = path.expanduser(
        path.join("~/.google", path.splitext(path.basename(__main__.__file__))[0]))
    if secrets_filename is None:
        secrets_filename = "{}.json".format(cache_basename)
    if storage_filename is None:
        storage_filename = "{}.cache".format(cache_basename)

    # Load the secrets file, to figure if it's for a service account or an OAUTH
    # secrets file.
    secrets_info = json.load(open(secrets_filename))
    if secrets_info.get("type") == "service_account":
        # Process service account flow.
        # pylint: disable=import-outside-toplevel
        import google.oauth2.service_account as sa
        credentials = sa.Credentials.from_service_account_info(
            secrets_info, scopes=scopes)
    else:
        # Process OAuth flow.
        credentials = None
        if path.exists(storage_filename):
            with open(storage_filename, 'rb') as token:
                credentials = pickle.load(token)
        # If there are no (valid) credentials available, let the user log in.
        if not credentials or not credentials.valid:
            if credentials and credentials.expired and credentials.refresh_token:
                credentials.refresh(grequests.Request())
            else:
                flow = InstalledAppFlow.from_client_secrets_file(
                    secrets_filename, scopes)
                credentials = flow.run_console()
            # Save the credentials for the next run
            with open(storage_filename, 'wb') as token:
                pickle.dump(credentials, token)

    return credentials


Json = Mapping[str, 'Json']


def find_links(obj: Any, find_key: str) -> Iterator[List[Json]]:
    """Enumerate all the links found.
    Returns a path of object, from leaf to parents to root."""
    if isinstance(obj, dict):
        for key, value in obj.items():
            if key == find_key:
                yield [value, obj]
            else:
                for found in find_links(value, find_key):
                    found.append(obj)
                    yield found
    elif isinstance(obj, list):
        for value in obj:
            for found in find_links(value, find_key):
                found.append(obj)
                yield found


def iter_links(document: Json) -> List[Tuple[str, str]]:
    """Find all the links and return them."""
    for jpath in find_links(document, 'link'):
        for item in jpath:
            if 'textRun' in item:
                content = item['textRun']['content']
                url = item['textRun']['textStyle']['link']['url']
                yield (url, content, item)


def process_links(document: Json,
                  fn: Callable[[str, str], Optional[str]]) -> List[Json]:
    """Find all the links and prepare updates.
    Outputs a list of batchUpdate requests to apply."""
    requests = []
    for url, content, item in iter_links(document):
        proposed_url = fn(url, content)
        if proposed_url:
            requests.append({
                'updateTextStyle': {
                    'range': {'startIndex': item['startIndex'],
                              'endIndex': item['endIndex']},
                    'textStyle': {'link': {'url': proposed_url}},
                    'fields': 'link'
                }})
    return requests


def propose_url(mapping: Dict[str, str], url: str, content: str) -> Optional[str]:
    """Process a URL, and optionally propose a replacement."""
    try:
        return mapping[url]
    except KeyError:
        pass


def transform_links(service, docid: str, mapping: Dict[str, str], dry_run: bool):
    """Run the link transformation."""

    # Get the document.
    document = service.documents().get(documentId=docid).execute()

    if dry_run:
        # Print the links only.
        links = list(iter_links(document))
        width = max(len(url) for url, _, __ in links)
        for url, content, _ in links:
            print(f"{url:{width}}  {content}")
        if links:
            print()

    # Create replacement requests.
    requests = process_links(document, functools.partial(propose_url, mapping))

    # Put together a batch update.
    if requests:
        if dry_run:
            pprint.pprint(requests)
        else:
            print("Sending {} requests".format(len(requests)))
            resp = service.documents().batchUpdate(
                documentId=docid,
                body={'requests': list(reversed(requests))}).execute()
    else:
        print("No changes.")


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip(),
                                     formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('docid', action='store',
                        help="Kix doc id to update")
    parser.add_argument('mapping_json', nargs='?', action='store',
                        help="Path to JSON list of pairs to replace")
    parser.add_argument('--dry-run', '-n', action='store_true',
                        help="Only print the existing links and replacments, do not run.")
    args = parser.parse_args()


    # Parse out the doc id, in case the user provided a full URL.
    if args.docid:
        match = re.match('https://docs.google.com/document/d/([^/]+)(/|$)', args.docid)
        if match:
            args.docid = match.group(1)

    # Read a file of mappings to apply in JSON format. See docstring.
    if args.mapping_json:
        with open(args.mapping_json) as mapfile:
            mapping = json.load(mapfile)
    else:
        mapping = {}

    # Discover the service.
    creds = get_credentials(['https://www.googleapis.com/auth/documents'])
    service = discovery.build('docs', 'v1', credentials=creds)

    transform_links(service, args.docid, mapping, args.dry_run)




if __name__ == '__main__':
    main()
