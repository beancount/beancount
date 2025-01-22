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

__copyright__ = "Copyright (C) 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import functools
import json
import re
from typing import Any
from typing import Callable
from typing import Iterator
from typing import Mapping
from typing import Optional

from googleapiclient import discovery

from beancount.tools import gapis  # See http://github.com/blais/gapis

Json = Mapping[str, "Json"]


def find_links(obj: Any, find_key: str) -> Iterator[list[Json]]:
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


def iter_links(document: Json) -> list[tuple[str, str]]:
    """Find all the links and return them."""
    for jpath in find_links(document, "link"):
        for item in jpath:
            if "textRun" in item:
                content = item["textRun"]["content"]
                link = item["textRun"]["textStyle"]["link"]
                if "url" not in link:
                    continue
                url = link["url"]
                yield (url, content, item)


def process_links(document: Json, func: Callable[[str, str], Optional[str]]) -> list[Json]:
    """Find all the links and prepare updates.
    Outputs a list of batchUpdate requests to apply."""
    requests = []
    for url, content, item in iter_links(document):
        proposed_url = func(url, content)
        if proposed_url:
            requests.append(
                {
                    "updateTextStyle": {
                        "range": {
                            "startIndex": item["startIndex"],
                            "endIndex": item["endIndex"],
                        },
                        "textStyle": {"link": {"url": proposed_url}},
                        "fields": "link",
                    }
                }
            )
    return requests


def propose_url(mapping: dict[str, str], url: str, unused_content: str) -> Optional[str]:
    """Process a URL, and optionally propose a replacement."""
    try:
        return mapping[url]
    except KeyError:
        pass


def transform_links(service, docid: str, mapping: dict[str, str], dry_run: bool):
    """Run the link transformation."""

    # Get the document.
    document = service.documents().get(documentId=docid).execute()

    links = list(iter_links(document))
    width = max(len(url) for url, _, __ in links)
    for url, content, _ in links:
        print(f"# {url:{width}}  {content}")
    if links:
        print()

    if dry_run:
        # Print the links only.
        string = json.dumps({url: url for url, _, __ in links}, indent=4, sort_keys=True)
        string = re.sub(r'",', '",\n', re.sub(r": ", "\n    ", string))
        print(string)

    # Create replacement requests.
    requests = process_links(document, functools.partial(propose_url, mapping))
    if dry_run:
        return

    # Put together a batch update.
    if requests:
        # Execute them.
        print("Sending {} requests".format(len(requests)))
        service.documents().batchUpdate(
            documentId=docid, body={"requests": list(reversed(requests))}
        ).execute()
    else:
        print("No changes.")


def main():
    """Main function."""
    parser = argparse.ArgumentParser(
        description=__doc__.strip(), formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("docid", action="store", help="Kix doc id to update")
    parser.add_argument(
        "mapping_json",
        nargs="?",
        action="store",
        help="Path to JSON list of pairs to replace",
    )
    parser.add_argument(
        "--dry-run",
        "-n",
        action="store_true",
        help="Only print the existing links and replacments, do not run.",
    )
    args = parser.parse_args()

    # Parse out the doc id, in case the user provided a full URL.
    if args.docid:
        match = re.match("https://docs.google.com/document/d/([^/]+)(/|$)", args.docid)
        if match:
            args.docid = match.group(1)

    # Read a file of mappings to apply in JSON format. See docstring.
    if args.mapping_json:
        with open(args.mapping_json) as mapfile:
            mapping = json.load(mapfile)
    else:
        mapping = {}

    # Discover the service.
    creds = gapis.get_credentials(
        ["https://www.googleapis.com/auth/documents"], "beancount-docs"
    )
    service = discovery.build("docs", "v1", credentials=creds)

    transform_links(service, args.docid, mapping, args.dry_run)


if __name__ == "__main__":
    main()
