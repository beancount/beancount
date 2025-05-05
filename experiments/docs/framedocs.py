#!/usr/bin/env python3
"""An iframe wrapper for a set of Google Docs documents.

This script is intended to be used to output static HTML files that wrap a set
of Google Docs. The idea is that you should be able to refer to these documents
with a nice URL, e.g., to share with others, and retain that URL at the top of
the document.

The second benefit it provides is that it opens the Google Docs in preview mode
by default, yet provides a convenient link to open the document in suggesting or
editing mode. Google Docs does not appear to support an option to open a
document in "Viewing" mode by default (and by that I mean "Viewing" as part of
the /edit page).
"""

__copyright__ = "Copyright (C) 2015-2016, 2019, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import collections
import logging
import os
import re
from os import path

TEMPLATE = """

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
  <head>
    <link href='https://fonts.googleapis.com/css?family=Bree+Serif' rel='stylesheet' type='text/css'>
    <style>
html, body {{ min-height:100%; padding:0; margin:0; }}
#wrapper {{ padding-top: 24px; position:absolute; top:0; bottom:0; left:0; right:0; }}
header {{ margin: -24px 0 0 0; padding: 0; height:24px; background-color: #F0F0F0; font-family: 'Bree Serif', serif; }}
header a {{ text-decoration: none; }}
#content {{ min-height: 100%; padding: 0; margin: 0; }}
div#left {{ float: left; padding: 2px; padding-left: 10px; }}
div#right {{ float: right; padding: 2px; padding-right: 10px; }}
    </style>
  </head>
  <body>
  <div id="wrapper">
    <header>
      <div id="left">
        <a href="index">Beancount Documentation</a>
      </div>
      <div id="right">
        <a href="https://docs.google.com/document/d/{id}/edit">Add Comments</a>
      </div>
    </header>
    <iframe id="content"
        src="https://docs.google.com/document/d/{id}/preview"
        width="100%" height="100%"
        frameborder="0" marginheight="0" marginwidth="0">
      Loading...
    </iframe>
  </div>
  </body>
</html>

"""


def parse_htaccess(filename):
    documents = collections.defaultdict(list)
    redirects = []
    with open(filename) as f:
        for line in f:
            match = re.match(r"RedirectMatch /doc/(.+?)\$\s+(.+)$", line)
            if match:
                name, url = match.groups()
                url_match = re.match("https://docs.google.com/document/d/(.+)/$", url)
                if url_match:
                    docid = url_match.group(1)
                    documents[docid].insert(0, name)
                else:
                    redirects.append((name, url))

    doc2id = {name[0]: docid for docid, name in documents.items()}
    for name, url in redirects:
        if not url.startswith("/beancount/doc/"):
            continue
        url = re.sub("^/beancount/doc/", "", url)
        try:
            docid = doc2id[url]
            documents[docid].append(name)
        except KeyError:
            pass

    return documents


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")

    parser.add_argument(
        "htaccess", action="store", default=os.getcwd(), help="Input .htaccess file"
    )
    parser.add_argument(
        "output",
        action="store",
        default=os.getcwd(),
        help="Output directory for all the filenames",
    )
    parser.add_argument(
        "docs_url", action="store", help="URL from the root where the documents are served"
    )

    args = parser.parse_args()

    # Parse the .htaccess file and extract the list of documents to create links for.
    documents = parse_htaccess(args.htaccess)

    # Generate wrapper files from the template.
    for docid, names in documents.items():
        name, synonyms = names[0], names[1:]
        filename = path.join(args.output, names[0])
        logging.info("Writing %s", filename)
        with open(filename, "w") as file:
            file.write(TEMPLATE.format(id=docid))

    # Generate a .htaccess for that directory.
    with open(path.join(args.output, ".htaccess"), "w") as file:
        for names in documents.values():
            name = names[0]
            name, synonyms = names[0], names[1:]
            for synonym in synonyms:
                print(
                    "RedirectMatch {}$ {}/{}".format(synonym, args.docs_url, name),
                    file=file,
                )


if __name__ == "__main__":
    main()
