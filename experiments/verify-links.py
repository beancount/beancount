#!/usr/bin/env python3
"""Look at all the filename under a given directory hierarchy and make sure that
all the local & relative links they point to have corresponding existing files.
"""
from os import path
import re
import logging
import argparse
import sys
import urllib.request
import urllib.parse
import logging
import os

import lxml.html

from beancount.web import web


def check_links(filename, missing, empty):
    logging.info('Processing %s', filename)
    filedir = path.dirname(filename)
    contents = open(filename, 'rb').read()
    if len(contents) == 0:
        empty.add(filename)
    html = lxml.html.document_fromstring(contents)
    if html is None:
        return
    for element, attribute, link, pos in lxml.html.iterlinks(html):
        urlpath = urllib.parse.urlparse(link)
        if urlpath.scheme or urlpath.netloc:
            continue
        if path.isabs(urlpath.path):
            continue
        target = path.normpath(path.join(filedir, urlpath.path))
        if not path.exists(target):
            missing.add(target)


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('root', help='Root directory to scour')
    parser.add_argument('-v', '--verbose', action='store_true')
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO if args.verbose else logging.ERROR,
                        format='%(levelname)-8s: %(message)s')

    num_files = 0
    try:
        missing, empty = set(), set()
        for root, dirs, files in os.walk(args.root):
            for filename in files:
                check_links(path.join(root, filename), missing, empty)
                num_files += 1
    except KeyboardInterrupt:
        pass

    logging.info('%d files processed.', num_files)

    for target in missing:
        logging.error('Missing %s', target)

    for target in empty:
        logging.error('Empty %s', target)


if __name__ == '__main__':
    main()
