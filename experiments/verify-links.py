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




def check_links(filename, missing):
    logging.info('Processing %s', filename)
    filedir = path.dirname(filename)
    html = lxml.html.parse(filename).getroot()
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
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('root', help='Root directory to scour')
    args = parser.parse_args()

    try:
        missing = set()
        for root, dirs, files in os.walk(args.root):
            for filename in files:
                check_links(path.join(root, filename), missing)
    except KeyboardInterrupt:
        pass

    for target in missing:
        print(target)


if __name__ == '__main__':
    main()
