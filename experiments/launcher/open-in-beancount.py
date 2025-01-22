#!/usr/bin/env python3
"""Launcher script that will open a Beancount file in an editor at a specific line number.

How to install this on Ubuntu
-----------------------------

1. Copy "beancount.desktop" to /usr/share/applications

2. Edit the file to reflect the location of this script.

3. Update the database of desktop launchers with this command:

     sudo update-desktop-database

4. Test out the opener:

     xdg-open 'beancount:///path/to/file.beancount?lineno=100'

"""

__copyright__ = "Copyright (C) 2015-2016, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import subprocess
import sys
from urllib import parse

# You can change the command you run here.
LISP = r"""
(progn
  (find-file \"{filename}\")
  (goto-line {lineno})
  (recenter-top))
""".replace("\n", " ").strip()

EDITOR = r'emacsclient -s server0 -n --eval "{}"'.format(LISP)


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("url", help="URL to open")
    args = parser.parse_args()

    # Parse the URL and extract the filename and line number from it.
    url = parse.urlparse(args.url)
    query_args = parse.parse_qs(url.query)
    filename = url.path
    lineno = int(query_args["lineno"][0])

    # Launch your favorite editor to that given location.
    command = EDITOR.format(filename=filename, lineno=lineno)
    print(command)
    code = subprocess.call(command, shell=True)
    if code != 0:
        print("Error launching editor: {}".format(code), file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
