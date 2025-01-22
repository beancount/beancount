#!/usr/bin/env python3
"""Convert selected OpenOffice documents to reStructuredText and build it into
Dominik Aumayr's Sphinx docs.

Two methods are possible from a Google Drive download:

- html -> rst (using pandoc)
- odt -> rst (using pandoc)


"""

__copyright__ = "Copyright (C) 2017-2018, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import shutil
import subprocess
import tempfile
from os import path

OVERLAY_DOCS = {
    "Beancount-Motivation": "cookbook/cl_cookbook.rst",
    "Beancount-Trading_with_Beancount": "cookbook/trading.rst",
    "Beancount-Cookbook-Vesting": "cookbook/stock_vesting.rst",
    "Beancount-Cookbook-Sharing_Expenses": "cookbook/sharing_expenses.rst",
}


def convert(inputdir, sphinxdir):
    """Process downloaded OpenOffice files to reStructuredText.

    Args:
      inputdir: The name of an input directory with ODT files.
      sphinxdir: A string, the name of the beancount-docs directory.
    """
    with tempfile.TemporaryDirectory(suffix=None, prefix=None, dir=None) as tmpdir:
        for basename, destname in sorted(OVERLAY_DOCS.items()):
            filename_odt = path.join(inputdir, basename + ".odt")
            logging.info("Processing %s", filename_odt)
            basename = path.splitext(path.basename(filename_odt))[0]
            filename_rst = path.join(tmpdir, basename + ".rst")
            subprocess.check_call(
                ["pandoc", "-f", "odt", "-t", "rst", "-o", filename_rst, filename_odt],
                shell=False,
            )

            try:
                overlay = OVERLAY_DOCS[basename]
                filename_overlay = path.join(sphinxdir, overlay)
                logging.info("Copying %s to %s", filename_rst, filename_overlay)
                shutil.copyfile(filename_rst, filename_overlay)
            except KeyError:
                pass

    subprocess.check_call(["make", "html"], cwd=sphinxdir)


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument("inputdir", help="Input filenames of OpenOffice documents")

    parser.add_argument("sphinxdir", help="Root of docs directory")

    args = parser.parse_args()

    convert(args.inputdir, args.sphinxdir)


if __name__ == "__main__":
    main()
