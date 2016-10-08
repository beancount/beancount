#!/usr/bin/env python3
"""
Install script for beancount.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import os
from os import path
import runpy
import sys
import warnings


# Check if the version is sufficient.
if sys.version_info[:2] < (3,3):
    raise SystemExit("ERROR: Insufficient Python version; you need v3.3 or higher.")


# Import setup().
setup_extra_kwargs = {}
if 'BEANCOUNT_DISABLE_SETUPTOOLS' in os.environ:
    # Note: this is used for testing only.
    from distutils.core import setup, Extension
else:
    try:
        from setuptools import setup, Extension
        setup_extra_kwargs.update(install_requires = [
            # This is required to parse dates from command-line options in a
            # loose, accepting format.
            'python-dateutil',

            # The SQL parser uses PLY in order to parse the input syntax.
            'ply',

            # The bean-web web application is built on top of this web
            # framework.
            'bottle',

            # This XML parsing library is mainly required to web scrape the
            # bean-web pages for testing.
            'lxml',

            # This library is needed to identify the type of a file for import.
            'python-magic',

            # This library is needed to parse XML files (for the OFX examples).
            'beautifulsoup4',

            # This library is needed to identify the character set of a file for
            # import, in order to read its contents and match expressions
            # against it.
            'chardet',

            # This library is used to download and convert the documentation
            # programmatically and to upload lists of holdings to a Google
            # Spreadsheet for live intra-day monitoring.
            'google-api-python-client',
        ])

        # A note about setuptools: It's profoundly BROKEN.
        #
        # - The header files are needed in order to distribution a working
        #   source distribution.
        # - Listing the header files under the extension "sources" fails to
        #   build; distutils cannot make out the file type.
        # - Listing them as "headers" makes them ignored; extra options to
        #   Extension() appear to be ignored silently.
        # - Listing them under setup()'s "headers" makes it recognize them, but
        #   they do not get included.
        # - Listing them with "include_dirs" of the Extension fails as well.
        #
        # The only way I managed to get this working is by working around and
        # including them as "package_data" (see {63fc8d84d30a} below). That
        # includes the header files in the sdist, and a source distribution can
        # be installed using pip3 (and be built locally). However, the header
        # files end up being installed next to the pure Python files in the
        # output. This is the sorry situation we're living in, but it works.
        #
        # If you think I'm a lunatic, fix it and make sure you can make this
        # command succeed:
        #   nosetests3 -s .../src/python/beancount/scripts/setup_test.py
        #
    except ImportError:
        warnings.warn("Setuptools not installed; falling back on distutils. "
                      "You will have to install dependencies explicitly.")
        from distutils.core import setup, Extension


# Make sure we can import hashsrc in order to create a binary with a checksum of
# the C source code. This checksum is used to issue a warning when loading an
# incompatible binary. (Also note that you should _NOT_ add the path of this
# file to the PYTHONPATH directly because it contains a 'parser' module and that
# overrides the stdlib 'parser' module which is used by setuptools, and causes a
# subtle bug. That's why I import this utility directly from path).
hashsrc = runpy.run_path(path.join(path.dirname(__file__),
                                   'src/python/beancount/parser/hashsrc.py'))
hash_parser_source_files = hashsrc['hash_parser_source_files']


# Explicitly list the scripts to install.
install_scripts = [path.join('bin', x) for x in """
bean-bake
bean-check
bean-doctor
bean-example
bean-format
bean-price
bean-query
bean-report
bean-sql
bean-web
bean-identify
bean-extract
bean-file
treeify
upload-csv-to-google-sheet
""".split() if x and not x.startswith('#')]


# Create a setup.
# Please read: http://furius.ca/beancount/doc/install about version numbers.
setup(
    name="beancount",
    version='2.0b12',
    description="Command-line Double-Entry Accounting",

    long_description=
    """
      A double-entry accounting system that uses text files as input.

      Beancount defines a simple data format or "language" that lets you define
      financial transaction records in a text file, load them in memory and
      generate and export a variety of reports, such as balance sheets or income
      statements. It also provides a client with an SQL-like query language to
      filter and aggregate financial data, and a web interface which renders
      those reports to HTML. Finally, it provides the scaffolding required to
      automate the conversion of external data into one's input file in
      Beancount syntax.
    """,

    license="GPL",
    author="Martin Blais",
    author_email="blais@furius.ca",
    url="http://furius.ca/beancount",
    download_url="http://bitbucket.org/blais/beancount",

    package_dir = {'': 'src/python',},
    packages = ['beancount',
                'beancount.parser',
                'beancount.core',
                'beancount.ops',
                'beancount.plugins',
                'beancount.query',
                'beancount.reports',
                'beancount.scripts',
                'beancount.prices',
                'beancount.prices.sources',
                'beancount.web',
                'beancount.docs',
                'beancount.ingest',
                'beancount.ingest.importers',
                'beancount.utils'],

    package_data = {
        'beancount.web': ['*.ico',
                          '*.html',
                          '*.css',
                          'third_party/*.js'],
        'beancount.reports': ['*.html'],
        'beancount.utils.file_type': ['*'],
        'beancount.parser': ['*.h'], # See note for {63fc8d84d30a} above.
        },

    scripts=install_scripts,

    ext_modules=[
        Extension("beancount.parser._parser",
                  sources=[
                      "src/python/beancount/parser/lexer.c",
                      "src/python/beancount/parser/grammar.c",
                      "src/python/beancount/parser/parser.c",
                  ],
                  define_macros=[('PARSER_SOURCE_HASH',
                                  '"{}"'.format(hash_parser_source_files()))]),
    ],

    # Add optional arguments that only work with some variants of setup().
    **setup_extra_kwargs
)
