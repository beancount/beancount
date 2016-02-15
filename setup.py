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
            'python-dateutil',
            'bottle',
            'ply',
            'lxml',
            'python-magic',
            'google-api-python-client',
        ])
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
treeify
upload-csv-to-google-sheet
""".split() if x and not x.startswith('#')]


# Create a setup.
# Please read: http://furius.ca/beancount/doc/install about version numbers.
setup(
    name="beancount",
    version='2.0b6',
    description="Command-line Double-Entry Accounting",

    long_description=
    """
      A double-entry accounting system that uses a simple text file format
      as input. A few Python scripts are used to parse the contents of the
      file, for example, to serve the contents as a locally running web
      server. Scripts are provided to convert from various input files into
      Beancount's input format.
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
                'beancount.utils'],

    package_data = {
        'beancount.web': ['*.ico',
                          '*.html',
                          '*.css',
                          'third_party/*.js'],
        'beancount.reports': ['*.html'],
        'beancount.utils.file_type': ['*'],

        # Note: There seems to be no other way to get the header files included
        # in the sdist but this workaround.
        'beancount.parser': ['*.h'],
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

    # This fails with setuptools: The headers aren't included in the resulting sdist.
    # headers=[
    #     "src/python/beancount/parser/lexer.h",
    #     "src/python/beancount/parser/grammar.h",
    #     "src/python/beancount/parser/parser.h",
    # ],

    # Add optional arguments that only work with some variants of setup().
    **setup_extra_kwargs
)
