#!/usr/bin/env python3
"""
Install script for beancount.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import sys
if sys.version_info[:2] < (3,3):
    raise SystemExit("ERROR: Insufficient Python version; you need v3.3 or higher.")

import os
from os import path

#
sys.path.insert(0, path.join(path.dirname(__file__), 'src/python/beancount/parser'))
import hashsrc

# Note: there is a bug with setuptools that makes local installation fail,
# the _parser.so extension is copied under src/python instead of
# src/python/beancount/parser. I don't know why at this point.
# try:
#     from setuptools import setup, Extension
# except ImportError:
from distutils.core import setup, Extension


install_scripts = [path.join('bin', x) for x in """
bean-bake
bean-check
bean-doctor
bean-report
bean-query
bean-web
bean-example
bean-format
bean-sql
treeify
""".split() if x and not x.startswith('#')]


# Please read: http://furius.ca/beancount/doc/install about releases.
setup(
    name="beancount",
    version='2.0beta2',
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

    # See note about about setuptools above; uncomment if the problem gets fixed.
    ##install_requires = ['python-dateutil'],

    package_dir = {'': 'src/python'},
    packages = ['beancount',
                'beancount.parser',
                'beancount.core',
                'beancount.ops',
                'beancount.plugins',
                'beancount.query',
                'beancount.reports',
                'beancount.scripts',
                'beancount.web',
                'beancount.utils'],

    package_data = {
        'beancount.web': ['*.ico',
                          '*.html',
                          '*.css',
                          'third_party/*.js'],
        'beancount.reports': ['*.html'],
        },

    scripts=install_scripts,

    ext_modules=[
        Extension("beancount/parser/_parser",
                  sources=[
                      "src/python/beancount/parser/lexer.c",
                      "src/python/beancount/parser/grammar.c",
                      "src/python/beancount/parser/parser.c"
                  ],
                  define_macros=[('PARSER_SOURCE_HASH',
                                  '"{}"'.format(hashsrc.hash_parser_source_files()))]),
    ],
)
