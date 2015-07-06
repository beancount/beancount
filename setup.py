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
from distutils.core import setup, Extension

# Unused support for setuptools. Setuptools is seriously broken:
#
# * Using setuptools v15, build_ext --in-place puts the compiled Extension
#   library under src/python. It needs to be under src/python/beancount/parser.
#
# * Using setuptools v18, it puts the library in the right place but invoking
#   it not from pip3 or easy_install makes it complain about install_requires
#   not being supported.
#
# Setuptools is broken. The only reason I was trying to use it was to support
# automatically installed dependencies. But it's so broken I'm giving up.
# Here's how to install dependencies:
#
#    pip3 install python-dateutil bottle ply lxml
#
# You do this once.
# Removed code follows.
#
## try:
##     # Use distutils if requested (this is use in testing).
##     if 'BEANCOUNT_DISABLE_SETUPTOOLS' in os.environ:
##         raise ImportError("Setuptools disabled explicitly")
##
##     # Try to use setuptools first, if it is installed, because it supports
##     # automatic installation of dependencies.
##     from setuptools import setup, Extension
##     if setuptools.__version__ < '18':
##
##     setup_extra_kwargs.update(
##         install_requires = ['python-dateutil', 'bottle', 'ply', 'lxml']
##         )
## except ImportError:
##     # If setuptools is not installed, fallback on the stdlib. This works too, it
##     # just won't install the dependencies automatically.
##     warnings.warn("Setuptools not installed; falling back on distutils. "
##                   "You will have to install dependencies explicitly.")
##     from distutils.core import setup, Extension


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
bean-report
bean-query
bean-web
bean-example
bean-format
bean-sql
treeify
upload-csv-to-google-sheet
""".split() if x and not x.startswith('#')]


# Create a setup.
# Please read: http://furius.ca/beancount/doc/install about version numbers.
setup(
    name="beancount",
    version='2.0b3',
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
                'beancount.web',
                'beancount.docs',
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
                                  '"{}"'.format(hash_parser_source_files()))]),
    ],

    # Add optional arguments that only work with some variants of setup().
    **setup_extra_kwargs
)
