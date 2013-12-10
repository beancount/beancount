#!/usr/bin/env python3
"""
Install script for beancount.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import sys
if sys.version_info[:2] < (3,3):
    raise SystemExit("ERROR: Insufficient Python version; you need v3.3 or higher.")

import os
from os.path import join, isfile
from distutils.core import setup, Extension

install_scripts = [join('bin', x) for x in """
bean-check
bean-accounts
bean-web
bean-grep
bean-import
bean-prices
bean-remove-crdb
bean-trial
bean-v1tov2
# bean-bake, not working yet
# bean-check-directories, not ready yet
# bean-dump-lexer, developer tool
# bean-sandbox, developer tool
""".splitlines() if x and not x.startswith('#')]


# # Include all files without having to create MANIFEST.in
# def add_all_files(fun):
#     import os, os.path
#     from os.path import abspath, dirname, join
#     def f(self):
#         for root, dirs, files in os.walk('.'):
#             if '.hg' in dirs: dirs.remove('.hg')
#             self.filelist.extend(join(root[2:], fn) for fn in files
#                                  if not fn.endswith('.pyc'))
#         return fun(self)
#     return f
# from distutils.command.sdist import sdist
# sdist.add_defaults = add_all_files(sdist.add_defaults)



setup(
  name="beancount",
  version='2.0beta',
  description="Command-line Double-Entry Accounting",
  long_description="""
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

  package_dir = {'': 'src/python'},
  packages = ['beancount',
              'beancount.parser',
              'beancount.core',
              'beancount.ops',
              'beancount.sources',
              'beancount.imports',
              'beancount.utils',
              'beancount.web',
              ],
  package_data = {'beancount.web': ['*.ico', '*.html', '*.css']},
  scripts=install_scripts,

  ext_modules=[
      Extension("beancount/parser/_parser",
                sources=[
                    "src/python/beancount/parser/lexer.c",
                    "src/python/beancount/parser/grammar.c",
                    "src/python/beancount/parser/parser.c"
                ]),
  ],
)
