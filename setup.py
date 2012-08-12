#!/usr/bin/env python
"""
Install script for beancount.
"""

__author__ = "Martin Blais <blais@furius.ca>"

import os
from os.path import join, isfile
from distutils.core import setup, Extension


# Install all scripts under bin.
scripts = [join('bin', x) for x in ('bean-web',
                                    'bean-trial',
                                    'bean-suck',
                                    'bean-convert-ofx',
                                    'bean-convert-paypal-csv',
                                    'bean-convert-rbc-activity',
                                    )]

def read_version():
    try:
        return open('VERSION', 'r').readline().strip()
    except IOError, e:
        raise SystemExit(
            "Error: you must run setup from the root directory (%s)" % str(e))


# Include all files without having to create MANIFEST.in
def add_all_files(fun):
    import os, os.path
    from os.path import abspath, dirname, join
    def f(self):
        for root, dirs, files in os.walk('.'):
            if '.hg' in dirs: dirs.remove('.hg')
            self.filelist.extend(join(root[2:], fn) for fn in files
                                 if not fn.endswith('.pyc'))
        return fun(self)
    return f
from distutils.command.sdist import sdist
sdist.add_defaults = add_all_files(sdist.add_defaults)


setup(name="beancount",
      version=read_version(),
      description=\
      "Command-line Double-Entry Accounting",
      long_description="""
A double-entry accounting system that uses a simple text file format
as input. A few Python scripts are used to parse the contents of the
file, for example, to serve the contents as a locally running web
server. Scripts are provided to convert from OFX files into Ledger
input format, and other formats (easily extensible). 
""",
      license="GPL",
      author="Martin Blais",
      author_email="blais@furius.ca",
      url="http://furius.ca/beancount",
      package_dir = {'': 'lib/python'},
      packages = ['beancount',
                  'beancount.web',
                  'beancount.fallback',
                  'beancount.fallback.elementtree',
                  ],
      package_data = {'beancount.web': ['*jpg', '*png', 'robots.txt', 'treetable.js', 'style.css']},

      ext_modules = [
          Extension('beancount/cwallet',
                    extra_compile_args=['-std=c++0x', '-Wall'],
                    sources=['lib/python/beancount/cwallet.cpp', 'lib/python/beancount/itoa.cpp'])],

      scripts=scripts
     )


