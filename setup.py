#!/usr/bin/env python3
"""
Install script for beancount.
"""
__copyright__ = "Copyright (C) 2008-2011, 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"


from os import path
import datetime
import os
import platform
import re
import runpy
import subprocess
import sys
import warnings


# Check if the version is sufficient.
if sys.version_info[:2] < (3,5):
    raise SystemExit("ERROR: Insufficient Python version; you need v3.5 or higher.")


# Import setup().
setup_extra_kwargs = {}
if 'BEANCOUNT_DISABLE_SETUPTOOLS' in os.environ:
    # Note: this is used for testing only.
    from distutils.core import setup, Extension
    has_setuptools = False
else:
    try:
        from setuptools import setup, Extension
        has_setuptools = True
        setup_extra_kwargs.update(install_requires = [
            # Testing support now uses the pytest module.
            'pytest',

            # This is required to parse dates from command-line options in a
            # loose, accepting format. Note that we use dateutil for timezone
            # database definitions as well, although it is inferior to pytz, but
            # because it can use the OS timezone database in the Windows
            # registry. See this article for context:
            # http://www.assert.cc/2014/05/25/which-python-time-zone-library.html
            # However, for creating offset timezones, we use the datetime.timezone
            # helper class because it is built-in.
            # Where this matters is for price source fetchers.
            # (Note: If pytz supported the Windows registry timezone information,
            # I would switch to that.)
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

            # This library is needed to make requests for price sources.
            'requests',

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
        #   pytest -s beancount/scripts/setup_test.py
        #
    except ImportError:
        warnings.warn("Setuptools not installed; falling back on distutils. "
                      "You will have to install dependencies explicitly.")
        from distutils.core import setup, Extension
        has_setuptools = False


# Make sure we can import hashsrc in order to create a binary with a checksum of
# the C source code. This checksum is used to issue a warning when loading an
# incompatible binary. (Also note that you should _NOT_ add the path of this
# file to the PYTHONPATH directly because it contains a 'parser' module and that
# overrides the stdlib 'parser' module which is used by setuptools, and causes a
# subtle bug. That's why I import this utility directly from path).
hashsrc = runpy.run_path(path.join(path.dirname(__file__),
                                   'beancount/parser/hashsrc.py'))
hash_parser_source_files = hashsrc['hash_parser_source_files']


# Explicitly list the scripts to install.
binaries = [
    ('bean-bake', 'beancount.scripts.bake'),
    ('bean-check', 'beancount.scripts.check'),
    ('bean-doctor', 'beancount.scripts.doctor'),
    ('bean-example', 'beancount.scripts.example'),
    ('bean-format', 'beancount.scripts.format'),
    ('bean-price', 'beancount.prices.price'),
    ('bean-query', 'beancount.query.shell'),
    ('bean-report', 'beancount.reports.report'),
    ('bean-sql', 'beancount.scripts.sql'),
    ('bean-web', 'beancount.web.web'),
    ('bean-identify', 'beancount.ingest.identify'),
    ('bean-extract', 'beancount.ingest.extract'),
    ('bean-file', 'beancount.ingest.file'),
    ('treeify', 'beancount.tools.treeify'),
    ('upload-to-sheets', 'beancount.tools.sheets_upload'),
]

if has_setuptools and platform.system() == 'Windows':
    setup_extra_kwargs.update(entry_points={
        'console_scripts': [
            '{} = {}:main'.format(binary, module)
            for binary, module in binaries]
    })

else:
    setup_extra_kwargs.update(scripts=[
        path.join('bin', binary)
        for binary, _ in binaries])


def get_cflags():
    "Returns suitable CFLAGS for the platform."
    if platform.system() == "Windows":
        # unistd.h is not available with MSDEV.
        # See https://bitbucket.org/blais/beancount/issues/173/
        return ["-DYY_NO_UNISTD_H"]
    else:
        return None


# Read the version.
version_module = runpy.run_path(path.join(path.dirname(__file__),
                                          'beancount/__init__.py'))
version = version_module['__version__']
assert isinstance(version, str)

def get_hg_changeset():
    """Get the Mercurial changeset id."""
    try:
        output = subprocess.check_output(
            ['hg', 'parent', '--template', '{node} {date|hgdate}'], shell=False)
        vc_changeset, vc_timestamp = output.decode('utf-8').split()[:2]
        vc_changeset = 'hg:{}'.format(vc_changeset)
        return vc_changeset, vc_timestamp
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None

def get_git_changeset():
    """Get the Git changeset id."""
    try:
        output = subprocess.check_output(['git', 'log', '--pretty=%H %ct', '-1'],
                                         shell=False)
        match = re.match(r'([0-9a-f]+) ([0-9]+)$', output.decode('utf-8').strip())
        if match:
            vc_changeset = 'git:{}'.format(match.group(1))
            vc_timestamp = match.group(2)
            return vc_changeset, vc_timestamp
        else:
            return None
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None

# Get the changeset to bake into the binary.
for get_changeset in get_hg_changeset, get_git_changeset:
    changeset_timestamp = get_changeset()
    if changeset_timestamp is not None:
        vc_changeset, vc_timestamp = changeset_timestamp
        break
else:
    vc_changeset, vc_timestamp = '', 0


# Create a setup.
# Please read: http://furius.ca/beancount/doc/install about version numbers.
setup(
    name="beancount",
    version=version,
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

    license="GNU GPLv2 only",
    author="Martin Blais",
    author_email="blais@furius.ca",
    url="http://furius.ca/beancount",
    download_url="http://bitbucket.org/blais/beancount",

    packages = [
        'beancount',
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
        'beancount.ingest',
        'beancount.ingest.importers',
        'beancount.ingest.importers.mixins',
        'beancount.utils',
        'beancount.tools',
    ],

    package_data = {
        'beancount.web': ['*.ico',
                          '*.html',
                          '*.css',
                          'third_party/*.js'],
        'beancount.reports': ['*.html'],
        'beancount.utils.file_type': ['*'],
        'beancount.parser': ['*.h'], # See note for {63fc8d84d30a} above.
        },

    ext_modules = [
        Extension("beancount.parser._parser",
                  sources=[
                      "beancount/parser/lexer.c",
                      "beancount/parser/grammar.c",
                      "beancount/parser/parser.c",
                  ],
                  define_macros=[
                      ('RELEASE_VERSION', version),
                      ('VC_CHANGESET', vc_changeset),
                      ('VC_TIMESTAMP', int(float(vc_timestamp))),
                      ('PARSER_SOURCE_HASH', hash_parser_source_files())],
                  extra_compile_args=get_cflags()),
    ],

    # Include the Emacs support for completeness, for packagers not to have to
    # check out from the repository.
    data_files = [
        ('elisp', ['editors/emacs/beancount.el']),
    ],

    # Add optional arguments that only work with some variants of setup().
    **setup_extra_kwargs
)


# Development setup requires two tools IFF you need to change the grammar:
#
# - flex-2.6.4
# - bison-3.0.5
#
# These versions are related to what's on a recent Ubuntu. If you don't change
# the grammar nor the tokenizer, the C sources are checked in so you won't need
# those tools.
