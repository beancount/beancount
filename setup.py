#!/usr/bin/env python3
"""
Install script for beancount.
"""

__copyright__ = "Copyright (C) 2008-2011, 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"


from os import path
import platform
import re
import runpy
import subprocess

from setuptools import setup, find_packages, Extension

# Make sure we can import hashsrc in order to create a binary with a checksum of
# the C source code. This checksum is used to issue a warning when loading an
# incompatible binary. (Also note that you should _NOT_ add the path of this
# file to the PYTHONPATH directly because it contains a 'parser' module and that
# overrides the stdlib 'parser' module which is used by setuptools, and causes a
# subtle bug. That's why I import this utility directly from path).
hashsrc = runpy.run_path(path.join(path.dirname(__file__), "beancount/parser/hashsrc.py"))
hash_parser_source_files = hashsrc["hash_parser_source_files"]


def get_cflags():
    "Returns suitable CFLAGS for the platform."
    if platform.system() == "Windows":
        # unistd.h is not available with MSDEV.
        # See https://bitbucket.org/blais/beancount/issues/173/
        return ["-DYY_NO_UNISTD_H"]
    else:
        return ["-std=gnu99"]


# Read the version.
with open("beancount/VERSION") as version_file:
    version = version_file.read().strip()
assert isinstance(version, str)


def get_hg_changeset():
    """Get the Mercurial changeset id."""
    try:
        output = subprocess.check_output(
            ["hg", "parent", "--template", "{node} {date|hgdate}"], shell=False
        )
        vc_changeset, vc_timestamp = output.decode("utf-8").split()[:2]
        vc_changeset = "hg:{}".format(vc_changeset)
        return vc_changeset, vc_timestamp
    except (subprocess.CalledProcessError, FileNotFoundError, PermissionError):
        return None


def get_git_changeset():
    """Get the Git changeset id."""
    try:
        output = subprocess.check_output(
            ["git", "log", "--pretty=%H %ct", "-1"], shell=False
        )
        match = re.match(r"([0-9a-f]+) ([0-9]+)$", output.decode("utf-8").strip())
        if match:
            vc_changeset = "git:{}".format(match.group(1))
            vc_timestamp = match.group(2)
            return vc_changeset, vc_timestamp
        else:
            return None
    except (subprocess.CalledProcessError, FileNotFoundError, PermissionError):
        return None


# Get the changeset to bake into the binary.
for get_changeset in get_git_changeset, get_hg_changeset:
    changeset_timestamp = get_changeset()
    if changeset_timestamp is not None:
        vc_changeset, vc_timestamp = changeset_timestamp
        break
else:
    vc_changeset, vc_timestamp = "", 0


install_requires = []

# Create a setup.
# Please read: http://furius.ca/beancount/doc/install about version numbers.
setup(
    name="beancount",
    version=version,
    description="Command-line Double-Entry Accounting",
    long_description="""
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
    url="https://github.com/beancount/beancount",
    download_url="https://github.com/beancount/beancount",
    packages=find_packages(exclude=["experiments*"]),
    package_data={
        "beancount": ["VERSION"],
        "beancount.utils.file_type_testdata": ["*"],
    },
    ext_modules=[
        Extension(
            "beancount.parser._parser",
            include_dirs=["."],
            sources=[
                "beancount/parser/decimal.c",
                "beancount/parser/lexer.c",
                "beancount/parser/grammar.c",
                "beancount/parser/parser.c",
                "beancount/parser/tokens.c",
            ],
            define_macros=[
                ("BEANCOUNT_VERSION", version),
                ("VC_CHANGESET", vc_changeset),
                ("VC_TIMESTAMP", int(float(vc_timestamp))),
                ("PARSER_SOURCE_HASH", hash_parser_source_files()),
            ],
            extra_compile_args=get_cflags(),
        ),
    ],
    install_requires=[
        # Command line parsing.
        "click",
        # Used to generate recurring events in beancounr.scripts.example.
        "python-dateutil",
        # Unicode character class aware regular expressions.
        "regex",
    ],
    entry_points={
        "console_scripts": [
            "bean-check = beancount.scripts.check:main",
            "bean-doctor = beancount.scripts.doctor:main",
            "bean-example = beancount.scripts.example:main",
            "bean-format = beancount.scripts.format:main",
            "treeify = beancount.tools.treeify:main",
        ]
    },
    python_requires=">=3.6",
)


# Development setup requires two tools IFF you need to change the grammar:
#
# - flex-2.6.4
# - bison-3.6.4
#
# These versions are related to what's on a recent Ubuntu. If you don't change
# the grammar nor the tokenizer, the C sources are checked in so you won't need
# those tools.
