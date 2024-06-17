#!/usr/bin/env python3
"""Generate a version header file to include.

This is used in the Bazel build.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import textwrap


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("version_file", help="Filename")
    args = parser.parse_args()

    with open(args.version_file) as version_file:
        version = version_file.read().strip()

    header = textwrap.dedent(
        """\
      #ifndef __BEANCOUNT_VERSION_H__
      #define __BEANCOUNT_VERSION_H__

      #define BEANCOUNT_VERSION "{version}"

      #endif // __BEANCOUNT_VERSION_H__
    """.format(version=version)
    )
    print(header)


if __name__ == "__main__":
    main()
