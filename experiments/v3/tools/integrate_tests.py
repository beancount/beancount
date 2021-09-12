#!/usr/bin/env python3
"""Integrate tests.
"""

import argparse
import collections
import logging
import re
import textwrap
import sys


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('cc_filename', help='c++ filename')
    parser.add_argument('errors_filename', help='Errors filename')
    args = parser.parse_args()


    name = None
    blocks = collections.defaultdict(list)
    with open(args.errors_filename) as infile:
        for line in infile:
            match = re.match(r'\[ RUN      \] (.*)', line.rstrip())
            if match:
                name = match.group(1)
            blocks[name].append(line)
    del blocks[None]

    with open(args.cc_filename) as infile:
        cc_file = infile.read()

    for testname, block in blocks.items():
        block_string = ''.join(block)
        if re.search(r'\bFAILED\b', block_string):
            #print(testname)
            #print(block_string)

            match = re.search(r',-----+ Actual(.*)^-----+ Expected', block_string,
                              flags=re.MULTILINE|re.DOTALL)
            actual = textwrap.indent(match.group(1), '    ')
            #print(actual)

            match = re.search(
                r'{}\) {{.*?ExpectParse\(R"(.*?)", R"\('.format(testname.replace('.', ', ')),
                cc_file,
                flags=re.DOTALL|re.MULTILINE)
            point = match.end()
            cc_file = cc_file[:point] + actual + cc_file[point:]

    sys.stdout.write(cc_file)


if __name__ == '__main__':
    main()
