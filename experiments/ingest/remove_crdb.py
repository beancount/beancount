"""Convert text files with '<number> CR' and '<number> DB' to signed numbers.

This is useful for converting CSV reports with CR and DB fields into something
that can actually be imported in a spreadsheet. An example is Think-or-Swim
input files, the signs are expressed as XXX.XX DB or XXX.XX CR, and this is just
a little tool to make it possible to import those.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import sys, re


def process_file_with_crdb(filename, outfile):
    """Replace CR/DB with signed values in file 'filename'.

    Args:
      filename: A string, the file to process.
      outfile: A file object, where to write the output.
    """
    for line in open(filename):
        if ',' not in line:
            continue
        line = re.sub(r'\xc2\xa0', r' ', line)
        line = re.sub(r'([-+]?[0-9][0-9.,]+) CR', r'\1', line)
        line = re.sub(r'([-+]?[0-9][0-9.,]+) DB', r'-\1', line)
        outfile.write(line)


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('filenames', nargs='+', help='Filenames')
    opts = parser.parse_args()

    for filename in opts.filenames:
        process_file_with_crdb(filename, sys.stdout)
