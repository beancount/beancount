"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
import sys
import argparse
from beancount.parser.parser import dump_lexer


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('filename', help='Beancount input filename.')

    parser.add_argument('--dump-lexer', action='store_true',
                        help="Dump the lexers for a beancount input.")

    opts = parser.parse_args()

    if opts.dump_lexer:
        dump_lexer(opts.filename, sys.stdout)


if __name__ == '__main__':
    main()
