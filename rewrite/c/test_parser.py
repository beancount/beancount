#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import _beancount
import datetime


class Builder:
    def __init__(self):
        pass

    def parseDate(self, s):
        return datetime.datetime.strptime(s, '%Y-%m-%d').date()

    def parseAccount(self, s):
        return s

    def parseCurrency(self, s):
        return s

    def parseString(self, s):
        # print(s)
        return s[1:-1]

    def parseNumber(self, s):
        pass


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(__doc__.strip())
    parser.add_argument('filename', help='Filename')
    opts = parser.parse_args()

    builder = Builder()
    _beancount.parse(opts.filename, builder)


if __name__ == '__main__':
    main()
