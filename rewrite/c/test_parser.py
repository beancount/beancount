#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import _beancount


class Builder:
    def __init__(self):
        pass

    def date(self, s):
        pass # print(1)
        
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
