#!/usr/bin/env python3
"""Print the loaded options map.
"""

import argparse
import logging
import pprint

from beancount import loader


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Filename')
    args = parser.parse_args()

    _, __, options_map = loader.load_file(args.filename)
    pprint.pprint(options_map)


if __name__ == '__main__':
    main()
