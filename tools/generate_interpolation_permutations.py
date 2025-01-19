#!/usr/bin/env python3
"""
Generate all combinations and permutations of missing accounts.
"""

__copyright__ = "Copyright (C) 2016, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import sys


def gen_inputs(template, args):
    for mask in range(2 ** len(args)):
        actual_args = [arg if not (1 << i & mask) else "" for i, arg in enumerate(args)]
        sys.stdout.write(template.format(*actual_args))


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())
    _args = parser.parse_args()

    gen_inputs("  Assets:Account        {:7} {:3}\n", ["100.00", "USD"])
    gen_inputs(
        "  Assets:Account        {:7} {:3} @ {:7} {:3}\n", ["100.00", "USD", "1.20", "CAD"]
    )
    gen_inputs(
        "  Assets:Account        {:2} {:4} {{{:7} {:3}}}\n", ["10", "HOOL", "100.00", "USD"]
    )
    gen_inputs(
        "  Assets:Account        {:2} {:4} {{{:7} # {:7} {:3}}}\n",
        ["10", "HOOL", "100.00", "9.95", "USD"],
    )
    gen_inputs(
        "  Assets:Account        {:2} {:4} {{{:7} # {:7} {:3}}} @ {:7} {:3}\n",
        ["10", "HOOL", "100.00", "9.95", "USD", "120.00", "USD"],
    )


if __name__ == "__main__":
    main()
