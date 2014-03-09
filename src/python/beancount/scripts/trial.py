"""Parse and realize a beancount input file, and output a tree of balances.

This is the equivalent to the "trial balance" page.

FIXME: Eventually all of web served pages should have an ASCII equivalent, and
we should remove this and use the common method in order to render these ASCII
reports. For now, this is a bit of a demo and a debugging tool as well.
"""
import argparse

from beancount import load
from beancount.core import realization


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('filename', help='Beancount input filename.')
    opts = parser.parse_args()

    # Load the file, realize it, and dump the accounts tree.
    entries, errors, options = load(opts.filename, do_print_errors=True)
    real_accounts = realization.realize(entries)
    realization.dump_tree_balances(real_accounts)


if __name__ == '__main__':
    main()
