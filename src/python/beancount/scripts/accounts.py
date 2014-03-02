#!/usr/bin/env python3
"""
Print a list of all the accounts and some associated information.
"""
from beancount import load
from beancount.core import getters
from beancount.core.account import account_sortkey


def main():
    import argparse
    optparser = argparse.ArgumentParser(__doc__.strip())
    optparser.add_argument('filename', help='Filename.')
    opts = optparser.parse_args()

    entries, errors, options = load(opts.filename, quiet=True)

    open_close = getters.get_account_open_close(entries)

    maxlen = max(len(account.name) for account in open_close)
    for account, (open, close) in sorted(open_close.items(), key=lambda x: account_sortkey(x[0])):
        open_date = open.date if open else ''
        close_date = close.date if close else ''
        print('{:{len}}  {}  {}'.format(account.name, open_date, close_date, len=maxlen))


if __name__ == '__main__':
    main()
