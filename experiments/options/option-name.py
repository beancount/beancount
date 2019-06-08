#!/usr/bin/env python3
"""Parse and/or convert option names given on the command-line.

This requires the Ameritrade package.
https://bitbucket.org/blais/ameritrade/
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime
import logging
import re
from decimal import Decimal

from ameritrade import options


def ParseBeancountOptionSymbol(string):
    match = re.match(r'[A-Z]+', string)
    if not match:
        raise ValueError("Invalid Beancount option symbol: {:r}".format(string))
    symbol = match.group(0)
    rest = string[len(symbol):]
    expiration = datetime.datetime.strptime(rest[:6], "%y%m%d").date()
    side = rest[6]
    strike = Decimal(rest[7:])
    return options.Option(symbol, expiration, strike, side)


def MakeBeancountOptionSymbol(opt: options.Option):
    return '{symbol}{expiration:%y%m%d}{side}{strike}'.format(**opt._asdict())


def MakeReadableOptionName(opt: options.Option):
    return "{} {:%Y-%m-%d} {:.2f} {}".format(opt.symbol, opt.expiration, opt.strike,
                                             'Call' if opt.side == 'C' else 'Put')


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument(
        'names', nargs='+',
        help='Option names in Ameritrade symbol, Beancount symbol, or CUSIP format')
    args = parser.parse_args()

    opt = None
    for arg in args.names:
        for func in [ParseBeancountOptionSymbol,
                     options.ParseOptionSymbol,
                     options.ParseOptionCusip]:
            try:
                opt = func(arg)
            except ValueError:
                continue
            else:
                #print('Arg:        {}'.format(arg))
                print('Option:     {}'.format(opt))
                print('Name:       {}'.format(MakeReadableOptionName(opt)))
                print('Beancount:  {}'.format(MakeBeancountOptionSymbol(opt)))
                print('Ameritrade: {}'.format(options.MakeOptionSymbol(opt)))
                print('CUSIP:      {}'.format(options.MakeOptionCusip(opt)))
                print()
                break
        else:
            print("Unparseable option symbol: {:r}".format(arg))
            print()



if __name__ == '__main__':
    main()
