#!/usr/bin/env python3
"""
Generate fake vesting events for one year.
"""
import uuid
import argparse
import logging
import textwrap
import decimal
import datetime
import io
import sys

from dateutil.parser import parse as parse_datetime
from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount.core import inventory
from beancount.parser import parser
from beancount.parser import printer


events = [
    #('2013-12-25', '2014-01-17', [('GOOG', '1110.72')]), # ?
    ('2014-01-25', '2014-02-14', [('GOOG', '1122.70')]), # ?
    ('2014-02-25', '2014-03-15', [('GOOG', '1212.51')]),
    ('2014-03-25', '2014-04-24', [('GOOG', '1157.93')]),
    ('2014-04-25', '2014-05-23', [('GOOG', '525.16'), ('GOOGL', '534.44')]),
    ('2014-05-25', '2014-06-21', [('GOOG', '552.70'), ('GOOGL', '563.80')]),
    ('2014-06-25', '2014-07-18', [('GOOG', '564.62'), ('GOOGL', '572.54')]),
    ('2014-07-25', '2014-08-15', [('GOOG', '593.35'), ('GOOGL', '603.10')]),
    ('2014-08-25', '2014-09-12', [('GOOG', '582.56'), ('GOOGL', '592.54')]),
    ('2014-09-25', '2014-10-10', [('GOOG', '587.99'), ('GOOGL', '598.42')]),
    ('2014-10-25', '2014-11-21', [('GOOG', '539.78'), ('GOOGL', '548.90')]),
    ('2014-11-25', '2014-12-19', [('GOOG', '539.27'), ('GOOGL', '547.48')]),
    ('2014-12-25', '2015-01-16', [('GOOG', '542.55'), ('GOOGL', '548.66')]), # ?
    ]

events = [(parse_datetime(date_vest).date(),
           parse_datetime(date_refund).date(),
           [(currency, D(amount_str))
            for currency, amount_str in amounts])
          for date_vest, date_refund, amounts in events]


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    argparser = argparse.ArgumentParser(__doc__.strip())
    argparser.add_argument('-s', '--shares', action='store_true', default=10)

    #parser.add_argument('filenames', nargs='+', help='Filenames')
    opts = argparser.parse_args()

    #decimal.getcontext().prec = 2
    Q = D('0.01')

    rate_medicare = D('0.0183')
    rate_federal  = D('0.25')
    rate_citynyc  = D('0.0424')
    rate_sdi      = D('0.')
    rate_stateny  = D('0.096')
    rate_socsec   = D('0.014')

    shares = D(opts.shares)

    output = sys.stdout
    output.write(textwrap.dedent("""
      plugin "beancount.ops.auto_accounts"
    """))

    for date_vest, date_refund, amounts in events:
        link = date_vest.strftime('google-vest.%Y-%m')

        vest_entries = []
        refunds = []

        # Generate vesting deposits corresponding to pay stubs.
        for currency, cost_amount in amounts:
            amount_gross = (cost_amount * shares).quantize(Q)

            amount_medicare = (amount_gross * rate_medicare).quantize(Q)
            amount_federal  = (amount_gross * rate_federal).quantize(Q)
            amount_citynyc  = (amount_gross * rate_citynyc).quantize(Q)
            amount_sdi      = (amount_gross * rate_sdi).quantize(Q)
            amount_stateny  = (amount_gross * rate_stateny).quantize(Q)
            amount_socsec   = (amount_gross * rate_socsec).quantize(Q)

            amount_refund = (amount_gross - (amount_medicare +
                                             amount_federal  +
                                             amount_citynyc  +
                                             amount_sdi      +
                                             amount_stateny  +
                                             amount_socsec)).quantize(Q)
            refunds.append(amount_refund)

            amount_gross = -amount_gross
            entries, _, __ = parser.parse_string(textwrap.dedent("""

              {date_vest} * "GOOGLE INC       PAYROLL" | "Vesting Event" ^{link}
                Income:US:GoogleInc:GSU           {amount_gross}    USD
                Expenses:Taxes:TY2014:US:Medicare {amount_medicare} USD
                Expenses:Taxes:TY2014:US:Federal  {amount_federal}  USD
                Expenses:Taxes:TY2014:US:CityNYC  {amount_citynyc}  USD
                Expenses:Taxes:TY2014:US:SDI      {amount_sdi}      USD
                Expenses:Taxes:TY2014:US:StateNY  {amount_stateny}  USD
                Expenses:Taxes:TY2014:US:SocSec   {amount_socsec}   USD
                Assets:US:GoogleInc:GSURefund     {amount_refund}   USD

            """.format(**vars())))
            vest_entries.extend(entries)


        # Generate conversion into shares in the integer number of shares.
        refund_balances = []
        for (currency, cost_amount), refund in zip(amounts, refunds):

            num_shares = int(refund / cost_amount)
            converted_refund = -num_shares * cost_amount

            refund_balances.append(refund + converted_refund)

            entries, _, __ = parser.parse_string(textwrap.dedent("""

              {date_vest} * "Conversion into shares" ^{link}
                Assets:US:GoogleInc:GSURefund {converted_refund} USD
                Assets:US:MSSB:{currency}     {num_shares} {currency} {{{cost_amount} USD}}

            """.format(**vars())))
            vest_entries.extend(entries)


        # Generate refund payment.
        oss = io.StringIO()
        oss.write('{date_refund} * "GOOGLE INC       PAYROLL" "Refund for fractional shares" ^{link}\n')
        total = ZERO
        for refund in refund_balances:
            neg_refund = -refund
            total += refund
            oss.write('  Assets:US:GoogleInc:GSURefund  {neg_refund} USD\n'.format(**vars()))
        oss.write('  Assets:US:BofA:Checking  {total} USD\n')
        oss.write('\n')

        date_balance = date_refund + datetime.timedelta(days=1)
        oss.write('{date_balance} balance Assets:US:GoogleInc:GSURefund   0 USD\n')

        entries, _, __ = parser.parse_string(textwrap.dedent(oss.getvalue().format(**vars())))
        assert entries
        vest_entries.extend(entries)

        printer.print_entries(vest_entries, file=output)
        output.write('\n\n')


if __name__ == '__main__':
    main()
