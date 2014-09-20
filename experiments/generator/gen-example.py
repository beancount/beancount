#!/usr/bin/env python3
"""
Generate a decently-sized example file, based on some hard-coded rules.
"""
import argparse
import calendar
import datetime
import io
import itertools
import logging
import math
import random
import re
import sys
import textwrap
from textwrap import dedent

import dateutil
from dateutil import rrule

from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount.core import data
from beancount.core import flags
from beancount.core import account_types
from beancount.core.account import join
from beancount.parser import parser
from beancount.parser import printer
from beancount.ops import validation
from beancount import loader


A = account_types.DEFAULT_ACCOUNT_TYPES

source_filename = 'script://gen-example'
source = data.Source(source_filename, 0)

employers = [
    ('Hooli', "1 Carloston Rd, Mountain Beer, CA"),
    ('BayBook', "1501 Billow Rd, Benlo Park, CA"),
    ]

country = 'US'
currency = 'USD'
taxdef_currency = 'IRAUSD'
vacation_currency = 'VACHR'

retirement_limits = {2005: D('15500'),
                     2006: D('15500'),
                     2007: D('15500'),
                     2008: D('16000'),
                     2009: D('16000'),
                     2010: D('16500'),
                     2011: D('16500'),
                     2012: D('17000'),
                     2013: D('17500'),
                     2014: D('17500'),
                     2015: D('18000'),
                     2016: D('18000'),
                     None: D('18500')}


def parse(string):
    entries, errors, unused_options = parser.parse_string(string)
    if errors:
        printer.print_errors(errors, file=sys.stderr)
        raise ValueError("Parsed text has errors")
    return entries


def replace(string, replacements, strip=False):
    output = dedent(string)
    if strip:
        output = output.strip()
    for from_, to_ in replacements.items():
        if not isinstance(to_, str) and not callable(to_):
            to_ = str(to_)
        output = re.sub(r'\b{}\b'.format(from_), to_, output)
    return output

def skipiter(iterable, num_skip):
    it = iter(iterable)
    while 1:
        value = next(it)
        yield value
        for _ in range(num_skip):
            next(it)


def random_date_sequence(date_begin, date_end, days_mu, days_sigma):
    date = date_begin
    while date < date_end:
        nb_days_forward = max(1, int(random.normalvariate(days_mu, days_sigma)))
        date += datetime.timedelta(days=nb_days_forward)
        yield date


def generate_employment_income(employer,
                               address,
                               annual_salary,
                               account_checking,
                               account_retirement,
                               date_begin,
                               date_end):
    replacements = {
        'YYYY-MM-DD': date_begin,
        'CC': country,
        'Employer': employer,
        'Address': address,
        }

    preamble = replace("""

        * Employment Income: Employer

        YYYY-MM-DD event "Employer" "Address"

        YYYY-MM-DD open Income:CC:Employer:Salary           CCY
        ;YYYY-MM-DD open Income:CC:Employer:SignOnBonus      CCY
        ;YYYY-MM-DD open Income:CC:Employer:AnnualBonus      CCY
        ;YYYY-MM-DD open Income:CC:Employer:Match401k        CCY
        YYYY-MM-DD open Income:CC:Employer:GroupTermLife    CCY
        ;YYYY-MM-DD open Income:CC:Employer:HolidayGift      CCY
        ;YYYY-MM-DD open Income:CC:Employer:GymReimbursement CCY

        ;YYYY-MM-DD open Income:CC:Employer:Vacation         VACCCY
        ;YYYY-MM-DD open Assets:CC:Employer:Vacation         VACCCY

        YYYY-MM-DD open Expenses:Health:Life:GroupTermLife
        ;YYYY-MM-DD open Expenses:Health:Medical:Insurance
        ;YYYY-MM-DD open Expenses:Health:Dental:Insurance
        ;YYYY-MM-DD open Expenses:Health:Vision:Insurance
        ;YYYY-MM-DD open Expenses:Communications:Internet:Reimbursement
        ;YYYY-MM-DD open Expenses:Transportation:PublicTrans:TransitPreTax
        ;YYYY-MM-DD open Expenses:Vacation:Employer

    """, replacements)

    replacements['Checking'] = account_checking
    replacements['Retirement'] = account_retirement

    biweekly_pay = annual_salary / 26
    def replace_amount(match):
        fraction = D(match.group(1)[1:-1]) / D(100)
        return '{:.2f}'.format(biweekly_pay * fraction)

    replacements[r'({[0-9.]+})'] = replace_amount

    date_prev = None

    contrib_retirement = ZERO
    contrib_socsec = ZERO

    retirement_per_pay = D('2000')

    transactions = []
    for dt in skipiter(rrule.rrule(rrule.WEEKLY, byweekday=rrule.TH,
                               dtstart=date_begin, until=date_end), 1):
        date = dt.date()
        replacements['YYYY-MM-DD'] = date
        replacements['Year'] = 'Y{}'.format(date.year)

        if not date_prev or date_prev.year != date.year:
            contrib_retirement = retirement_limits[date.year]
            contrib_socsec = D('7250')
        date_prev = date

        gross = biweekly_pay

        retirement_uncapped = math.ceil((gross * D('0.25')) / 100) * 100
        retirement = min(contrib_retirement, retirement_uncapped)
        contrib_retirement -= retirement

        socsec_uncapped = gross * D('0.0610')
        socsec = max(0, contrib_socsec - socsec_uncapped)
        contrib_socsec -= socsec

        lifeinsurance = gross * D('0.0060')
        medicare      = gross * D('0.0230')
        federal       = gross * D('0.2300')
        state         = gross * D('0.0790')
        city          = gross * D('0.0380')
        sdi           = D('1.20')

        deposit = (gross -
                   retirement -
                   medicare -
                   federal -
                   state -
                   city -
                   sdi -
                   socsec)

        txn = replace("""

            YYYY-MM-DD * "Employer" | "Payroll"
              Checking                                          {deposit:.2f} CCY
              Retirement                                        {retirement:.2f} CCY
              Assets:CC:Federal:PreTax401k                     -{retirement:.2f} DEFCCY
              Expenses:Taxes:Year:CC:Federal:PreTax401k         {retirement:.2f} DEFCCY
              Income:CC:Employer:Salary                        -{gross:.2f} CCY
              Income:CC:Employer:GroupTermLife                 -{lifeinsurance:.2f} CCY
              Expenses:Health:Life:GroupTermLife                {lifeinsurance:.2f} CCY
              Expenses:Taxes:Year:CC:Medicare                   {medicare:.2f} CCY
              Expenses:Taxes:Year:CC:Federal                    {federal:.2f} CCY
              Expenses:Taxes:Year:CC:CityNYC                    {city:.2f} CCY
              Expenses:Taxes:Year:CC:SDI                        {sdi:.2f} CCY
              Expenses:Taxes:Year:CC:StateNY                    {state:.2f} CCY
              Expenses:Taxes:Year:CC:SocSec                     {socsec:.2f} CCY

        """.format(**vars()), replacements)
        txn = re.sub(r'({[0-9.]+})', replace_amount, txn)
        transactions.append(txn)

    return preamble + '\n\n'.join(transactions) + '\n\n'








def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    opts = parser.parse_args()

    date_birth = datetime.date(1980, 1, 1)
    date_begin = datetime.date(2012, 1, 1)
    date_end = datetime.date(2016, 1, 1)

    # Tax accounts.
    tax_io = io.StringIO()
    tax_io.write(replace("""
      * Tax accounts

      YYYY-MM-DD open Income:CC:Federal:PreTax401k     DEFCCY
      YYYY-MM-DD open Assets:CC:Federal:PreTax401k     DEFCCY

    """, {'YYYY-MM-DD': date_birth}))

    for year in range(date_begin.year, date_end.year):
        tax_io.write(replace("""
          ** Tax Year YEAR

          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:Federal:PreTax401k   DEFCCY
          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:Medicare             CCY
          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:Federal              CCY
          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:CityNYC              CCY
          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:SDI                  CCY
          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:StateNY              CCY
          YYYY-MM-DD open Expenses:Taxes:YYEAR:US:SocSec               CCY

          YYYY-MM-DD * "Allowed contributions for one year"
            Income:CC:Federal:PreTax401k    -LIMIT DEFCCY
            Assets:CC:Federal:PreTax401k     LIMIT DEFCCY

        """, {'YYYY-MM-DD': datetime.date(year, 1, 1),
              'YEAR': year,
              'YYEAR': 'Y{}'.format(year),
              'LIMIT': retirement_limits[year]}))

    # Banking accounts.
    bank_string = replace("""
      * Banking Accounts

      YYYY-MM-DD open Assets:CC:Bank:Checking    CCY
      ;; YYYY-MM-DD open Assets:CC:Bank:Savings    CCY
    """, {'YYYY-MM-DD': date_begin})

    # Investment accounts for retirement.
    retirement_string = replace("""
      * Investment accounts for retirement

      YYYY-MM-DD open Assets:CC:Retirement:Cash    CCY
    """, {'YYYY-MM-DD': date_begin})

    # Income sources.
    employer, address = employers[0]
    income_string = generate_employment_income(employer, address,
                                               D(120000),
                                               'Assets:CC:Bank:Checking',
                                               'Assets:CC:Retirement:Cash',
                                               date_begin, date_end)

    first_line = ';; -*- mode: org; mode: beancount; -*-\n'
    contents = replace(''.join([first_line,
                                bank_string,
                                retirement_string,
                                income_string,
                                tax_io.getvalue()]), {
        'CC': country,
        'CCY': currency,
        'VACCCY': vacation_currency,
        'DEFCCY': taxdef_currency,
        'Bank': 'BofA',
        'Retirement': 'Vanguard',
        })

    sys.stdout.write(contents)
    loader.load_string(contents,
                       log_errors=sys.stderr,
                       extra_validations=validation.HARDCORE_VALIDATIONS)


if __name__ == '__main__':
    main()
