#!/usr/bin/env python3
"""
Generate a decently-sized example file, based on some hard-coded rules.
"""
import argparse
import calendar
import datetime
import decimal
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
from beancount.core import amount
from beancount.core import inventory
from beancount.core import account_types
from beancount.core import realization
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
    ('Babble', "1 Continuous Loop, Bupertina, CA"),
    ('Hoogle', "1600 Amphibious Parkway, River View, CA"),
    ]

retirement_limits = {2000: D('10500'),
                     2001: D('10500'),
                     2002: D('11000'),
                     2003: D('12000'),
                     2004: D('13000'),
                     2005: D('14000'),
                     2006: D('15000'),
                     2007: D('15500'),
                     2008: D('15500'),
                     2009: D('16500'),
                     2010: D('16500'),
                     2011: D('16500'),
                     2012: D('17000'),
                     2013: D('17500'),
                     2014: D('17500'),
                     2015: D('18000'),
                     2016: D('18000'),
                     None: D('18500')}


def debug(*args):
    print(*args, file=sys.stderr)


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
        'Employer': employer,
        'Address': address,
        }

    preamble = replace("""

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
            contrib_socsec = D('7000')
        date_prev = date

        gross = biweekly_pay

        retirement_uncapped = math.ceil((gross * D('0.25')) / 100) * 100
        retirement = min(contrib_retirement, retirement_uncapped)
        contrib_retirement -= retirement

        socsec_uncapped = gross * D('0.0610')
        socsec = min(contrib_socsec, socsec_uncapped)
        contrib_socsec -= socsec

        with decimal.localcontext() as ctx:
            ctx.prec = 6
            lifeinsurance = gross * D('0.0060')
            medicare      = gross * D('0.0230')
            federal       = gross * D('0.2300')
            state         = gross * D('0.0790')
            city          = gross * D('0.0380')
            sdi           = D('1.20')

        deposit = (gross - retirement - medicare - federal - state - city - sdi - socsec)

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

    return parse(preamble + ''.join(transactions))


def generate_tax_preamble(date_birth):
    return parse(replace("""
      * Tax accounts

      ;; Tax accounts not specific to a year.
      YYYY-MM-DD open Income:CC:Federal:PreTax401k     DEFCCY
      YYYY-MM-DD open Assets:CC:Federal:PreTax401k     DEFCCY

    """, {'YYYY-MM-DD': date_birth}))

def generate_tax_accounts(year):
    return parse(replace("""

      ;; Open tax accounts for that year.
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:Federal:PreTax401k   DEFCCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:Medicare             CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:Federal              CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:CityNYC              CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:SDI                  CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:StateNY              CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:SocSec               CCY

      ;; Check that the tax amounts have been fully used.
      YYYY-MM-DD balance Assets:CC:Federal:PreTax401k  0 DEFCCY

      YYYY-MM-DD * "Allowed contributions for one year"
        Income:CC:Federal:PreTax401k    -LIMIT DEFCCY
        Assets:CC:Federal:PreTax401k     LIMIT DEFCCY

    """, {'YYYY-MM-DD': datetime.date(year, 1, 1),
          'YEAR': year,
          'YYEAR': 'Y{}'.format(year),
          'LIMIT': retirement_limits[year]}))


def generate_retirement_investment(date_begin, date_end):
    retirement_string = replace("""
      YYYY-MM-DD open Assets:CC:Retirement:Cash    CCY
    """, {'YYYY-MM-DD': date_begin})

    return parse(retirement_string)


def generate_banking(date_begin, date_end):
    return parse(replace("""
      YYYY-MM-DD open Assets:CC:Bank1:Checking    CCY
      ;; YYYY-MM-DD open Assets:CC:Bank1:Savings    CCY
    """, {'YYYY-MM-DD': date_begin}))


def generate_taxable_investment(date_begin, date_end):
    return parse(replace("""
      YYYY-MM-DD open Assets:CC:Investment:Cash    CCY
    """, {'YYYY-MM-DD': date_begin}))


def generate_checking_expenses_rent(date_begin, date_end, account_checking, rent_amount):
    oss = io.StringIO()
    for dt in skipiter(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 1):
        # Have the landlord cash the check a few days after.
        date = dt.date()
        date += datetime.timedelta(days=random.randint(2, 5))

        oss.write(replace("""

          YYYY-MM-DD * "Paying the rent"
            CHECKING             -AMOUNT CCY
            Expenses:Home:Rent    AMOUNT CCY

        """, {
            'YYYY-MM-DD': date,
            'CHECKING': account_checking,
            'AMOUNT': '{:.2f}'.format(rent_amount)
        }))

    return parse(oss.getvalue())


def generate_checking_transfers(income_entries,
                                account_checking,
                                account_investment,
                                transfer_minimum,
                                transfer_threshold):
    oss = io.StringIO()
    real_root = realization.realize(income_entries)
    real_account = realization.get(real_root, account_checking)
    balance = inventory.Inventory()
    for posting in real_account.postings:
        if not isinstance(posting, data.Posting):
            continue

        balance.add_position(posting.position)

        current_amount = balance.get_amount('CCY').number
        if current_amount > (transfer_minimum + transfer_threshold):
            transfer_amount = current_amount - transfer_minimum

            oss.write(replace("""
              YYYY-MM-DD * "Transfering accumulated savings to investment account"
                CHECKING        -AMOUNT CCY
                INVESTMENT       AMOUNT CCY
            """, {
                'YYYY-MM-DD': posting.entry.date + datetime.timedelta(days=1),
                'CHECKING': account_checking,
                'INVESTMENT': account_investment,
                'AMOUNT': '{:.2f}'.format(transfer_amount)
            }))

            balance.add_amount(amount.Amount(-transfer_amount, 'CCY'))

    return parse(oss.getvalue())


def generate_expenses(date_birth):
    return parse(replace("""

      YYYY-MM-DD open Expenses:Home:Rent

    """, {'YYYY-MM-DD': date_birth}))


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    argparser = argparse.ArgumentParser(description=__doc__.strip())
    opts = argparser.parse_args()

    date_birth = datetime.date(1980, 1, 1)
    date_begin = datetime.date(2012, 1, 1)
    date_end = datetime.date(2016, 1, 1)

    # The following code entirely writes out the output to generic names, such
    # as "Employer1", "Bank1", and "CCY" (for principal currency). Those names
    # are purposely chosen to be unique, and only near the very end do we make
    # renamings to more specific and realistic names.

    # Income sources.
    annual_salary = D('120000')
    rent_divisor = D('45')
    employer, address = employers[0]
    income_entries = generate_employment_income(employer, address,
                                                annual_salary,
                                                'Assets:CC:Bank1:Checking',
                                                'Assets:CC:Retirement:Cash',
                                                date_begin, date_end)

    # Book transfers to investment account.
    rent_amount = annual_salary / rent_divisor
    banking_expenses = generate_checking_expenses_rent(
        date_begin, date_end,
        'Assets:CC:Bank1:Checking',
        rent_amount)

    banking_transfers = generate_checking_transfers(
        sorted(income_entries + banking_expenses, key=data.entry_sortkey),
        'Assets:CC:Bank1:Checking',
        'Assets:CC:Investment:Cash',
        rent_amount + D('100'),
        D('4000'))


    # Tax accounts.
    tax_preamble = generate_tax_preamble(date_birth)
    taxes = [(year, generate_tax_accounts(year))
             for year in range(date_begin.year, date_end.year)]

    # Banking accounts.
    banking_entries = generate_banking(date_begin, date_end)
    # printer.print_entries(banking_entries)

    # Investment accounts for retirement.
    retirement_entries = generate_retirement_investment(date_begin, date_end)

    # Taxable savings / investment accounts.
    investment_entries = generate_taxable_investment(date_begin, date_end)

    # Expense accounts.
    expenses_entries = generate_expenses(date_birth)

    # Format the results.
    output = io.StringIO()

    first_line = ';; -*- mode: org; mode: beancount; -*-\n'
    output.write(first_line)

    output.write('* Banking\n\n')
    banking = sorted(banking_entries + banking_transfers + banking_expenses,
                     key=data.entry_sortkey)
    printer.print_entries(banking, file=output)

    output.write('* Taxable Investments\n\n')
    printer.print_entries(investment_entries, file=output)

    output.write('* Retirement Investments\n\n')
    printer.print_entries(retirement_entries, file=output)

    output.write('* Sources of Income\n\n')
    printer.print_entries(income_entries, file=output)

    output.write('* Taxes\n\n')
    printer.print_entries(tax_preamble, file=output)
    for year, entries in taxes:
        output.write('** Tax Year {}\n\n'.format(year))
        printer.print_entries(entries, file=output)

    output.write('* Expenses \n\n')
    printer.print_entries(expenses_entries, file=output)

    # Replace generic names by realistic names.
    generic_contents = output.getvalue()

    contents = replace(generic_contents, {
        'CC': 'US',
        'CCY': 'USD',
        'VACCCY': 'VACHR',
        'DEFCCY': 'IRAUSD',
        'Bank1': 'BofA',
        'Retirement': 'Vanguard',
        })

    # Output the results.
    sys.stdout.write(contents)

    # Validate the results parse fine.
    loader.load_string(contents,
                       log_errors=sys.stderr,
                       extra_validations=validation.HARDCORE_VALIDATIONS)


## TODO(blais) - bean-format the entire output file after renamings
## TODO(blais) - Start with some non-zero opening balances in some accounts.
## TODO(blais) - Expenses in checking accounts.
## TODO(blais) - Credit card accounts, with lots of expenses (two times).
## TODO(blais) - Move this to a unit-tested location.


if __name__ == '__main__':
    main()
