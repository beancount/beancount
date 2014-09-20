#!/usr/bin/env python3
"""
Generate a decently-sized example file, based on some hard-coded rules.
"""
import argparse
import calendar
import datetime
import itertools
import logging
import random

import dateutil
from dateutil import rrule

from beancount.core import data
from beancount.core import flags
from beancount.core import account_types
from beancount.core.account import join
from beancount.parser import parser
from beancount.parser import printer


A = account_types.DEFAULT_ACCOUNT_TYPES

source_filename = 'script://gen-example'
source = data.Source(source_filename, 0)

employers = {
    'Hooli': "1 Carloston Rd, Mountain Beer, CA",
    'BayBook': "1501 Billow Rd, Benlo Park, CA",
    }

country = 'US'
currency = 'USD'
taxdef_currency = 'IRAUSD'
vacation_currency = 'VACHR'

bank = 'BofA'
checking = 'Checking'
savings = 'Savings'


def parse(string):
    entries, errors, unused_options = parser.parse_string(string)
    assert not errors
    return entries


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


def generate_employment_income(employer, address, date_begin, date_end):
    open_entries = parse("""

        {date_begin} event "{employer}" "{address}"

        {date_begin} open Income:{country}:{employer}:Salary           {currency}
        {date_begin} open Income:{country}:{employer}:SignOnBonus      {currency}
        {date_begin} open Income:{country}:{employer}:AnnualBonus      {currency}
        {date_begin} open Income:{country}:{employer}:Match401k        {currency}
        {date_begin} open Income:{country}:{employer}:GroupTermLife    {currency}
        {date_begin} open Income:{country}:{employer}:HolidayGift      {currency}
        {date_begin} open Income:{country}:{employer}:GymReimbursement {currency}

        {date_begin} open Income:{country}:{employer}:Vacation {vacation_currency}
        {date_begin} open Assets:{country}:{employer}:Vacation {vacation_currency}

        {date_begin} open Expenses:Health:Life:GroupTermLife
        {date_begin} open Expenses:Health:Medical:Insurance
        {date_begin} open Expenses:Health:Dental:Insurance
        {date_begin} open Expenses:Health:Vision:Insurance
        {date_begin} open Expenses:Communications:Internet:Reimbursement
        {date_begin} open Expenses:Transportation:PublicTrans:TransitPreTax
        {date_begin} open Expenses:Vacation:{employer}

    """.format(country=country,
               currency=currency,
               vacation_currency=vacation_currency,
               **vars()))

    transactions = []
    for dt in skipiter(rrule.rrule(rrule.WEEKLY, byweekday=rrule.TH,
                               dtstart=date_begin, until=date_end), 1):
        date = dt.date()
        transactions.extend(parse("""

            {date} * "{employer}" | "Payroll"
              Assets:US:TD:Checking                         XXXX.XX {currency}
              Assets:US:Vanguard:Cash                       XXXX.XX {currency}
              Assets:US:Federal:PreTax401k                  XXXX.XX {taxdef_currency}
              Expenses:Taxes:TY2013:US:Federal:PreTax401k   XXXX.XX {taxdef_currency}
              Income:US:Google:Salary                      -XXXX.XX {currency}
              Income:US:Google:GroupTermLife                XXXX.XX {currency}
              Expenses:Health:Life:GroupTermLife:Google     XXXX.XX {currency}
              Expenses:Taxes:TY2013:US:Medicare:Google      XXXX.XX {currency}
              Expenses:Taxes:TY2013:US:Federal:Google       XXXX.XX {currency}
              Expenses:Taxes:TY2013:US:CityNYC:Google       XXXX.XX {currency}
              Expenses:Taxes:TY2013:US:SDI:Google           XXXX.XX {currency}
              Expenses:Taxes:TY2013:US:StateNY:Google       XXXX.XX {currency}
              Expenses:Taxes:TY2013:US:SocSec:Google        XXXX.XX {currency}

        """.format(currency=currency,
                   taxdef_currency=taxdef_currency,
                   **vars())))

    return open_entries + transactions








def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    opts = parser.parse_args()

    date_begin = datetime.date(2012, 1, 1)
    date_end = datetime.date(2016, 1, 1)

    employer, address = next(iter(employers.items()))
    income_entries = generate_employment_income(employer, address, date_begin, date_end)

    printer.print_entries(income_entries)


if __name__ == '__main__':
    main()
