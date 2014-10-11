"""Generate a decently-sized example history, based on some rules.

This script is used to generate some meaningful input to Beancount, input that
looks as realistic as possible for a moderately complex mock individual. This
can also be used as an input generator for a stress test for performance
evaluation.
"""
import argparse
import calendar
import collections
import copy
import datetime
import decimal
import functools
import io
import itertools
import logging
import math
import operator
import random
import re
import re
import sys
import textwrap
from textwrap import dedent

import dateutil
from dateutil import rrule
from dateutil.parser import parse as parse_datetime

from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount.core.amount import ONE
from beancount.core.account import join
from beancount.core import data
from beancount.core import flags
from beancount.core import amount
from beancount.core import inventory
from beancount.core import account_types
from beancount.core import realization
from beancount.parser import parser
from beancount.parser import printer
from beancount.ops import validation
from beancount.ops import prices
from beancount.scripts import format
from beancount.core import getters
from beancount.utils import misc_utils
from beancount import loader


# Constants.
ONE_DAY = datetime.timedelta(days=1)

# Annual salary.
annual_salary = D('120000')

# Annual vacation days.
annual_vacation_days = D('15')

# Divisor of the annual salary to estimate the rent.
rent_divisor = D('50')
rent_increment = D('25')


# A list of mock employers.
employers = [
    ('Hooli', "1 Carloston Rd, Mountain Beer, CA"),
    ('BayBook', "1501 Billow Rd, Benlo Park, CA"),
    ('Babble', "1 Continuous Loop, Bupertina, CA"),
    ('Hoogle', "1600 Amphibious Parkway, River View, CA"),
    ]

# Generic names of restaurants and grocery places to choose from.
restaurant_names = ["Rose Flower",
                    "Cafe Modagor",
                    "Goba Goba",
                    "Kin Soy",
                    "Uncle Boons",
                    "China Garden",
                    "Jewel of Morroco",
                    "Chichipotle"]

restaurant_narrations = ["Eating out {}".format(party_name)
                         for party_name in ["with Joe",
                                            "with Natasha",
                                            "with Bill",
                                            "with Julie",
                                            "with work buddies",
                                            "after work",
                                            "alone",
                                            ""]]

groceries_names = ["Onion Market",
                   "Good Moods Market",
                   "Corner Deli",
                   "Farmer Fresh"]

home_name = "New Metropolis"


trip_destinations = {
    "los-angeles": [
        ("Mr. Marcel", "Expenses:Food:Restaurant", (40, 25)),
        ("Banana Leaf", "Expenses:Food:Restaurant", (25, 10)),
        ("Dupar's", "Expenses:Food:Restaurant", (30, 12)),
        ("Pampas Grill", "Expenses:Food:Restaurant", (25, 10)),
        ("Chipotle", "Expenses:Food:Restaurant", (16, 5)),
        ("Starbucks", "Expenses:Food:Coffee", (6, 2)),
        ("E.B.'s Beer and Wine", "Expenses:Food:Alcohol", (9, 5)),
        ],
    "chicago": [
        ("Star of Siam", "Expenses:Food:Restaurant", (25, 10)),
        ("Mercadito", "Expenses:Food:Restaurant", (40, 15)),
        ("25 Degrees Burger Bar", "Expenses:Food:Restaurant", (22, 7)),
        ("Eataly Chicago", "Expenses:Food:Restaurant", (40, 25)),
        ("Another Sports Pub", "Expenses:Food:Alcohol", (12, 7)),
        ("Argo Tea", "Expenses:Food:Coffee", (6, 2)),
    ],
    "boston": [
        ("Giacomo's Restaurant", "Expenses:Food:Restaurant", (40, 12)),
        ("Legal Seafood", "Expenses:Food:Restaurant", (35, 15)),
        ("Franklin Cafe", "Expenses:Food:Restaurant", (30, 12)),
        ("Starbucks", "Expenses:Food:Coffee", (6, 2)),
    ],
    "new-york": [
        ("Uncle Boons", "Expenses:Food:Restaurant", (40, 15)),
        ("Cafe Select", "Expenses:Food:Restaurant", (30, 12)),
        ("Takahachi", "Expenses:Food:Restaurant", (50, 15)),
        ("Laut", "Expenses:Food:Restaurant", (32, 10)),
        ("La Colombe", "Expenses:Food:Coffee", (6, 3)),
        ("Gimme! Coffee", "Expenses:Food:Coffee", (7, 4)),
    ],
    "san-francisco": [
        ("Bar Crudo", "Expenses:Food:Restaurant", (70, 20)),
        ("Pizza Delfina", "Expenses:Food:Restaurant", (20, 6)),
        ("Waterbar", "Expenses:Food:Restaurant", (50, 20)),
        ("Mission Chinese Food", "Expenses:Food:Restaurant", (27, 12)),
        ("Starbucks", "Expenses:Food:Coffee", (6, 2)),
    ],
    }


# Limits on allowed retirement contributions.
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

file_preamble = """\
;; -*- mode: org; mode: beancount; -*-
;; Birth: {date_birth}
;; Dates: {date_begin} - {date_end}
;; THIS FILE HAS BEEN AUTO-GENERATED.
* Options

option "title" "Example Beancount file"
option "operating_currency" "CCY"

"""


def parse(input_string, **replacements):
    """Parse some input string and assert no errors.

    Args:
      input_string: Beancount input text.
      **replacements: A dict of keywords to replace to their values.
    Returns:
      A list of directive objects.
    """

    if replacements:
        import string
        class IgnoreFormatter(string.Formatter):
            def check_unused_args(self, used_args, args, kwargs):
                pass
        formatter = IgnoreFormatter()
        formatted_string = formatter.format(input_string, **replacements)
    else:
        formatted_string = input_string

    entries, errors, unused_options = parser.parse_string(formatted_string)
    if errors:
        printer.print_errors(errors, file=sys.stderr)
        raise ValueError("Parsed text has errors")

    return sorted_entries(entries)


# FIXME: This is generic; move to data.
def sorted_entries(entries):
    """Sort the entries using the entry-specific ordering.

    Args:
      entries: An unsorted list of directives.
    Returns:
      A new list of entries, sorted accoriding to date and source.
    """
    return sorted(entries, key=data.entry_sortkey)


# FIXME: This is generic; move to amount.
def round_to(number, increment):
    """Round a number *down* to a particular increment.

    Args:
      number: A Decimal, the number to be rounded.
      increment: A Decimal, the size of the increment.
    Returns:
      A Decimal, the rounded number.
    """
    return int((number / increment)) * increment


# FIXME: This is generic; move to utils.
def skipiter(iterable, num_skip):
    """Skip some elements from an iterator.

    Args:
      iterable: An iterator.
      num_skip: The number of elements in the period.
    Yields:
      Elemnets from the iterable, with num_skip elements skipped.
      For example, skipiter(range(10), 3) yields [0, 3, 6, 9].
    """
    assert num_skip > 0
    it = iter(iterable)
    while 1:
        value = next(it)
        yield value
        for _ in range(num_skip-1):
            next(it)


def date_iter(date_begin, date_end):
    """Generate a sequence of dates.

    Args:
      date_begin: The start date.
      date_end: The end date.
    Yields:
      Instances of datetime.date.
    """
    assert date_begin <= date_end
    date = date_begin
    ONE_DAY =  datetime.timedelta(days=1)
    while date < date_end:
        date += ONE_DAY
        yield date


def date_random_seq(date_begin, date_end, days_min, days_max):
    """Generate a sequence of dates with some random increase in days.

    Args:
      date_begin: The start date.
      date_end: The end date.
      days_min: The minimum number of days to advance on each iteration.
      days_max: The maximum number of days to advance on each iteration.
    Yields:
      Instances of datetime.date.
    """
    assert days_min > 0
    assert days_min <= days_max
    date = date_begin
    while date < date_end:
        nb_days_forward = random.randint(days_min, days_max)
        date += datetime.timedelta(days=nb_days_forward)
        if date >= date_end:
            break
        yield date


def delay_dates(date_iter, delay_days_min, delay_days_max):
    """Delay the dates from the given iterator by some uniformly dranw number of days.

    Args:
      date_iter: An iterator of datetime.date instances.
      delay_days_min: The minimum amount of advance days for the transaction.
      delay_days_max: The maximum amount of advance days for the transaction.
    Yields:
      datetime.date instances.
    """
    dates = list(date_iter)
    last_date = dates[-1]
    last_date = last_date.date() if isinstance(last_date, datetime.datetime) else last_date
    for dt in dates:
        date = dt.date() if isinstance(dt, datetime.datetime) else dt
        date += datetime.timedelta(days=random.randint(delay_days_min, delay_days_max))
        if date >= last_date:
            break
        yield date


def merge_postings(entries, accounts):
    """Merge all the postings from the given account names.

    Args:
      entries: A list of directives.
      accounts: A list of account strings to get the balances for.
    Yields:
      A list of postings for all the accounts, in sorted order.
    """
    real_root = realization.realize(entries)
    merged_postings = []
    for account in accounts:
        real_account = realization.get(real_root, account)
        if real_account is None:
            continue
        merged_postings.extend(posting
                               for posting in real_account.postings
                               if isinstance(posting, data.Posting))
    merged_postings.sort(key=lambda posting: posting.entry.date)
    return merged_postings


def postings_for(entries, accounts, before=False):
    """Realize the entries and get the list of postings for the given accounts.

    All the non-Posting directives are already filtered out.

    Args:
      entries: A list of directives.
      accounts: A list of account strings to get the balances for.
      before: A boolean, if true, yield the balance before the position is applied.
        The default is to yield the balance after applying the position.
    Yields:
      Tuples of:
        posting: An instance of Posting
        balances: A dict of Inventory balances for the given accounts _after_
          applying the posting. These inventory objects can be mutated to adjust
          the balance due to generated transactions to be applied later.
    """
    assert isinstance(accounts, list)
    merged_postings = merge_postings(entries, accounts)
    balances = collections.defaultdict(inventory.Inventory)
    for posting in merged_postings:
        if before:
            yield posting, balances
        balances[posting.account].add_position(posting.position)
        if not before:
            yield posting, balances


def iter_dates_with_balance(date_begin, date_end, entries, accounts):
    """Iterate over dates, including the balances of the postings iterator.

    Args:
      postings_iter: An iterator of postings as per postings_for().
      date_begin: The start date.
      date_end: The end date.
    Yields:
      Pairs of (data, balances) objects, with
        date: A datetime.date instance
        balances: An Inventory object, representing the current balance.
          You can modify the inventory object to feed back changes in the
          balance.
    """
    balances = collections.defaultdict(inventory.Inventory)
    merged_postings = iter(merge_postings(entries, accounts))
    posting = next(merged_postings, None)
    for date in date_iter(date_begin, date_end):
        while posting and posting.entry.date == date:
            balances[posting.account].add_position(posting.position)
            posting = next(merged_postings, None)
        yield date, balances


def get_minimum_balance(entries, account, currency):
    """Compute the minimum balance of the given account according to the entries history.

    Args:
      entries: A list of directives.
      account: An account string.
      currency: A currency string, for which we want to compute the minimum.
    Returns:
      A Decimal number, the minimum amount throughout the history of this account.
    """
    min_amount = ZERO
    for posting, balances in postings_for(sorted_entries(entries), [account]):
        balance = balances[account]
        current = balance.get_amount(currency).number
        if current < min_amount:
            min_amount = current
    return min_amount


def generate_employment_income(employer_name,
                               employer_address,
                               annual_salary,
                               account_deposit,
                               account_retirement,
                               date_begin,
                               date_end):
    """Generate bi-weekly entries for payroll salary income.

    Args:
      employer_name: A string, the human-readable name of the employer.
      employer_address: A string, the address of the employer.
      annual_salary: A Decimal, the annual salary of the employee.
      account_deposit: An account string, the account to deposit the salary to.
      account_retirement: An account string, the account to deposit retirement
        contributions to.
      date_begin: The start date.
      date_end: The end date.
    Returns:
      A list of directives, including open directives for the account.
    """
    preamble = parse("""

        {date_begin} event "employer" "{employer_name}, {employer_address}"

        {date_begin} open Income:CC:Employer1:Salary           CCY
        ;{date_begin} open Income:CC:Employer1:AnnualBonus     CCY
        {date_begin} open Income:CC:Employer1:GroupTermLife    CCY

        {date_begin} open Income:CC:Employer1:Vacation         VACCCY
        {date_begin} open Assets:CC:Employer1:Vacation         VACCCY

        {date_begin} open Expenses:Health:Life:GroupTermLife
        {date_begin} open Expenses:Health:Medical:Insurance
        {date_begin} open Expenses:Health:Dental:Insurance
        {date_begin} open Expenses:Health:Vision:Insurance

        ;{date_begin} open Expenses:Vacation:Employer

    """, **vars())

    date_prev = None

    contrib_retirement = ZERO
    contrib_socsec = ZERO

    retirement_per_pay = D('2000')

    biweekly_pay = annual_salary / 26
    gross = biweekly_pay

    medicare      = gross * D('0.0231')
    federal       = gross * D('0.2303')
    state         = gross * D('0.0791')
    city          = gross * D('0.0379')
    sdi           = D('1.12')

    lifeinsurance = D('24.32')
    dental        = D('2.90')
    medical       = D('27.38')
    vision        = D('42.30')

    fixed = (medicare + federal + state + city + sdi +
             dental + medical + vision)

    # Calculate vacation hours per-pay.
    with decimal.localcontext() as ctx:
        ctx.prec = 4
        vacation_hrs = (annual_vacation_days * D('8')) / D('26')

    transactions = []
    for dt in skipiter(rrule.rrule(rrule.WEEKLY, byweekday=rrule.TH,
                                   dtstart=date_begin, until=date_end), 2):
        date = dt.date()
        year = date.year

        if not date_prev or date_prev.year != date.year:
            contrib_retirement = retirement_limits.get(date.year, retirement_limits[None])
            contrib_socsec = D('7000')
        date_prev = date

        retirement_uncapped = math.ceil((gross * D('0.25')) / 100) * 100
        retirement = min(contrib_retirement, retirement_uncapped)
        contrib_retirement -= retirement

        socsec_uncapped = gross * D('0.0610')
        socsec = min(contrib_socsec, socsec_uncapped)
        contrib_socsec -= socsec

        with decimal.localcontext() as ctx:
            ctx.prec = 6
            deposit = (gross - retirement - fixed - socsec)

        template = """
            {date} * "{employer_name}" | "Payroll"
              {account_deposit}                                 {deposit:.2f} CCY
              {account_retirement}                              {retirement:.2f} CCY
              Assets:CC:Federal:PreTax401k                     -{retirement:.2f} DEFCCY
              Expenses:Taxes:Y{year}:CC:Federal:PreTax401k      {retirement:.2f} DEFCCY
              Income:CC:Employer1:Salary                       -{gross:.2f} CCY
              Income:CC:Employer1:GroupTermLife                -{lifeinsurance:.2f} CCY
              Expenses:Health:Life:GroupTermLife                {lifeinsurance:.2f} CCY
              Expenses:Health:Dental:Insurance                  {dental} CCY
              Expenses:Health:Medical:Insurance                 {medical} CCY
              Expenses:Health:Vision:Insurance                  {vision} CCY
              Expenses:Taxes:Y{year}:CC:Medicare                {medicare:.2f} CCY
              Expenses:Taxes:Y{year}:CC:Federal                 {federal:.2f} CCY
              Expenses:Taxes:Y{year}:CC:State                   {state:.2f} CCY
              Expenses:Taxes:Y{year}:CC:CityNYC                 {city:.2f} CCY
              Expenses:Taxes:Y{year}:CC:SDI                     {sdi:.2f} CCY
              Expenses:Taxes:Y{year}:CC:SocSec                  {socsec:.2f} CCY
              Assets:CC:Employer1:Vacation                      {vacation_hrs:.2f} VACCCY
              Income:CC:Employer1:Vacation                     -{vacation_hrs:.2f} VACCCY
        """
        if retirement == ZERO:
            # Remove retirement lines.
            template = '\n'.join(line
                                 for line in template.splitlines()
                                 if not re.search(r'\bretirement\b', line))

        transactions.extend(parse(template, **vars()))

    return preamble + transactions


def generate_tax_preamble(date_birth):
    """Generate tax declarations not specific to any particular year.

    Args:
      date_birth: A date instance, the birth date of the character.
    Returns:
      A list of directives.
    """
    return parse("""
      ;; Tax accounts not specific to a year.
      {date_birth} open Income:CC:Federal:PreTax401k     DEFCCY
      {date_birth} open Assets:CC:Federal:PreTax401k     DEFCCY

    """, **vars())

def generate_tax_accounts(year, date_max):
    """Generate accounts and contributino directives for a particular tax year.

    Args:
      year: An integer, the year we're to generate this for.
      date_max: The maximum date to produce an entry for.
    Returns:
      A list of directives.
    """
    date_year = datetime.date(year, 1, 1)
    date_filing = (datetime.date(year + 1, 3, 20) +
                   datetime.timedelta(days=random.randint(0, 5)))

    date_federal = (date_filing + datetime.timedelta(days=random.randint(0, 4)))
    date_state = (date_filing + datetime.timedelta(days=random.randint(0, 4)))

    amount_federal = D(max(random.normalvariate(500, 120), 12))
    amount_state = D(max(random.normalvariate(300, 100), 10))

    amount_limit = retirement_limits.get(year, retirement_limits[None])

    entries = parse("""

      ;; Open tax accounts for that year.
      {date_year} open Expenses:Taxes:Y{year}:CC:Federal:PreTax401k   DEFCCY
      {date_year} open Expenses:Taxes:Y{year}:CC:Medicare             CCY
      {date_year} open Expenses:Taxes:Y{year}:CC:Federal              CCY
      {date_year} open Expenses:Taxes:Y{year}:CC:CityNYC              CCY
      {date_year} open Expenses:Taxes:Y{year}:CC:SDI                  CCY
      {date_year} open Expenses:Taxes:Y{year}:CC:State                CCY
      {date_year} open Expenses:Taxes:Y{year}:CC:SocSec               CCY

      ;; Check that the tax amounts have been fully used.
      {date_year} balance Assets:CC:Federal:PreTax401k  0 DEFCCY

      {date_year} * "Allowed contributions for one year"
        Income:CC:Federal:PreTax401k    -{amount_limit} DEFCCY
        Assets:CC:Federal:PreTax401k     {amount_limit} DEFCCY

      {date_filing} * "Filing taxes for {year}"
        Expenses:Taxes:Y{year}:CC:Federal      {amount_federal:.2f} CCY
        Expenses:Taxes:Y{year}:CC:State        {amount_state:.2f} CCY
        Liabilities:AccountsPayable

      {date_federal} * "FEDERAL TAXPYMT"
        Assets:CC:Bank1:Checking      -{amount_federal:.2f} CCY
        Liabilities:AccountsPayable

      {date_state} * "STATE TAX & FINANC PYMT"
        Assets:CC:Bank1:Checking      -{amount_state:.2f} CCY
        Liabilities:AccountsPayable

    """, **vars())

    return [entry for entry in entries if entry.date < date_max]


def generate_retirement_employer_match(entries, account_invest, account_income):
    """Generate employer matching contributions into a retirement account.

    Args:
      entries: A list of directives that cover the retirement account.
      account_invest: The name of the retirement cash account.
      account_income: The name of the income account.
    Returns:
      A list of new entries generated for employer contributions.
    """
    MATCH_FRAC = D('0.50')

    new_entries = parse("""

      {date} open {account_income}   CCY

    """, date=entries[0].date, account_income=account_income)

    for posting, balances in postings_for(entries, [account_invest]):
        amount = posting.position.number * MATCH_FRAC
        date = posting.entry.date + ONE_DAY
        new_entries.extend(parse("""

          {date} * "Employer match for contribution"
            {account_invest}         {amount:.2f} CCY
            {account_income}        -{amount:.2f} CCY

        """, **vars()))

    return new_entries


def generate_retirement_investments(entries, account, commodities_items, price_map):
    """Invest money deposited to the given retirement account.

    Args:
      entries: A list of directives
      account: The root account for all retirement investment sub-accounts.
      commodities_items: A list of (commodity, fraction to be invested in) items.
      price_map: A dict of prices, as per beancount.ops.prices.build_price_map().
    Returns:
      A list of new directives for the given investments. This also generates account
      opening directives for the desired investment commodities.
    """
    open_entries = []
    account_cash = join(account, 'Cash')
    date_origin = entries[0].date
    open_entries.extend(parse("""
      {date_origin} open {account_cash} CCY
    """, **vars()))
    for currency, _ in commodities_items:
        open_entries.extend(parse("""
          {date_origin} open {account}:{currency} {currency}
        """, **vars()))

    new_entries = []
    for posting, balances in postings_for(entries, [account_cash]):
        balance = balances[account_cash]
        amount_to_invest = balance.get_amount('CCY').number

        # Find the date the following Monday, the date to invest.
        txn_date = posting.entry.date
        while txn_date.weekday() != calendar.MONDAY:
            txn_date += ONE_DAY

        amount_invested = ZERO
        for commodity, fraction in commodities_items:
            amount_fraction = amount_to_invest * D(fraction)

            # Find the price at that date.
            _, price = prices.get_price(price_map, (commodity, 'CCY'), txn_date)
            units = (amount_fraction / price).quantize(D('0.001'))
            amount_cash = (units * price).quantize(D('0.01'))
            new_entries.extend(parse("""

              {txn_date} * "Investing {fraction:.0%} of cash in {commodity}"
                {account}:{commodity}  {units:.3f} {commodity} {{{price:.2f} CCY}}
                {account}:Cash         -{amount_cash:.2f} CCY

            """, **vars()))

            balance.add_amount(amount.Amount(-amount_cash, 'CCY'))

    return sorted_entries(open_entries + new_entries)


def generate_banking(date_begin, date_end, amount_initial):
    """Generate a checking account opening.

    Args:
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
      amount_initial: A Decimal instance, the amount to initialize the checking account with.
    Returns:
      A list of directives.
    """
    date_balance = date_begin + datetime.timedelta(days=1)
    return parse("""

      {date_begin} open Assets:CC:Bank1:Checking    CCY
      ;; {date_begin} open Assets:CC:Bank1:Savings    CCY

      {date_begin} * "Opening Balance for checking account"
        Assets:CC:Bank1:Checking   {amount_initial} CCY
        Equity:Opening-Balances

      {date_balance} balance Assets:CC:Bank1:Checking   {amount_initial} CCY

    """, **vars())


def generate_taxable_investment(date_begin, date_end, entries, price_map, stocks):
    """Generate opening directives and transactions for an investment account.

    Args:
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
      entries: A list of entries that contains at least the transfers to the investment
        account's cash account.
      price_map: A dict of prices, as per beancount.ops.prices.build_price_map().
      stocks: A list of strings, the list of commodities to invest in.
    Returns:
      A list of directives.
    """
    account = 'Assets:CC:Investment'
    account_cash = join(account, 'Cash')
    account_gains = 'Income:CC:Investment:Gains'

    open_entries = parse("""
      {date_begin} open {account}:Cash    CCY
      {date_begin} open {account_gains}    CCY
    """, **vars())
    for stock in stocks:
        open_entries.extend(parse("""
          {date_begin} open {account}:{stock} {stock}
        """, **vars()))

    # Iterate over all the dates, but merging in the postings for the cash
    # account.
    MIN_AMOUNT = D('1000.00')
    ROUND_AMOUNT = D('100.00')
    COMMISSION = D('8.95')
    ROUND_UNITS = D('1')
    FRAC_INVEST = D('1.00')
    P_DAILY_BUY = 1./15  # days
    P_DAILY_SELL = 1./90  # days
    stocks_inventory = inventory.Inventory()
    new_entries = []
    for date, balances in iter_dates_with_balance(date_begin, date_end,
                                                  entries, [account_cash]):
        # If the balance is high, buy with high probability.
        balance = balances[account_cash]
        total_cash = balance.get_amount('CCY').number
        assert total_cash >= ZERO, ('Cash balance is negative: {}'.format(total_cash))
        invest_cash = total_cash * FRAC_INVEST - COMMISSION
        if invest_cash > MIN_AMOUNT:
            if random.random() < P_DAILY_BUY:
                commodities = random.sample(stocks, random.randint(1, len(stocks)))
                lot_amount = round_to(invest_cash / len(commodities), ROUND_AMOUNT)

                invested_amount = ZERO
                for stock in commodities:
                    # Find the price at that date.
                    _, price = prices.get_price(price_map, (stock, 'CCY'), date)

                    units = round_to((lot_amount / price), ROUND_UNITS)
                    if units <= ZERO:
                        continue
                    amount_cash = units * price + COMMISSION
                    #logging.info('Buying %s %s @ %s CCY = %s CCY', units, stock, price, units * price)

                    buy = parse("""
                      {date} * "Buy shares of {stock}"
                        {account}:Cash       -{amount_cash:.2f} CCY
                        {account}:{stock}     {units:.0f} {stock} {{{price:.2f} CCY}}
                        Expenses:Financial:Commissions   {COMMISSION:.2f} CCY
                    """, **vars())[0]
                    new_entries.append(buy)

                    balances[account_cash].add_position(buy.postings[0].position)
                    stocks_inventory.add_position(buy.postings[1].position)

                # Don't sell on days you buy.
                continue

        # Otherwise, sell with low probability.
        if not stocks_inventory.is_empty() and random.random() < P_DAILY_SELL:
            # Choose the lot with the highest gain or highest loss.
            gains = []
            for position in stocks_inventory.positions:
                base_quote = (position.lot.currency, position.lot.cost.currency)
                _, price = prices.get_price(price_map, base_quote, date)
                if price == position.lot.cost.number:
                    continue # Skip lots without movement.
                market_value = position.number * price
                book_value = position.get_cost().number
                gain = market_value - book_value
                gains.append((gain, market_value, price, position))
            if not gains:
                continue

            # Sell either biggest winner or biggest loser.
            biggest = bool(random.random() < 0.5)
            lot_tuple = sorted(gains)[0 if biggest else -1]
            gain, market_value, price, sell_position = lot_tuple
            #logging.info('Selling {} for {}'.format(sell_position, market_value))

            stock = sell_position.lot.currency
            amount_cash = market_value - COMMISSION
            sell = parse("""
              {date} * "Sell shares of {stock}"
                {account}:{stock}              -{sell_position} @ {price:.2f} CCY
                {account}:Cash                  {amount_cash:.2f} CCY
                Expenses:Financial:Commissions  {COMMISSION:.2f} CCY
                {account_gains}
            """, **vars())[0]
            new_entries.append(sell)

            balances[account_cash].add_position(sell.postings[1].position)
            stocks_inventory.add_position(sell.postings[0].position)
            continue

    return open_entries + new_entries


def generate_periodic_expenses(date_iter,
                               payee, narration,
                               account_from, account_to,
                               amount_generator):
    """Generate periodic expense transactions.

    Args:
      date_iter: An iterator for dates or datetimes.
      payee: A string, the payee name to use on the transactions, or a set of such strings
        to randomly choose from
      narration: A string, the narration to use on the transactions.
      account_from: An account string the debited account.
      account_to: An account string the credited account.
      amount_generator: A callable object to generate variates.
    Returns:
      A list of directives.
    """
    new_entries = []
    for dt in date_iter:
        date = dt.date() if isinstance(dt, datetime.datetime) else dt
        amount = D(amount_generator())
        txn_payee = (payee
                     if isinstance(payee, str)
                     else random.choice(payee))
        txn_narration = (narration
                         if isinstance(narration, str)
                         else random.choice(narration))
        new_entries.extend(parse("""
          {date} * "{txn_payee}" "{txn_narration}"
            {account_from}   -{amount:.2f} CCY
            {account_to}      {amount:.2f} CCY
        """, **vars()))

    return new_entries


def generate_clearing_entries(date_iter,
                              payee, narration,
                              entries, account_clear, account_from):
    """Generate entries to clear the value of an account.

    Args:
      date_iter: An iterator of datetime.date instances.
      payee: A string, the payee name to use on the transactions.
      narration: A string, the narration to use on the transactions.
      postings_iter: Iterator for postings and balances, as per postings_for().
      account_clear: The account to clear.
      account_from: The source account to clear 'account_clear' from.
    Returns:
      A list of directives.
    """
    # The next date we're looking for.
    next_date = next(iter(date_iter))

    # Iterate over all the postings of the account to clear.
    new_entries = []
    for posting, balances in postings_for(entries, [account_clear]):
        balance_clear = balances[account_clear]

        # Check if we need to clear.
        if next_date <= posting.entry.date:
            pos_amount = balance_clear.get_amount('CCY')
            neg_amount = -pos_amount
            new_entries.extend(parse("""
              {next_date} * "{payee}" "{narration}"
                {account_clear}     {neg_amount:.2f}
                {account_from}      {pos_amount:.2f}
            """, **vars()))
            balance_clear.add_amount(neg_amount)

            # Advance to the next date we're looking for.
            try:
                next_date = next(iter(date_iter))
            except StopIteration:
                break

    return new_entries


def generate_outgoing_transfers(entries,
                                account,
                                account_out,
                                transfer_minimum,
                                transfer_threshold,
                                transfer_increment):
    """Generate transfers of accumulated funds out of an account.

    This monitors the balance of an account and when it is beyond a threshold,
    generate out transfers form that account to another account.

    Args:
      entries: A list of existing entries that affect this account so far.
        The generated entries will also affect this account.
      account: An account string, the account to monitor.
      account_out: An account string, the savings account to make transfers to.
      transfer_minimum: The minimum amount of funds to always leave in this account
        after a transfer.
      transfer_threshold: The minimum amount of funds to be able to transfer out without
        breaking the minimum.
      transfer_increment: A Decimal, the increment to round transfers to.
    Returns:
      A list of new directives, the transfers to add to the given account.
    """
    last_date = entries[-1].date

    # Reverse the balance amounts taking into account the minimum balance for
    # all time in the future.
    amounts = [(balances[account].get_amount('CCY').number, posting)
               for posting, balances in postings_for(entries, [account])]
    reversed_amounts = []
    last_amount, _ = amounts[-1]
    for current_amount, _ in reversed(amounts):
        if current_amount < last_amount:
            reversed_amounts.append(current_amount)
            last_amount = current_amount
        else:
            reversed_amounts.append(last_amount)
    capped_amounts = reversed(reversed_amounts)

    # Create transfers outward where the future allows it.
    new_entries = []
    offset_amount = ZERO
    for current_amount, (_, posting) in zip(capped_amounts, amounts):
        if posting.entry.date >= last_date:
            break

        adjusted_amount = current_amount - offset_amount
        if adjusted_amount > (transfer_minimum + transfer_threshold):
            amount_transfer = round_to(adjusted_amount - transfer_minimum,
                                       transfer_increment)

            date = posting.entry.date + datetime.timedelta(days=1)
            new_entries.extend(parse("""
              {date} * "Transfering accumulated savings to other account"
                {account}         -{amount_transfer:2f} CCY
                {account_out}      {amount_transfer:2f} CCY
            """, **vars()))

            offset_amount += amount_transfer

    return new_entries


def generate_expense_accounts(date_birth):
    """Generate directives for expense accounts.

    Args:
      date_birth: Birth date of the character.
    Returns:
      A list of directives.
    """
    return parse("""

      {date_birth} open Expenses:Food:Groceries
      {date_birth} open Expenses:Food:Restaurant
      {date_birth} open Expenses:Food:Coffee
      {date_birth} open Expenses:Food:Alcohol

      {date_birth} open Expenses:Transport:Subway

      {date_birth} open Expenses:Home:Rent
      {date_birth} open Expenses:Home:Electricity
      {date_birth} open Expenses:Home:Internet

      {date_birth} open Expenses:Financial:Fees
      {date_birth} open Expenses:Financial:Commissions

    """, **vars())


def generate_open_entries(date, accounts, currency=None):
    """Generate a list of Open entries for the given accounts:

    Args:
      date: A datetime.date instance for the open entries.
      accounts: A list of account strings.
      currency: An optional currency constraint.
    Returns:
      A list of Open directives.
    """
    assert isinstance(accounts, (list, tuple))
    return parse(''.join(
        '{date} open {account} {currency}\n'.format(date=date,
                                                    account=account,
                                                    currency=currency or '')
        for account in accounts))


def generate_balance_checks(entries, account, date_iter):
    """Generate balance check entries to the given frequency.

    Args:
      entries: A list of directives that contain all the transactions for the
        accounts.
      account: The name of the account for which to generate.
      date_iter: Iterator of dates. We generate balance checks at these dates.
    Returns:
      A list of balance check entries.
    """
    balance_checks = []
    date_iter = iter(date_iter)
    next_date = next(date_iter)
    with misc_utils.swallow(StopIteration):
        for posting, balance in postings_for(entries, [account], before=True):
            while posting.entry.date >= next_date:
                amount = balance[account].get_amount('CCY').number
                balance_checks.extend(parse("""
                  {next_date} balance {account} {amount} CCY
                """, **vars()))
                next_date = next(date_iter)

    return balance_checks


def check_non_negative(entries, account, currency):
    """Check that the balance of the given account never goes negative.

    Args:
      entries: A list of directives.
      account: An account string, the account to check the balance for.
      currency: A string, the currency to check minimums for.
    Raises:
      AssertionError: if the balance goes negative.
    """
    previous_date = None
    for posting, balances in postings_for(sorted_entries(entries), [account], before=True):
        balance = balances[account]
        date = posting.entry.date
        if date != previous_date:
            assert all(amt.number >= ZERO for amt in balance.get_amounts()), (
                "Negative balance: {} at: {}".format(balance, posting.entry.date))
        previous_date = date


def validate_output(contents, positive_accounts, currency):
    """Check that the output file validates.

    Args:
      contents: A string, the output file.
      positive_accounts: A list of strings, account names to check for
        non-negative balances.
      currency: A string, the currency to check minimums for.
    Raises:
      AssertionError: If the output does not validate.
    """
    loaded_entries, _, _ = loader.load_string(
        contents,
        log_errors=sys.stderr,
        extra_validations=validation.HARDCORE_VALIDATIONS)

    # Sanity checks: Check that the checking balance never goes below zero.
    for account in positive_accounts:
        check_non_negative(loaded_entries, account, currency)


def generate_banking_expenses(date_begin, date_end, account, rent_amount):
    """Generate expenses paid out of a checking account, typically living expenses.

    Args:
      date_begin: The start date.
      date_end: The end date.
      account: The checking account to generate expenses to.
      rent_amount: The amount of rent.
    Returns:
      A list of directives.
    """
    fee_expenses = generate_periodic_expenses(
        rrule.rrule(rrule.MONTHLY, bymonthday=4, dtstart=date_begin, until=date_end),
        "BANK FEES", "Monthly bank fee",
        account, 'Expenses:Financial:Fees',
        lambda: D('4.00'))

    rent_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 2, 5),
        "RiverBank Properties", "Paying the rent",
        account, 'Expenses:Home:Rent',
        lambda: random.normalvariate(float(rent_amount), 0))

    electricity_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 7, 8),
        "EDISON POWER", "",
        account, 'Expenses:Home:Electricity',
        lambda: random.normalvariate(65, 0))

    internet_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 20, 22),
        "Wine-Tarner Cable", "",
        account, 'Expenses:Home:Internet',
        lambda: random.normalvariate(80, 0.10))

    return sorted_entries(fee_expenses +
                          rent_expenses +
                          electricity_expenses +
                          internet_expenses)


def generate_regular_credit_expenses(date_birth, date_begin, date_end,
                                     account_credit,
                                     account_checking):
    """Generate expenses paid out of a credit card account, including payments to the
    credit card.

    Args:
      date_birth: The user's birth date.
      date_begin: The start date.
      date_end: The end date.
      account_credit: The credit card account to generate expenses against.
      account_checking: The checking account to generate payments from.
    Returns:
      A list of directives.
    """
    restaurant_expenses = generate_periodic_expenses(
        date_random_seq(date_begin, date_end, 1, 5),
        restaurant_names, restaurant_narrations,
        account_credit, 'Expenses:Food:Restaurant',
        lambda: min(random.lognormvariate(math.log(30), math.log(1.5)),
                    random.randint(200, 220)))

    groceries_expenses = generate_periodic_expenses(
        date_random_seq(date_begin, date_end, 5, 20),
        groceries_names, "Buying groceries",
        account_credit, 'Expenses:Food:Groceries',
        lambda: min(random.lognormvariate(math.log(80), math.log(1.3)),
                    random.randint(250, 300)))

    subway_expenses = generate_periodic_expenses(
        date_random_seq(date_begin, date_end, 27, 33),
        "Metro Transport", "Subway tickets",
        account_credit, 'Expenses:Transport:Subway',
        lambda: D('120.00'))

    credit_expenses = sorted_entries(restaurant_expenses +
                                     groceries_expenses +
                                     subway_expenses)

    # Entries to open accounts.
    credit_preamble = generate_open_entries(date_birth, [account_credit], 'CCY')

    return sorted_entries(credit_preamble + credit_expenses)


def compute_trip_dates(date_begin, date_end):
    """Generate dates at reasonable intervals for trips during the given time period.

    Args:
      date_begin: The start date.
      date_end: The end date.
    Yields:
      Pairs of dates for the trips within the period.
    """
    # Min and max number of days remaining at home.
    DAYS_AT_HOME = (4*30, 13*30)

    # Length of trip.
    DAYS_TRIP = (8, 22)

    # Number of days to ensure no trip at the beginning and the ned.
    DAYS_BUFFER = 21

    date_begin += datetime.timedelta(days=DAYS_BUFFER)
    date_end -= datetime.timedelta(days=DAYS_BUFFER)

    date = date_begin
    while 1:
        duration_at_home = datetime.timedelta(days=random.randint(*DAYS_AT_HOME))
        duration_trip = datetime.timedelta(days=random.randint(*DAYS_TRIP))
        date_trip_begin = date + duration_at_home
        date_trip_end = date_trip_begin + duration_trip
        if date_trip_end >= date_end:
            break
        yield (date_trip_begin, date_trip_end)
        date = date_trip_end


def generate_trip_entries(date_begin, date_end,
                          tag, config,
                          trip_city, home_city,
                          account_credit):
    """Generate more dense expenses for a trip.

    Args:
      date_begin: A datetime.date instance, the beginning of the trip.
      date_end: A datetime.date instance, the end of the trip.
      tag: A string, the name of the tag.
      config: A list of (payee name, account name, (mu, 3sigma)), where
        mu is the mean of the prices to generate and 3sigma is 3 times
        the standard deviation.
      trip_city: A string, the capitalized name of the destination city.
      home_city: A string, the name of the home city.
      account_credit: A string, the name of the credit card account to pay
        the expenses from.
    Returns:
      A list of entries for the trip, all tagged with the given tag.
    """
    P_DAY_GENERATE = 0.3
    new_entries = []
    for date in date_iter(date_begin, date_end):
        for payee, account_expense, (mu, sigma3) in config:
            if random.random() < P_DAY_GENERATE:
                amount = random.normalvariate(mu, sigma3 / 3.)
                neg_amount = -amount
                new_entries.extend(parse("""
                  {date} * "{payee}" "" #{tag}
                    {account_credit}     {neg_amount} CCY
                    {account_expense}    {amount} CCY
                """, **vars()))

    # Generate events for the trip.
    new_entries.extend(parse("""
      {date_begin} event "location" "{trip_city}"
      {date_end}   event "location" "{home_city}"
    """, **vars()))

    return new_entries


def price_series(start, mu, sigma):
    """Generate a price series based on a simple stochastic model.

    Args:
      start: The beginning value.
      mu: The per-step drift, in units of value.
      sigma: Volatility of the changes.
    Yields:
      Floats, at each step.
    """
    value = start
    while 1:
        yield value
        value += random.normalvariate(mu, sigma) * value


def generate_prices(date_begin, date_end, currencies, cost_currency):
    """Generate weekly or monthyl price entries for the given currencies.

    Args:
      date_begin: The start date.
      date_end: The end date.
      currencies: A list of currency strings to generate prices for.
      cost_currency: A string, the cost currency.
    Returns:
      A list of Price directives.
    """
    digits = D('0.01')
    entries = []
    counter = itertools.count()
    for currency in currencies:
        start_price = random.uniform(30, 200)
        growth = random.uniform(0.02, 0.13) # %/year
        mu = growth * (7 / 365)
        sigma = random.uniform(0.005, 0.02) # Vol

        for dt, price_float in zip(rrule.rrule(rrule.WEEKLY, byweekday=rrule.FR,
                                               dtstart=date_begin, until=date_end),
                             price_series(start_price, mu, sigma)):
            price = D(price_float).quantize(digits)
            source = data.Source(generate_prices.__name__, next(counter))
            entry = data.Price(source, dt.date(), currency,
                               amount.Amount(price, cost_currency))
            entries.append(entry)
    return entries


def replace(string, replacements, strip=False):
    """Apply word-boundaried regular expression replacements to an indented string.

    Args:
      string: Some input template string.
      replacements: A dict of regexp to replacement value.
      strip: A boolean, true if we should strip the input.
    Returns:
      The input string with the replacements applied to it, with the indentation removed.
    """
    output = dedent(string)
    if strip:
        output = output.strip()
    for from_, to_ in replacements.items():
        if not isinstance(to_, str) and not callable(to_):
            to_ = str(to_)
        output = re.sub(r'\b{}\b'.format(from_), to_, output)
    return output


def contextualize_file(contents, employer):
    """Replace generic strings in the generated file with realistic strings.

    Args:
      contents: A string, the generic file contents.
    Returns:
      A string, the contextualized version.
    """
    replacements = {
        'CC': 'US',
        'CCY': 'USD',
        'VACCCY': 'VACHR',
        'DEFCCY': 'IRAUSD',
        'Bank1': 'BofA',
        'CreditCard1': 'Chase:Slate',
        'CreditCard2': 'Amex:BlueCash',
        'Employer1': employer,
        'Retirement': 'Vanguard',
        'FUND1': 'VBMPX',
        'FUND2': 'RGAGX',
        }
    new_contents = replace(contents, replacements)
    return format.align_beancount(new_contents), replacements


def write_example_file(date_birth, date_begin, date_end, file):
    """Generate the example file.

    Args:
      date_birth: A datetime.date instance, the birth date of our character.
      date_begin: A datetime.date instance, the beginning date at which to generate
        transactions.
      date_end: A datetime.date instance, the end date at which to generate
        transactions.
      file: A file object, where to write out the output.
    """
    # The following code entirely writes out the output to generic names, such
    # as "Employer1", "Bank1", and "CCY" (for principal currency). Those names
    # are purposely chosen to be unique, and only near the very end do we make
    # renamings to more specific and realistic names.

    # Name of the checking account.
    account_opening = 'Equity:Opening-Balances'
    account_payable = 'Liabilities:AccountsPayable'
    account_checking = 'Assets:CC:Bank1:Checking'
    account_credit = 'Liabilities:CC:CreditCard1'
    account_retirement = 'Assets:CC:Retirement'
    account_investing = 'Assets:CC:Investment:Cash'

    # Estimate the rent.
    rent_amount = round_to(annual_salary / rent_divisor, rent_increment)

    # Get a random employer.
    employer_name, employer_address = random.choice(employers)

    logging.info("Generating Salary Employment Income")
    income_entries = generate_employment_income(employer_name, employer_address,
                                                annual_salary,
                                                account_checking,
                                                join(account_retirement, 'Cash'),
                                                date_begin, date_end)

    logging.info("Generating Expenses from Banking Accounts")
    banking_expenses = generate_banking_expenses(date_begin, date_end,
                                                 account_checking, rent_amount)

    logging.info("Generating Regular Expenses via Credit Card")
    credit_regular_entries = generate_regular_credit_expenses(
        date_birth, date_begin, date_end, account_credit, account_checking)

    logging.info("Generating Credit Card Expenses for Trips")
    trip_entries = []
    destinations = sorted(trip_destinations.items())
    destinations.extend(destinations)
    random.shuffle(destinations)
    for (date_trip_begin, date_trip_end), (destination_name, config) in zip(
        compute_trip_dates(date_begin, date_end), destinations):

        # Compute a suitable tag.
        tag = 'trip-{}-{}'.format(destination_name.lower().replace(' ', '-'),
                                  date_trip_begin.year)
        #logging.info("%s -- %s %s", tag, date_trip_begin, date_trip_end)

        # Remove regular entries during this trip.
        credit_regular_entries = [entry
                                  for entry in credit_regular_entries
                                  if not(date_trip_begin <= entry.date < date_trip_end)]

        # Generate entries for the trip.
        this_trip_entries = generate_trip_entries(
            date_trip_begin, date_trip_end,
            tag, config,
            destination_name.replace('-', ' ').title(), home_name,
            account_credit)

        trip_entries.extend(this_trip_entries)

    logging.info("Generating Credit Card Payment Entries")
    credit_payments = generate_clearing_entries(
        delay_dates(rrule.rrule(rrule.MONTHLY,
                                dtstart=date_begin, until=date_end, bymonthday=7), 0, 4),
        "CreditCard1", "Paying off credit card",
        credit_regular_entries,
        account_credit, account_checking)

    credit_entries = credit_regular_entries + trip_entries + credit_payments

    logging.info("Generating Tax Filings and Payments")
    tax_preamble = generate_tax_preamble(date_birth)

    # Figure out all the years we need tax accounts for.
    years = set()
    for account_name in getters.get_accounts(income_entries):
        match = re.match('Expenses:Taxes:Y(\d\d\d\d)', account_name)
        if match:
            years.add(int(match.group(1)))

    taxes = [(year, generate_tax_accounts(year, date_end)) for year in sorted(years)]
    tax_entries = tax_preamble + functools.reduce(operator.add,
                                                  (entries
                                                   for _, entries in taxes))

    logging.info("Generating Opening of Banking Accounts")
    # Open banking accounts and gift the checking account with a balance that
    # will offset all the amounts to ensure a positive balance throughout its
    # lifetime.
    minimum = get_minimum_balance(
        sorted_entries(income_entries +
                       banking_expenses +
                       credit_entries +
                       tax_entries),
        account_checking, 'CCY')
    banking_entries = generate_banking(date_begin, date_end, max(-minimum, ZERO))

    logging.info("Generating Transfers to Investment Account")
    banking_transfers = generate_outgoing_transfers(
        sorted_entries(income_entries +
                       banking_entries +
                       banking_expenses +
                       credit_entries +
                       tax_entries),
        account_checking,
        account_investing,
        transfer_minimum=D('200'),
        transfer_threshold=D('3000'),
        transfer_increment=D('500'))

    logging.info("Generating Prices")
    # Generate price entries for investment currencies and create a price map to
    # use for later for generating investment transactions.
    funds_allocation = {'FUND1': 0.40, 'FUND2': 0.60}
    stocks = ['STK1', 'STK2', 'STK3', 'STK4']
    price_entries = generate_prices(date_begin, date_end,
                                    sorted(funds_allocation.keys()) + stocks, 'CCY')
    price_map = prices.build_price_map(price_entries)

    logging.info("Generating Employer Match Contribution")
    account_match = 'Income:US:Employer1:Match401k'
    retirement_match = generate_retirement_employer_match(income_entries,
                                                          join(account_retirement, 'Cash'),
                                                          account_match)

    logging.info("Generating Retirement Investments")
    retirement_entries = generate_retirement_investments(
        income_entries + retirement_match, account_retirement,
        sorted(funds_allocation.items()),
        price_map)

    logging.info("Generating Taxes Investments")
    investment_entries = generate_taxable_investment(date_begin, date_end,
                                                     banking_transfers, price_map,
                                                     stocks)

    logging.info("Generating Expense Accounts")
    expense_accounts_entries = generate_expense_accounts(date_birth)

    logging.info("Generating Equity Accounts")
    equity_entries = generate_open_entries(date_birth, [account_opening,
                                                        account_payable])

    logging.info("Generating Balance Checks")
    credit_checks = generate_balance_checks(credit_entries, account_credit,
                                            date_random_seq(date_begin, date_end, 20, 30))

    banking_checks = generate_balance_checks(sorted_entries(income_entries +
                                                            banking_entries +
                                                            banking_expenses +
                                                            banking_transfers +
                                                            credit_entries +
                                                            tax_entries),
                                             account_checking,
                                             date_random_seq(date_begin, date_end, 20, 30))

    logging.info("Outputting and Formatting Entries")
    output = io.StringIO()
    def output_section(title, entries):
        output.write('{}\n\n'.format(title))
        printer.print_entries(sorted_entries(entries), file=output)

    output.write(file_preamble.format(**vars()))
    output_section('* Equity Accounts', equity_entries)
    output_section('* Banking', sorted_entries(banking_entries +
                                               banking_expenses +
                                               banking_transfers +
                                               banking_checks))
    output_section('* Credit-Cards', sorted_entries(credit_entries +
                                                    credit_checks))
    output_section('* Taxable Investments', investment_entries)
    output_section('* Retirement Investments', sorted_entries(retirement_entries +
                                                              retirement_match))
    output_section('* Sources of Income', income_entries)
    output_section('* Taxes', tax_preamble)
    for year, entries in taxes:
        output_section('** Tax Year {}'.format(year), entries)
    output_section('* Expenses', expense_accounts_entries)
    output_section('* Prices', price_entries)
    output_section('* Cash', [])  # FIXME TODO(blais): cash entries

    logging.info("Contextualizing to Realistic Names")
    contents, replacements = contextualize_file(output.getvalue(), employer_name)
    file.write(contents)

    logging.info("Validating Results")
    validate_output(contents,
                    [replace(account, replacements)
                     for account in [account_checking]],
                    replace('CCY', replacements))


def main():
    parse_date = lambda s: parse_datetime(s).date()
    today = datetime.date.today()

    argparser = argparse.ArgumentParser(description=__doc__.strip())

    default_years = 2
    argparser.add_argument('--date-begin', '--begin-date', action='store', type=parse_date,
                           default=datetime.date(today.year - default_years, 1, 1),
                           help="Beginning date")

    argparser.add_argument('--date-end', '--end-date', action='store', type=parse_date,
                           default=today,
                           help="End date.")

    argparser.add_argument('--date-birth', '--birth-date', action='store', type=parse_date,
                           default=datetime.date(1980, 5, 12),
                           help="Date of birth of our fictional character.")

    argparser.add_argument('-s', '--seed', action='store', type=int,
                           help="Fix the random seed for debugging.")

    argparser.add_argument('-o', '--output', action='store',
                           help="Output filename (default stdout)")

    opts = argparser.parse_args()

    logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
    if opts.seed is not None:
        logging.info("Seed = %s", opts.seed)
        random.seed(opts.seed)

    output_file = open(opts.output, 'w') if opts.output else sys.stdout
    write_example_file(opts.date_birth,
                       opts.date_begin,
                       opts.date_end,
                       file=output_file)

    return 0
