"""Generate a decently-sized example history, based on some rules.

This script is used to generate some meaningful input to Beancount, input that
looks as realistic as possible for a moderately complex mock individual. This
can also be used as an input generator for a stress test for performance
evaluation.
"""
import argparse
import calendar
import copy
import collections
import datetime
import decimal
import functools
import io
import itertools
import logging
import operator
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
from beancount.scripts import format
from beancount import loader


# Constants.
ONE_DAY = datetime.timedelta(days=1)

# Date of birth of our fictional character.
date_birth = datetime.date(1980, 1, 1)

# Default begin and end dates for the generation of data.
date_begin = datetime.date(2012, 1, 1)
date_end = datetime.date(2016, 1, 1)

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
;; THIS FILE HAS BEEN AUTO-GENERATED.
* Options

option "title" "Example Beancount file"
option "operating_currency" "CCY"

"""


def parse(string):
    """Parse some input string and assert no errors.

    Args:
      string: Beancount input text.
    Returns:
      A list of directive objects.
    """
    entries, errors, unused_options = parser.parse_string(string)
    if errors:
        printer.print_errors(errors, file=sys.stderr)
        raise ValueError("Parsed text has errors")
    return entries


def sorted_entries(entries):
    """Sort the entries using the entry-specific ordering.

    Args:
      entries: An unsorted list of directives.
    Returns:
      A new list of entries, sorted accoriding to date and source.
    """
    return sorted(entries, key=data.entry_sortkey)


# FIXME: This is generic; move to utils.
def round_to(number, increment):
    """Round a number down to a particular increment.

    Args:
      number: A Decimal, the number to be rounded.
      increment: A Decimal, the size of the increment.
    Returns:
      A Decimal, the rounded number.
    """
    return int((number / increment)) * increment


# FIXME: This is generic; move to utils.
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

def date_seq(date_begin, date_end, days_min, days_max):
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


def postings_for(entries, accounts):
    """Realize the entries and get the list of postings for the given accounts.

    All the non-Posting directives are already filtered out.

    Args:
      entries: A list of directives.
      accounts: A list of account strings to get the balances for.
    Yields:
      Tuples of:
        posting: An instance of Posting
        balances: A dict of Inventory balances for the given accounts _after_
          applying the posting. These inventory objects can be mutated to adjust
          the balance due to generated transactions to be applied later.
    """
    real_root = realization.realize(entries)
    merged_postings = []
    for account in accounts:
        real_account = realization.get(real_root, account)
        merged_postings.extend(posting
                               for posting in real_account.postings
                               if isinstance(posting, data.Posting))
    merged_postings.sort(key=lambda posting: posting.entry.date)

    balances = collections.defaultdict(inventory.Inventory)
    for posting in merged_postings:
        balances[posting.account].add_position(posting.position)
        yield posting, balances


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


def generate_employment_income(employer,
                               address,
                               annual_salary,
                               account_deposit,
                               account_retirement,
                               date_begin,
                               date_end):
    """Generate bi-weekly entries for payroll salary income.

    Args:
      employer: A string, the human-readable name of the employer.
      address: A string, the address of the employer.
      annual_salary: A Decimal, the annual salary of the employee.
      account_deposit: An account string, the account to deposit the salary to.
      account_retirement: An account string, the account to deposit retirement
        contributions to.
      date_begin: The start date.
      date_end: The end date.
    Returns:
      A list of directives, including open directives for the account.
    """
    replacements = {
        'YYYY-MM-DD': date_begin,
        'Employer': employer,
        'Address': address,
        }

    preamble = replace("""

        YYYY-MM-DD event "employer" "Employer, Address"

        YYYY-MM-DD open Income:CC:Employer1:Salary           CCY
        ;YYYY-MM-DD open Income:CC:Employer1:AnnualBonus      CCY
        YYYY-MM-DD open Income:CC:Employer1:GroupTermLife    CCY

        YYYY-MM-DD open Income:CC:Employer1:Vacation         VACCCY
        YYYY-MM-DD open Assets:CC:Employer1:Vacation         VACCCY

        YYYY-MM-DD open Expenses:Health:Life:GroupTermLife
        YYYY-MM-DD open Expenses:Health:Medical:Insurance
        YYYY-MM-DD open Expenses:Health:Dental:Insurance
        YYYY-MM-DD open Expenses:Health:Vision:Insurance

        ;YYYY-MM-DD open Expenses:Vacation:Employer

    """, replacements)

    replacements['Deposit'] = account_deposit
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
        replacements['YYYY-MM-DD'] = date
        replacements['Year'] = 'Y{}'.format(date.year)

        if not date_prev or date_prev.year != date.year:
            contrib_retirement = retirement_limits[date.year]
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
            YYYY-MM-DD * "Employer" | "Payroll"
              Deposit                                           {deposit:.2f} CCY
              Retirement                                        {retirement:.2f} CCY
              Assets:CC:Federal:PreTax401k                     -{retirement:.2f} DEFCCY
              Expenses:Taxes:Year:CC:Federal:PreTax401k         {retirement:.2f} DEFCCY
              Income:CC:Employer1:Salary                       -{gross:.2f} CCY
              Income:CC:Employer1:GroupTermLife                -{lifeinsurance:.2f} CCY
              Expenses:Health:Life:GroupTermLife                {lifeinsurance:.2f} CCY
              Expenses:Health:Dental:Insurance                  {dental} CCY
              Expenses:Health:Medical:Insurance                 {medical} CCY
              Expenses:Health:Vision:Insurance                  {vision} CCY
              Expenses:Taxes:Year:CC:Medicare                   {medicare:.2f} CCY
              Expenses:Taxes:Year:CC:Federal                    {federal:.2f} CCY
              Expenses:Taxes:Year:CC:State                      {state:.2f} CCY
              Expenses:Taxes:Year:CC:CityNYC                    {city:.2f} CCY
              Expenses:Taxes:Year:CC:SDI                        {sdi:.2f} CCY
              Expenses:Taxes:Year:CC:SocSec                     {socsec:.2f} CCY
              Assets:CC:Employer1:Vacation                      {vacation_hrs:.2f} VACCCY
              Income:CC:Employer1:Vacation                     -{vacation_hrs:.2f} VACCCY
        """
        if retirement == ZERO:
            # Remove retirement lines.
            template = '\n'.join(line
                                 for line in template.splitlines()
                                 if not re.search(r'\bretirement\b', line))

        txn = replace(template.format(**vars()), replacements)
        txn = re.sub(r'({[0-9.]+})', replace_amount, txn)
        transactions.append(txn)

    return parse(preamble + ''.join(transactions))


def generate_tax_preamble(date_birth):
    """Generate tax declarations not specific to any particular year.

    Args:
      date_birth: A date instance, the birth date of the character.
    Returns:
      A list of directives.
    """
    return parse(replace("""
      * Tax accounts

      ;; Tax accounts not specific to a year.
      YYYY-MM-DD open Income:CC:Federal:PreTax401k     DEFCCY
      YYYY-MM-DD open Assets:CC:Federal:PreTax401k     DEFCCY

    """, {'YYYY-MM-DD': date_birth}))

def generate_tax_accounts(year):
    """Generate accounts and contributino directives for a particular tax year.

    Args:
      year: An integer, the year we're to generate this for.
    Returns:
      A list of directives.
    """
    date_filing = (datetime.date(year, 3, 20) +
                   datetime.timedelta(days=random.randint(0, 5)))

    date_federal = (date_filing + datetime.timedelta(days=random.randint(0, 4)))
    date_state = (date_filing + datetime.timedelta(days=random.randint(0, 4)))

    amount_federal = D(max(random.normalvariate(500, 120), 12))
    amount_state = D(max(random.normalvariate(300, 100), 10))

    return parse(replace("""

      ;; Open tax accounts for that year.
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:Federal:PreTax401k   DEFCCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:Medicare             CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:Federal              CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:CityNYC              CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:SDI                  CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:State                CCY
      YYYY-MM-DD open Expenses:Taxes:YYEAR:CC:SocSec               CCY

      ;; Check that the tax amounts have been fully used.
      YYYY-MM-DD balance Assets:CC:Federal:PreTax401k  0 DEFCCY

      YYYY-MM-DD * "Allowed contributions for one year"
        Income:CC:Federal:PreTax401k    -LIMIT DEFCCY
        Assets:CC:Federal:PreTax401k     LIMIT DEFCCY

      YYYY-MM-EE * "Filing taxes for YEAR"
        Expenses:Taxes:YYEAR:CC:Federal      FEDERAL_AMT CCY
        Expenses:Taxes:YYEAR:CC:State        STATE_AMT CCY
        Liabilities:AccountsPayable

      YYYY-MM-FF * "FEDERAL TAXPYMT"
        Assets:CC:Bank1:Checking      -FEDERAL_AMT CCY
        Liabilities:AccountsPayable

      YYYY-MM-GG * "STATE TAX & FINANC PYMT"
        Assets:CC:Bank1:Checking      -STATE_AMT CCY
        Liabilities:AccountsPayable

    """, {'YYYY-MM-DD': datetime.date(year, 1, 1),
          'YYYY-MM-EE': date_filing,
          'YYYY-MM-FF': date_federal,
          'YYYY-MM-GG': date_state,
          'FEDERAL_AMT': '{:.2f}'.format(amount_federal),
          'STATE_AMT': '{:.2f}'.format(amount_state),
          'YEAR': year,
          'YYEAR': 'Y{}'.format(year),
          'LIMIT': retirement_limits[year]}))


def generate_retirement_investments(entries, account, commodities_map):
    """Invest money deposited to the given retirement account.

    Args:
      entries: A list of directives
      account: The checking account to generate expenses to.
      commodities_map: A dict of commodity/currency to a fraction to be invested in.
    Returns:
      A list of new directives for the given investments. This also generates account
      opening directives for the desired investment commodities.
    """
    oss = io.StringIO()
    account_cash = join(account, 'Cash')
    oss.write("{0} open {1} CCY\n".format(entries[0].date, account_cash))
    for currency in commodities_map.keys():
        oss.write("{0} open {1} {2}\n".format(entries[0].date,
                                              join(account, currency),
                                              currency))
    open_entries = parse(oss.getvalue())

    oss = io.StringIO()
    for posting, balances in postings_for(entries, [account_cash]):
        balance = balances[account]
        next_date = posting.entry.date + ONE_DAY

        # oss.write(replace("""

        #   YYYY-MM-DD * "Investing cash"
        #     ACCOUNT:COMM    UNITS COMM {COST CCY}
        #     ACCOUNT:Cash    CASH_AMOUNT CCY

        # """, {'YYYY-MM-DD': date_begin,
        #       'COMM': 'RETINV1',
        #       'UNITS': '{:.2f}'.format(D('1')),
        #       'COST': '{:.2f}'.format(D('1')),
        #   }))

    return open_entries


def generate_banking(date_begin, date_end, initial_amount):
    """Generate a checking account opening.

    Args:
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
      initial_amount: A Decimal instance, the amount to initialize the checking account with.
    Returns:
      A list of directives.
    """
    return parse(replace("""
      YYYY-MM-DD open Assets:CC:Bank1:Checking    CCY
      ;; YYYY-MM-DD open Assets:CC:Bank1:Savings    CCY

      YYYY-MM-DD * "Opening Balance for checking account"
        Assets:CC:Bank1:Checking   INIT CCY
        Equity:Opening-Balances

      YYYY-MM-EE balance Assets:CC:Bank1:Checking   INIT CCY

    """, {'YYYY-MM-EE': date_begin + datetime.timedelta(days=1),
          'YYYY-MM-DD': date_begin,
          'INIT': initial_amount}))


def generate_taxable_investment(date_begin, date_end):
    """Generate opening directives and transactions for an investment account.

    Args:
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
    Returns:
      A list of directives.
    """
    return parse(replace("""
      YYYY-MM-DD open Assets:CC:Investment:Cash    CCY
    """, {'YYYY-MM-DD': date_begin}))


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
    oss = io.StringIO()
    for dt in date_iter:
        date = dt.date() if isinstance(dt, datetime.datetime) else dt
        txn_amount = D(amount_generator())
        oss.write(replace("""

          YYYY-MM-DD * "Payee" "Narration"
            Account:From    -AMOUNT CCY
            Account:To       AMOUNT CCY

        """, {
            'YYYY-MM-DD': date,
            'Payee': (payee
                      if isinstance(payee, str)
                      else random.choice(payee)),
            'Narration': (narration
                          if isinstance(narration, str)
                          else random.choice(narration)),
            'Account:From': account_from,
            'Account:To': account_to,
            'AMOUNT': '{:.2f}'.format(txn_amount)
        }))

    return parse(oss.getvalue())


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
    oss = io.StringIO()
    for posting, balances in postings_for(entries, [account_clear]):
        balance_clear = balances[account_clear]

        # Check if we need to clear.
        if next_date <= posting.entry.date:
            balance_amount = balance_clear.get_amount('CCY')
            oss.write(replace("""

              YYYY-MM-DD * "Payee" "Narration"
                Account:Clear    POS_AMOUNT
                Account:From     NEG_AMOUNT

            """, {
                'YYYY-MM-DD': next_date,
                'Payee': payee,
                'Narration': narration,
                'Account:Clear': account_clear,
                'Account:From': account_from,
                'POS_AMOUNT': '{:.2f}'.format(-balance_amount),
                'NEG_AMOUNT': '{:.2f}'.format(balance_amount)
            }))
            balance_clear.add_amount(-balance_amount)

            # Advance to the next date we're looking for.
            try:
                next_date = next(iter(date_iter))
            except StopIteration:
                break

    return parse(oss.getvalue())


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
    oss = io.StringIO()
    offset_amount = ZERO
    for current_amount, (_, posting) in zip(capped_amounts, amounts):
        if posting.entry.date >= last_date:
            break

        adjusted_amount = current_amount - offset_amount
        if adjusted_amount > (transfer_minimum + transfer_threshold):
            transfer_amount = round_to(adjusted_amount - transfer_minimum,
                                       transfer_increment)

            oss.write(replace("""

              YYYY-MM-DD * "Transfering accumulated savings to other account"
                ACCOUNT         -AMOUNT CCY
                ACCOUNT_OUT      AMOUNT CCY

            """, {
                'YYYY-MM-DD': posting.entry.date + datetime.timedelta(days=1),
                'ACCOUNT': account,
                'ACCOUNT_OUT': account_out,
                'AMOUNT': '{:.2f}'.format(transfer_amount)
            }))

            offset_amount += transfer_amount

    return parse(oss.getvalue())


def generate_expense_accounts(date_birth):
    """Generate directives for expense accounts.

    Args:
      date_birth: Birth date of the character.
    Returns:
      A list of directives.
    """
    return parse(replace("""

      YYYY-MM-DD open Expenses:Food:Restaurant
      YYYY-MM-DD open Expenses:Food:Groceries

      YYYY-MM-DD open Expenses:Transport:Subway

      YYYY-MM-DD open Expenses:Home:Rent
      YYYY-MM-DD open Expenses:Home:Electricity
      YYYY-MM-DD open Expenses:Home:Internet

    """, {'YYYY-MM-DD': date_birth}))


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


def check_non_negative(entries, account, currency):
    """Check that the balance of the given account never goes negative.

    Args:
      entries: A list of directives.
      account: An account string, the account to check the balance for.
      currency: A string, the currency to check minimums for.
    Raises:
      AssertionError: if the balance goes negative.
    """
    for posting, balances in postings_for(sorted_entries(entries), [account]):
        balance = balances[account]
        assert all(amt.number >= ZERO
                   for amt in balance.get_amounts()), "Negative balance: {}".format(balance)


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

    return sorted_entries(rent_expenses + electricity_expenses + internet_expenses)


def generate_credit_expenses(date_begin, date_end, account_credit, account_checking):
    """Generate expenses paid out of a credit card account, including payments to the
    credit card.

    Args:
      date_begin: The start date.
      date_end: The end date.
      account: The checking account to generate expenses to.
    Returns:
      A list of directives.
    """
    restaurant_expenses = generate_periodic_expenses(
        date_seq(date_begin, date_end, 1, 5),
        restaurant_names, restaurant_narrations,
        account_credit, 'Expenses:Food:Restaurant',
        lambda: min(random.lognormvariate(math.log(30), math.log(1.5)),
                    random.randint(200, 220)))

    groceries_expenses = generate_periodic_expenses(
        date_seq(date_begin, date_end, 5, 20),
        groceries_names, "Buying groceries",
        account_credit, 'Expenses:Food:Groceries',
        lambda: min(random.lognormvariate(math.log(80), math.log(1.3)),
                    random.randint(250, 300)))

    subway_expenses = generate_periodic_expenses(
        date_seq(date_begin, date_end, 27, 33),
        "Metro Transport", "Subway tickets",
        account_credit, 'Expenses:Transport:Subway',
        lambda: D('120.00'))

    credit_expenses = sorted_entries(restaurant_expenses +
                                     groceries_expenses +
                                     subway_expenses)

    # Generate entries that will pay off the credit card (unconditionally).
    credit_payments = generate_clearing_entries(
        delay_dates(rrule.rrule(rrule.MONTHLY,
                                dtstart=date_begin, until=date_end, bymonthday=7), 0, 4),
        "CreditCard1", "Paying off credit card",
        credit_expenses,
        account_credit, account_checking)

    # Entries to open accounts.
    credit_preamble = generate_open_entries(date_birth, [account_credit], 'CCY')

    return sorted_entries(credit_preamble + credit_expenses + credit_payments)


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
        }
    new_contents = replace(contents, replacements)
    return format.align_beancount(new_contents), replacements


def main():
    argparser = argparse.ArgumentParser(description=__doc__.strip())
    argparser.add_argument('-s', '--seed', action='store', type=int,
                        help="Fix the random seed for debugging.")
    opts = argparser.parse_args()

    logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
    if opts.seed:
        random.seed(opts.seed)

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
    employer, address = random.choice(employers)

    # Salary income payments.
    income_entries = generate_employment_income(employer, address,
                                                annual_salary,
                                                account_checking,
                                                join(account_retirement, 'Cash'),
                                                date_begin, date_end)

    # Periodic expenses from banking accounts.
    banking_expenses = generate_banking_expenses(date_begin, date_end,
                                                 account_checking, rent_amount)

    # Expenses via credit card.
    credit_entries = generate_credit_expenses(date_begin, date_end,
                                              account_credit,
                                              account_checking)

    # Tax accounts.
    tax_preamble = generate_tax_preamble(date_birth)
    taxes = [(year, generate_tax_accounts(year))
             for year in range(date_begin.year, date_end.year)]
    tax_entries = tax_preamble + functools.reduce(operator.add,
                                                  (entries for _, entries in taxes))

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

    # Book transfers to investment account.
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

    # Investment accounts for retirement.
    retirement_entries = generate_retirement_investments(
        income_entries, account_retirement, {'RETINV1': 0.40,
                                             'RETINV2': 0.60})

    # Taxable savings / investment accounts.
    investment_entries = generate_taxable_investment(date_begin, date_end)

    # Expense accounts.
    expense_accounts_entries = generate_expense_accounts(date_birth)

    # Equity accounts.
    equity_entries = generate_open_entries(date_birth, [account_opening,
                                                        account_payable])

    # Format the results.
    output = io.StringIO()
    def output_section(title, entries):
        output.write('{}\n\n'.format(title))
        printer.print_entries(sorted_entries(entries), file=output)

    output.write(file_preamble)
    output_section('* Equity Accounts', equity_entries)
    output_section('* Banking', sorted_entries(banking_entries +
                                               banking_expenses +
                                               banking_transfers))
    output_section('* Credit-Cards', credit_entries)
    output_section('* Taxable Investments', investment_entries)
    output_section('* Retirement Investments', retirement_entries)
    output_section('* Sources of Income', income_entries)
    output_section('* Taxes', tax_preamble)
    for year, entries in taxes:
        output_section('** Tax Year {}'.format(year), entries)
    output_section('* Expenses', expense_accounts_entries)
    output_section('* Cash', [])  # FIXME TODO(blais): cash entries

    # Replace generic names by realistic names and output results.
    contents, replacements = contextualize_file(output.getvalue(), employer)
    sys.stdout.write(contents)

    # Validate the results parse fine.
    validate_output(contents,
                    [replace(account, replacements)
                     for account in [account_checking]],
                    replace('CCY', replacements))

    return 0



## TODO(blais) - bean-format the entire output file after renamings
## TODO(blais) - Generate random price series for commodities that we use; use those as input for costs.
## TODO(blais) - Create investments in retirement.
## TODO(blais) - Create investments in taxable account (along with sales).
## TODO(blais) - Add employer match amount for 401k.
## TODO(blais) - Generate some minimum amount of realistic-ish cash entries.
