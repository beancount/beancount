"""Generate a decently-sized example history, based on some rules.

This script is used to generate some meaningful input to Beancount, input that
looks as realistic as possible for a moderately complex mock individual. This
can also be used as an input generator for a stress test for performance
evaluation.
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


# Date of birth of our fictional character.
date_birth = datetime.date(1980, 1, 1)

# Default begin and end dates for the generation of data.
date_begin = datetime.date(2012, 1, 1)
date_end = datetime.date(2016, 1, 1)

# Name of the checking account.
account_checking = 'Assets:CC:Bank1:Checking'

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


def debug(*args):
    """Debug print to stderr. Just a convenience funicton.

    Args:
      *args: Arguments to be printed out.
    """
    print(*args, file=sys.stderr)


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


# def random_date_sequence(date_begin, date_end, days_mu, days_sigma):
#     date = date_begin
#     while date < date_end:
#         nb_days_forward = max(1, int(random.normalvariate(days_mu, days_sigma)))
#         date += datetime.timedelta(days=nb_days_forward)
#         yield date


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
              Expenses:Taxes:Year:CC:StateNY                    {state:.2f} CCY
              Expenses:Taxes:Year:CC:CityNYC                    {city:.2f} CCY
              Expenses:Taxes:Year:CC:SDI                        {sdi:.2f} CCY
              Expenses:Taxes:Year:CC:SocSec                     {socsec:.2f} CCY
              Assets:CC:Employer1:Vacation                      {vacation_hrs} VACCCY
              Income:CC:Employer1:Vacation                     -{vacation_hrs} VACCCY
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
    """Generate transactions for retirement investments.

    Args:
      date_begin: A date instance, when to beginning applying this.
      date_end: A date instance, when to end applying this.
    Returns:
      A list of directives.
    """
    retirement_string = replace("""
      YYYY-MM-DD open Assets:CC:Retirement:Cash    CCY
    """, {'YYYY-MM-DD': date_begin})

    return parse(retirement_string)


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

      YYYY-MM-DD pad Assets:CC:Bank1:Checking  Equity:Opening-Balances
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


def generate_periodic_expenses(frequency, date_begin, date_end,
                               payee, narration,
                               account_from, account_to,
                               amount_mu, amount_sigma=0,
                               delay_days_min=0, delay_days_max=0):
    """Generate periodic expense transactions.

    Args:
      frequency: A rrule frequency enum, such as rrule.MONTHLY.
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
      payee: A string, the payee name to use on the transactions.
      narration: A string, the narration to use on the transactions.
      account_from: An account string the debited account.
      account_to: An account string the credited account.
      amount_mu: The average amount.
      amount_sigma: The standard deviation of the amount.
      delay_days_min: The minimum amount of advance days for the transaction.
      delay_days_max: The maximum amount of advance days for the transaction.
    Returns:
      A list of directives.
    """

    oss = io.StringIO()
    for dt in rrule.rrule(frequency, dtstart=date_begin, until=date_end):
        date = dt.date()

        # Apply delay to the transaction date.
        date += datetime.timedelta(days=random.randint(delay_days_min, delay_days_max))

        txn_amount = D(random.normalvariate(amount_mu, amount_sigma))

        oss.write(replace("""

          YYYY-MM-DD * "Payee" "Narration"
            Account:From    -AMOUNT CCY
            Account:To       AMOUNT CCY

        """, {
            'YYYY-MM-DD': date,
            'Payee': payee,
            'Narration': narration,
            'Account:From': account_from,
            'Account:To': account_to,
            'AMOUNT': '{:.2f}'.format(txn_amount)
        }))

    return parse(oss.getvalue())


def generate_outgoing_transfers(entries,
                                account,
                                account_out,
                                transfer_minimum,
                                transfer_threshold):
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
    Returns:
      A list of new directives, the transfers to add to the given account.
    """
    oss = io.StringIO()
    real_root = realization.realize(entries)
    real_account = realization.get(real_root, account)
    balance = inventory.Inventory()
    for posting in real_account.postings:
        if not isinstance(posting, data.Posting):
            continue

        balance.add_position(posting.position)

        current_amount = balance.get_amount('CCY').number
        if current_amount > (transfer_minimum + transfer_threshold):
            transfer_amount = current_amount - transfer_minimum

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

            balance.add_amount(amount.Amount(-transfer_amount, 'CCY'))

    return parse(oss.getvalue())


def generate_expenses(date_birth):
    """Generate directives for expense accounts.

    Args:
      date_birth: Birth date of the character.
    Returns:
      A list of directives.
    """
    return parse(replace("""

      YYYY-MM-DD open Expenses:Home:Rent
      YYYY-MM-DD open Expenses:Home:Electricity
      YYYY-MM-DD open Expenses:Home:Internet

    """, {'YYYY-MM-DD': date_birth}))


def generate_equity(date_birth):
    """Generate directives for equity accounts we're going to use.

    Args:
      date_birth: Birth date of the character.
    Returns:
      A list of directives.
    """
    return parse(replace("""
      YYYY-MM-DD open Equity:Opening-Balances
    """, {'YYYY-MM-DD': date_birth}))


def check_non_negative(entries, account):
    """Check that the balance of the given account never goes negative.

    Args:
      entries: A list of directives.
      account: An account string, the account to check the balance for.
    Raises:
      AssertionError: if the balance goes negative.
    """
    real_root = realization.realize(entries)
    real_account = realization.get(real_root, account)
    balance = inventory.Inventory()
    for posting in real_account.postings:
        if isinstance(posting, data.Posting):
            balance.add_position(posting.position)
            assert all(amt.number > ZERO for amt in balance.get_amounts())


def validate_output(contents, positive_accounts):
    """Check that the output file validates.

    Args:
      contents: A string, the output file.
      positive_accounts: A list of strings, account names to check for
        non-negative balances.
    Raises:
      AssertionError: If the output does not validate.
    """
    loaded_entries, _, _ = loader.load_string(
        contents,
        log_errors=sys.stderr,
        extra_validations=validation.HARDCORE_VALIDATIONS)

    # Sanity checks: Check that the checking balance never goes below zero.
    for account in positive_accounts:
        check_non_negative(loaded_entries, account)


def main():
    argparser = argparse.ArgumentParser(description=__doc__.strip())
    opts = argparser.parse_args()

    # The following code entirely writes out the output to generic names, such
    # as "Employer1", "Bank1", and "CCY" (for principal currency). Those names
    # are purposely chosen to be unique, and only near the very end do we make
    # renamings to more specific and realistic names.

    # Estimate the rent.
    rent_amount = (int(annual_salary / rent_divisor) / rent_increment) * rent_increment

    # Get a random employer.
    employer, address = random.choice(employers)

    # Income sources.
    income_entries = generate_employment_income(employer, address,
                                                annual_salary,
                                                account_checking,
                                                'Assets:CC:Retirement:Cash',
                                                date_begin, date_end)

    # Expenses via banking.
    rent_expenses = generate_periodic_expenses(
        rrule.MONTHLY, date_begin, date_end,
        "RiverBank Properties", "Paying the rent",
        account_checking, 'Expenses:Home:Rent',
        float(rent_amount), 0,
        2, 5)

    electricity_expenses = generate_periodic_expenses(
        rrule.MONTHLY, date_begin, date_end,
        "EDISON POWER", "",
        account_checking, 'Expenses:Home:Electricity',
        65, 10,
        7, 8)

    internet_expenses = generate_periodic_expenses(
        rrule.MONTHLY, date_begin, date_end,
        "Wine-Tarner Cable", "",
        account_checking, 'Expenses:Home:Internet',
        80, 0.10,
        20, 22)

    banking_expenses = rent_expenses + electricity_expenses + internet_expenses

    # Book transfers to investment account.
    banking_transfers = generate_outgoing_transfers(
        sorted(income_entries + banking_expenses, key=data.entry_sortkey),
        account_checking,
        'Assets:CC:Investment:Cash',
        rent_amount + D('100'),
        D('4000'))

    # Tax accounts.
    tax_preamble = generate_tax_preamble(date_birth)
    taxes = [(year, generate_tax_accounts(year))
             for year in range(date_begin.year, date_end.year)]

    # Banking accounts.
    banking_entries = generate_banking(date_begin, date_end, rent_amount * D('1.10'))

    # Investment accounts for retirement.
    retirement_entries = generate_retirement_investment(date_begin, date_end)

    # Taxable savings / investment accounts.
    investment_entries = generate_taxable_investment(date_begin, date_end)

    # Expense accounts.
    expenses_entries = generate_expenses(date_birth)

    # Equity accounts.
    equity_entries = generate_equity(date_birth)

    # Format the results.
    output = io.StringIO()

    # Preambule and options.
    output.write(dedent("""\
      ;; -*- mode: org; mode: beancount; -*-
      ;; THIS FILE HAS BEEN AUTO-GENERATED.
      * Options

      option "title" "Example Beancount file"
      option "operating_currency" "CCY"

    """))

    output.write('* Equity Accounts\n\n')
    printer.print_entries(equity_entries, file=output)

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

    replacements = {
        'CC': 'US',
        'CCY': 'USD',
        'VACCCY': 'VACHR',
        'DEFCCY': 'IRAUSD',
        'Bank1': 'BofA',
        'Employer1': employer,
        'Retirement': 'Vanguard',
        }

    contents = replace(generic_contents, replacements)

    # Output the results.
    sys.stdout.write(contents)

    # Validate the results parse fine.
    validate_output(contents, [replace(account, replacements)
                               for account in [account_checking]])

    return 0



## TODO(blais) - bean-format the entire output file after renamings
## TODO(blais) - Expenses in checking accounts.
## TODO(blais) - Credit card accounts, with lots of expenses (two times).
## TODO(blais) - Move this to a unit-tested location.
