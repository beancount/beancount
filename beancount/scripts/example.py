__copyright__ = "Copyright (C) 2014-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import calendar
import collections
import contextlib
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
import sys
import textwrap

import click
import dateutil.parser
from dateutil import rrule

from beancount import loader
from beancount.core import amount
from beancount.core import convert
from beancount.core import data
from beancount.core import display_context
from beancount.core import getters
from beancount.core import inventory
from beancount.core import prices
from beancount.core import realization
from beancount.core.account import join
from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core.number import round_to
from beancount.ops import validation
from beancount.parser import booking
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser.version import VERSION
from beancount.scripts import format
from beancount.utils import misc_utils

# Constants.
ONE_DAY = datetime.timedelta(days=1)

# Annual salary.
ANNUAL_SALARY = D("120000")

# Annual vacation days.
ANNUAL_VACATION_DAYS = D("15")

# Divisor of the annual salary to estimate the rent.
RENT_DIVISOR = D("50")
RENT_INCREMENT = D("25")

# A list of mock employers.
EMPLOYERS = [
    ("Hooli", "1 Carloston Rd, Mountain Beer, CA"),
    ("BayBook", "1501 Billow Rd, Benlo Park, CA"),
    ("Babble", "1 Continuous Loop, Bupertina, CA"),
    ("Hoogle", "1600 Amphibious Parkway, River View, CA"),
]

# Generic names of restaurants and grocery places to choose from.
RESTAURANT_NAMES = [
    "Rose Flower",
    "Cafe Modagor",
    "Goba Goba",
    "Kin Soy",
    "Uncle Boons",
    "China Garden",
    "Jewel of Morroco",
    "Chichipotle",
]

RESTAURANT_NARRATIONS = [
    "Eating out {}".format(party_name)
    for party_name in [
        "with Joe",
        "with Natasha",
        "with Bill",
        "with Julie",
        "with work buddies",
        "after work",
        "alone",
        "",
    ]
]

GROCERIES_NAMES = ["Onion Market", "Good Moods Market", "Corner Deli", "Farmer Fresh"]

HOME_NAME = "New Metropolis"

TRIP_DESTINATIONS = {
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
RETIREMENT_LIMITS = {
    2000: D("10500"),
    2001: D("10500"),
    2002: D("11000"),
    2003: D("12000"),
    2004: D("13000"),
    2005: D("14000"),
    2006: D("15000"),
    2007: D("15500"),
    2008: D("15500"),
    2009: D("16500"),
    2010: D("16500"),
    2011: D("16500"),
    2012: D("17000"),
    2013: D("17500"),
    2014: D("17500"),
    2015: D("18000"),
    2016: D("18000"),
    None: D("18500"),
}

FILE_PREAMBLE = """\
;; -*- mode: org; mode: beancount; -*-
;; Birth: {date_birth}
;; Dates: {date_begin} - {date_end}
;; THIS FILE HAS BEEN AUTO-GENERATED.
* Options

option "title" "Example Beancount file"
option "operating_currency" "CCY"

"""


def parse(input_string):
    """Parse some input string and assert no errors.

    This parse function does not just create the object, it also triggers local
    interpolation to fill in the missing amounts.

    Args:
      input_string: Beancount input text.
    Returns:
      A list of directive objects.
    """
    entries, errors, options_map = parser.parse_string(textwrap.dedent(input_string))
    if errors:
        printer.print_errors(errors, file=sys.stderr)
        raise ValueError("Parsed text has errors")

    # Interpolation.
    entries, unused_balance_errors = booking.book(entries, options_map)

    return data.sorted(entries)


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
    one_day = datetime.timedelta(days=1)
    while date < date_end:
        date += one_day
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
    """Delay the dates from the given iterator by some uniformly drawn number of days.

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
    for dtime in dates:
        date = dtime.date() if isinstance(dtime, datetime.datetime) else dtime
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
      A list of TxnPosting's for all the accounts, in sorted order.
    """
    real_root = realization.realize(entries)
    merged_postings = []
    for account in accounts:
        real_account = realization.get(real_root, account)
        if real_account is None:
            continue
        merged_postings.extend(
            txn_posting
            for txn_posting in real_account.txn_postings
            if isinstance(txn_posting, data.TxnPosting)
        )
    merged_postings.sort(key=lambda txn_posting: txn_posting.txn.date)
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
        posting: An instance of TxnPosting
        balances: A dict of Inventory balances for the given accounts _after_
          applying the posting. These inventory objects can be mutated to adjust
          the balance due to generated transactions to be applied later.
    """
    assert isinstance(accounts, list)
    merged_txn_postings = merge_postings(entries, accounts)
    balances = collections.defaultdict(inventory.Inventory)
    for txn_posting in merged_txn_postings:
        if before:
            yield txn_posting, balances
        posting = txn_posting.posting
        balances[posting.account].add_position(posting)
        if not before:
            yield txn_posting, balances


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
    merged_txn_postings = iter(merge_postings(entries, accounts))
    txn_posting = next(merged_txn_postings, None)
    for date in date_iter(date_begin, date_end):
        while txn_posting and txn_posting.txn.date == date:
            posting = txn_posting.posting
            balances[posting.account].add_position(posting)
            txn_posting = next(merged_txn_postings, None)
        yield date, balances


def iter_quarters(date_begin, date_end):
    """Iterate over all quarters between begin and end dates.

    Args:
      date_begin: The start date.
      date_end: The end date.
    Yields:
      Instances of datetime.date at the beginning of the quarters. This will
      include the quarter of the beginning date and of the end date.
    """
    quarter = (date_begin.year, (date_begin.month - 1) // 3)
    quarter_last = (date_end.year, (date_end.month - 1) // 3)
    assert quarter <= quarter_last
    while True:
        year, trimester = quarter
        yield datetime.date(year, trimester * 3 + 1, 1)
        if quarter == quarter_last:
            break
        trimester = (trimester + 1) % 4
        if trimester == 0:
            year += 1
        quarter = (year, trimester)


def get_minimum_balance(entries, account, currency):
    """Compute the minimum balance of the given account according to the entries history.

    Args:
      entries: A list of directives.
      account: An account string.
      currency: A currency string, for which we want to compute the minimum.
    Returns:
      A Decimal number, the minimum amount throughout the history of this account.
    """
    min_amount = decimal.Decimal("0.001")
    for _, balances in postings_for(entries, [account]):
        balance = balances[account]
        current = balance.get_currency_units(currency).number
        min_amount = min(current, min_amount)
    return min_amount


def generate_employment_income(
    employer_name,
    employer_address,
    annual_salary,
    account_deposit,
    account_retirement,
    date_begin,
    date_end,
):
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
    preamble = parse(f"""

        {date_begin} event "employer" "{employer_name}, {employer_address}"

        {date_begin} open Income:CC:Employer1:Salary           CCY
        ;{date_begin} open Income:CC:Employer1:AnnualBonus     CCY
        {date_begin} open Income:CC:Employer1:GroupTermLife    CCY

        {date_begin} open Income:CC:Employer1:Vacation         VACHR
        {date_begin} open Assets:CC:Employer1:Vacation         VACHR
        {date_begin} open Expenses:Vacation                    VACHR

        {date_begin} open Expenses:Health:Life:GroupTermLife
        {date_begin} open Expenses:Health:Medical:Insurance
        {date_begin} open Expenses:Health:Dental:Insurance
        {date_begin} open Expenses:Health:Vision:Insurance

        ;{date_begin} open Expenses:Vacation:Employer

    """)

    date_prev = None

    contrib_retirement = ZERO
    contrib_socsec = ZERO

    biweekly_pay = annual_salary / 26
    gross = biweekly_pay

    medicare = gross * D("0.0231")
    federal = gross * D("0.2303")
    state = gross * D("0.0791")
    city = gross * D("0.0379")
    sdi = D("1.12")

    lifeinsurance = D("24.32")
    dental = D("2.90")
    medical = D("27.38")
    vision = D("42.30")

    fixed = medicare + federal + state + city + sdi + dental + medical + vision

    # Calculate vacation hours per-pay.
    with decimal.localcontext() as ctx:
        ctx.prec = 4
        vacation_hrs = (ANNUAL_VACATION_DAYS * D("8")) / D("26")

    transactions = []
    for dtime in misc_utils.skipiter(
        rrule.rrule(rrule.WEEKLY, byweekday=rrule.TH, dtstart=date_begin, until=date_end), 2
    ):
        date = dtime.date()
        year = date.year

        if not date_prev or date_prev.year != date.year:
            contrib_retirement = RETIREMENT_LIMITS.get(date.year, RETIREMENT_LIMITS[None])
            contrib_socsec = D("7000")
        date_prev = date

        retirement_uncapped = math.ceil((gross * D("0.25")) / 100) * 100
        retirement = min(contrib_retirement, retirement_uncapped)
        contrib_retirement -= retirement

        socsec_uncapped = gross * D("0.0610")
        socsec = min(contrib_socsec, socsec_uncapped)
        contrib_socsec -= socsec

        with decimal.localcontext() as ctx:
            ctx.prec = 6
            deposit = gross - retirement - fixed - socsec

        retirement_neg = -retirement
        gross_neg = -gross
        lifeinsurance_neg = -lifeinsurance
        vacation_hrs_neg = -vacation_hrs

        transactions.extend(
            parse(
                f"""
            {date} * "{employer_name}" "Payroll"
              {account_deposit}                                 {deposit:.2f} CCY
              """
                + (
                    ""
                    if retirement == ZERO
                    else f"""\
              {account_retirement}                              {retirement:.2f} CCY
              Assets:CC:Federal:PreTax401k                      {retirement_neg:.2f} DEFCCY
              Expenses:Taxes:Y{year}:CC:Federal:PreTax401k      {retirement:.2f} DEFCCY
              """
                )
                + f"""\
              Income:CC:Employer1:Salary                        {gross_neg:.2f} CCY
              Income:CC:Employer1:GroupTermLife                 {lifeinsurance_neg:.2f} CCY
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
              Assets:CC:Employer1:Vacation                      {vacation_hrs:.2f} VACHR
              Income:CC:Employer1:Vacation                      {vacation_hrs_neg:.2f} VACHR
        """
            )
        )

    return preamble + transactions


def generate_tax_preamble(date_birth):
    """Generate tax declarations not specific to any particular year.

    Args:
      date_birth: A date instance, the birth date of the character.
    Returns:
      A list of directives.
    """
    return parse(f"""
      ;; Tax accounts not specific to a year.
      {date_birth} open Income:CC:Federal:PreTax401k     DEFCCY
      {date_birth} open Assets:CC:Federal:PreTax401k     DEFCCY

    """)


def generate_tax_accounts(year, date_max):
    """Generate accounts and contribution directives for a particular tax year.

    Args:
      year: An integer, the year we're to generate this for.
      date_max: The maximum date to produce an entry for.
    Returns:
      A list of directives.
    """
    date_year = datetime.date(year, 1, 1)
    date_filing = datetime.date(year + 1, 3, 20) + datetime.timedelta(
        days=random.randint(0, 5)
    )

    date_federal = date_filing + datetime.timedelta(days=random.randint(0, 4))
    date_state = date_filing + datetime.timedelta(days=random.randint(0, 4))

    quantum = D("0.01")
    amount_federal = D(max(random.normalvariate(500, 120), 12)).quantize(quantum)
    amount_federal_neg = -amount_federal
    amount_state = D(max(random.normalvariate(300, 100), 10)).quantize(quantum)
    amount_state_neg = -amount_state
    amount_payable = -(amount_federal + amount_state)

    amount_limit = RETIREMENT_LIMITS.get(year, RETIREMENT_LIMITS[None])
    amount_limit_neg = -amount_limit

    entries = parse(f"""

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
        Income:CC:Federal:PreTax401k     {amount_limit_neg} DEFCCY
        Assets:CC:Federal:PreTax401k     {amount_limit} DEFCCY

      {date_filing} * "Filing taxes for {year}"
        Expenses:Taxes:Y{year}:CC:Federal      {amount_federal:.2f} CCY
        Expenses:Taxes:Y{year}:CC:State        {amount_state:.2f} CCY
        Liabilities:AccountsPayable            {amount_payable:.2f} CCY

      {date_federal} * "FEDERAL TAXPYMT"
        Assets:CC:Bank1:Checking       {amount_federal_neg:.2f} CCY
        Liabilities:AccountsPayable    {amount_federal:.2f} CCY

      {date_state} * "STATE TAX & FINANC PYMT"
        Assets:CC:Bank1:Checking       {amount_state_neg:.2f} CCY
        Liabilities:AccountsPayable    {amount_state:.2f} CCY

    """)

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
    match_frac = D("0.50")

    new_entries = parse(f"""

      {entries[0].date} open {account_income}   CCY

    """)

    for txn_posting, balances in postings_for(entries, [account_invest]):
        amount = txn_posting.posting.units.number * match_frac
        amount_neg = -amount
        date = txn_posting.txn.date + ONE_DAY
        new_entries.extend(
            parse(f"""

          {date} * "Employer match for contribution"
            {account_invest}         {amount:.2f} CCY
            {account_income}         {amount_neg:.2f} CCY

        """)
        )

    return new_entries


def generate_retirement_investments(entries, account, commodities_items, price_map):
    """Invest money deposited to the given retirement account.

    Args:
      entries: A list of directives
      account: The root account for all retirement investment sub-accounts.
      commodities_items: A list of (commodity, fraction to be invested in) items.
      price_map: A dict of prices, as per beancount.core.prices.build_price_map().
    Returns:
      A list of new directives for the given investments. This also generates account
      opening directives for the desired investment commodities.
    """
    open_entries = []
    account_cash = join(account, "Cash")
    date_origin = entries[0].date
    open_entries.extend(
        parse(f"""

      {date_origin} open {account} CCY
        institution: "Retirement_Institution"
        address: "Retirement_Address"
        phone: "Retirement_Phone"

      {date_origin} open {account_cash} CCY
        number: "882882"

    """)
    )
    for currency, _ in commodities_items:
        open_entries.extend(
            parse(f"""
          {date_origin} open {account}:{currency} {currency}
            number: "882882"
        """)
        )

    new_entries = []
    for txn_posting, balances in postings_for(entries, [account_cash]):
        balance = balances[account_cash]
        amount_to_invest = balance.get_currency_units("CCY").number

        # Find the date the following Monday, the date to invest.
        txn_date = txn_posting.txn.date
        while txn_date.weekday() != calendar.MONDAY:
            txn_date += ONE_DAY

        for commodity, fraction in commodities_items:
            amount_fraction = amount_to_invest * D(fraction)

            # Find the price at that date.
            _, price = prices.get_price(price_map, (commodity, "CCY"), txn_date)
            units = (amount_fraction / price).quantize(D("0.001"))
            amount_cash = (units * price).quantize(D("0.01"))
            amount_cash_neg = -amount_cash
            new_entries.extend(
                parse(f"""

              {txn_date} * "Investing {fraction:.0%} of cash in {commodity}"
                {account}:{commodity}  {units:.3f} {commodity} {{{price:.2f} CCY}}
                {account}:Cash         {amount_cash_neg:.2f} CCY

            """)
            )

            balance.add_amount(amount.Amount(-amount_cash, "CCY"))

    return data.sorted(open_entries + new_entries)


def generate_banking(entries, date_begin, date_end, amount_initial):
    """Generate a checking account opening.

    Args:
      entries: A list of entries which affect this account.
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
      amount_initial: A Decimal instance, the amount to initialize the checking
        account with.
    Returns:
      A list of directives.
    """
    amount_initial_neg = -amount_initial
    new_entries = parse(f"""

      {date_begin} open Assets:CC:Bank1
        institution: "Bank1_Institution"
        address: "Bank1_Address"
        phone: "Bank1_Phone"

      {date_begin} open Assets:CC:Bank1:Checking    CCY
        account: "00234-48574897"

      ;; {date_begin} open Assets:CC:Bank1:Savings    CCY

      {date_begin} * "Opening Balance for checking account"
        Assets:CC:Bank1:Checking   {amount_initial} CCY
        Equity:Opening-Balances    {amount_initial_neg} CCY

    """)

    date_balance = date_begin + datetime.timedelta(days=1)
    account = "Assets:CC:Bank1:Checking"
    for txn_posting, balances in postings_for(
        data.sorted(entries + new_entries), [account], before=True
    ):
        if txn_posting.txn.date >= date_balance:
            break
    amount_balance = balances[account].get_currency_units("CCY").number
    bal_entries = parse(f"""

      {date_balance} balance Assets:CC:Bank1:Checking   {amount_balance} CCY

    """)

    return new_entries + bal_entries


def generate_taxable_investment(date_begin, date_end, entries, price_map, stocks):
    """Generate opening directives and transactions for an investment account.

    Args:
      date_begin: A date instance, the beginning date.
      date_end: A date instance, the end date.
      entries: A list of entries that contains at least the transfers to the investment
        account's cash account.
      price_map: A dict of prices, as per beancount.core.prices.build_price_map().
      stocks: A list of strings, the list of commodities to invest in.
    Returns:
      A list of directives.
    """
    account = "Assets:CC:Investment"
    income = "Income:CC:Investment"
    account_cash = join(account, "Cash")
    account_gains = "{income}:PnL".format(income=income)
    dividends = "Dividend"
    accounts_stocks = ["Assets:CC:Investment:{}".format(commodity) for commodity in stocks]

    open_entries = parse(f"""
      {date_begin} open {account}:Cash    CCY
      {date_begin} open {account_gains}    CCY
    """)
    for stock in stocks:
        open_entries.extend(
            parse(f"""
          {date_begin} open {account}:{stock} {stock}
          {date_begin} open {income}:{stock}:{dividends}    CCY
        """)
        )

    # Figure out dates at which dividends should be distributed, near the end of
    # each quarter.
    days_to = datetime.timedelta(days=3 * 90 - 10)
    dividend_dates = []
    for quarter_begin in iter_quarters(date_begin, date_end):
        end_of_quarter = quarter_begin + days_to
        if not (date_begin < end_of_quarter < date_end):
            continue
        dividend_dates.append(end_of_quarter)

    # Iterate over all the dates, but merging in the postings for the cash
    # account.
    min_amount = D("1000.00")
    round_amount = D("100.00")
    commission = D("8.95")
    round_units = D("1")
    frac_invest = D("1.00")
    frac_dividend = D("0.004")
    p_daily_buy = 1.0 / 15  # days
    p_daily_sell = 1.0 / 90  # days

    stocks_inventory = inventory.Inventory()
    new_entries = []
    dividend_date_iter = iter(dividend_dates)
    next_dividend_date = next(dividend_date_iter, None)
    for date, balances in iter_dates_with_balance(
        date_begin, date_end, entries, [account_cash]
    ):
        # Check if we should insert a dividend. Note that we could not factor
        # this out because we want to explicitly reinvest the cash dividends and
        # we also want the dividends to be proportional to the amount of
        # invested stock, so one feeds on the other and vice-versa.
        if next_dividend_date and date > next_dividend_date:
            # Compute the total balances for the stock accounts in order to
            # create a realistic dividend.
            total = inventory.Inventory()
            for account_stock in accounts_stocks:
                total.add_inventory(balances[account_stock])

            # Create an entry offering dividends of 1% of the portfolio.
            portfolio_cost = total.reduce(convert.get_cost).get_currency_units("CCY").number
            amount_cash = (frac_dividend * portfolio_cost).quantize(D("0.01"))
            amount_cash_neg = -amount_cash
            stock = random.choice(stocks)
            cash_dividend = parse(f"""
              {next_dividend_date} * "Dividends on portfolio"
                {account}:Cash        {amount_cash:.2f} CCY
                {income}:{stock}:{dividends}   {amount_cash_neg:.2f} CCY
            """)[0]
            new_entries.append(cash_dividend)

            # Advance the next dividend date.
            next_dividend_date = next(dividend_date_iter, None)

        # If the balance is high, buy with high probability.
        balance = balances[account_cash]
        total_cash = balance.get_currency_units("CCY").number
        assert total_cash >= ZERO, "Cash balance is negative: {}".format(total_cash)
        invest_cash = total_cash * frac_invest - commission
        if invest_cash > min_amount:
            if random.random() < p_daily_buy:
                commodities = random.sample(stocks, random.randint(1, len(stocks)))
                lot_amount = round_to(invest_cash / len(commodities), round_amount)

                for stock in commodities:
                    # Find the price at that date.
                    _, price = prices.get_price(price_map, (stock, "CCY"), date)

                    units = round_to((lot_amount / price), round_units)
                    if units <= ZERO:
                        continue
                    amount_cash = -(units * price + commission)
                    # logging.info('Buying %s %s @ %s CCY = %s CCY',
                    #              units, stock, price, units * price)

                    buy = parse(f"""
                      {date} * "Buy shares of {stock}"
                        {account}:Cash                  {amount_cash:.2f} CCY
                        {account}:{stock}               {units:.0f} {stock} {{{price:.2f} CCY}}
                        Expenses:Financial:Commissions  {commission:.2f} CCY
                    """)[0]
                    new_entries.append(buy)

                    account_stock = ":".join([account, stock])
                    balances[account_cash].add_position(buy.postings[0])
                    balances[account_stock].add_position(buy.postings[1])
                    stocks_inventory.add_position(buy.postings[1])

                # Don't sell on days you buy.
                continue

        # Otherwise, sell with low probability.
        if not stocks_inventory.is_empty() and random.random() < p_daily_sell:
            # Choose the lot with the highest gain or highest loss.
            gains = []
            for position in stocks_inventory.get_positions():
                base_quote = (position.units.currency, position.cost.currency)
                _, price = prices.get_price(price_map, base_quote, date)
                if price == position.cost.number:
                    continue  # Skip lots without movement.
                market_value = position.units.number * price
                book_value = convert.get_cost(position).number
                gain = market_value - book_value
                gains.append((gain, market_value, price, position))
            if not gains:
                continue

            # Sell either biggest winner or biggest loser.
            biggest = bool(random.random() < 0.5)
            lot_tuple = sorted(gains)[0 if biggest else -1]
            gain, market_value, price, sell_position = lot_tuple
            # logging.info('Selling {} for {}'.format(sell_position, market_value))

            sell_position = -sell_position
            stock = sell_position.units.currency
            amount_cash = market_value - commission
            amount_gain = -gain
            sell = parse(f"""
              {date} * "Sell shares of {stock}"
                {account}:{stock}               {sell_position} @ {price:.2f} CCY
                {account}:Cash                  {amount_cash:.2f} CCY
                Expenses:Financial:Commissions  {commission:.2f} CCY
                {account_gains}                 {amount_gain:.2f} CCY
            """)[0]
            new_entries.append(sell)

            balances[account_cash].add_position(sell.postings[1])
            stocks_inventory.add_position(sell.postings[0])
            continue

    return open_entries + new_entries


def generate_periodic_expenses(
    date_iter, payee, narration, account_from, account_to, amount_generator
):
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
    for dtime in date_iter:
        date = dtime.date() if isinstance(dtime, datetime.datetime) else dtime
        amount = D(amount_generator())
        txn_payee = payee if isinstance(payee, str) else random.choice(payee)
        txn_narration = (
            narration if isinstance(narration, str) else random.choice(narration)
        )
        amount_neg = -amount
        new_entries.extend(
            parse(f"""
          {date} * "{txn_payee}" "{txn_narration}"
            {account_from}    {amount_neg:.2f} CCY
            {account_to}      {amount:.2f} CCY
        """)
        )

    return new_entries


def generate_clearing_entries(
    date_iter, payee, narration, entries, account_clear, account_from
):
    """Generate entries to clear the value of an account.

    Args:
      date_iter: An iterator of datetime.date instances.
      payee: A string, the payee name to use on the transactions.
      narration: A string, the narration to use on the transactions.
      entries: A list of entries.
      account_clear: The account to clear.
      account_from: The source account to clear 'account_clear' from.
    Returns:
      A list of directives.
    """
    new_entries = []

    # The next date we're looking for.
    date_iter = iter(date_iter)
    next_date = next(date_iter, None)
    if not next_date:
        return new_entries

    # Iterate over all the postings of the account to clear.
    for txn_posting, balances in postings_for(entries, [account_clear]):
        balance_clear = balances[account_clear]

        # Check if we need to clear.
        if next_date <= txn_posting.txn.date:
            pos_amount = balance_clear.get_currency_units("CCY")
            neg_amount = -pos_amount
            new_entries.extend(
                parse(f"""
              {next_date} * "{payee}" "{narration}"
                {account_clear}     {neg_amount.number:.2f} CCY
                {account_from}      {pos_amount.number:.2f} CCY
            """)
            )
            balance_clear.add_amount(neg_amount)

            # Advance to the next date we're looking for.
            next_date = next(date_iter, None)
            if not next_date:
                break

    return new_entries


def generate_outgoing_transfers(
    entries, account, account_out, transfer_minimum, transfer_threshold, transfer_increment
):
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
    amounts = [
        (balances[account].get_currency_units("CCY").number, txn_posting)
        for txn_posting, balances in postings_for(entries, [account])
    ]
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
    for current_amount, (_, txn_posting) in zip(capped_amounts, amounts):
        if txn_posting.txn.date >= last_date:
            break

        adjusted_amount = current_amount - offset_amount
        if adjusted_amount > (transfer_minimum + transfer_threshold):
            amount_transfer = round_to(
                adjusted_amount - transfer_minimum, transfer_increment
            )

            date = txn_posting.txn.date + datetime.timedelta(days=1)
            amount_transfer_neg = -amount_transfer
            new_entries.extend(
                parse(f"""
              {date} * "Transfering accumulated savings to other account"
                {account}          {amount_transfer_neg:2f} CCY
                {account_out}      {amount_transfer:2f} CCY
            """)
            )

            offset_amount += amount_transfer

    return new_entries


def generate_expense_accounts(date_birth):
    """Generate directives for expense accounts.

    Args:
      date_birth: Birth date of the character.
    Returns:
      A list of directives.
    """
    return parse(f"""

      {date_birth} open Expenses:Food:Groceries
      {date_birth} open Expenses:Food:Restaurant
      {date_birth} open Expenses:Food:Coffee
      {date_birth} open Expenses:Food:Alcohol

      {date_birth} open Expenses:Transport:Tram

      {date_birth} open Expenses:Home:Rent
      {date_birth} open Expenses:Home:Electricity
      {date_birth} open Expenses:Home:Internet
      {date_birth} open Expenses:Home:Phone

      {date_birth} open Expenses:Financial:Fees
      {date_birth} open Expenses:Financial:Commissions

    """)


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
    return parse(
        "".join(
            "{date} open {account} {currency}\n".format(
                date=date, account=account, currency=currency or ""
            )
            for account in accounts
        )
    )


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
    with contextlib.suppress(StopIteration):
        for txn_posting, balance in postings_for(entries, [account], before=True):
            while txn_posting.txn.date >= next_date:
                amount = balance[account].get_currency_units("CCY").number
                balance_checks.extend(
                    parse(f"""
                  {next_date} balance {account} {amount} CCY
                """)
                )
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
    for txn_posting, balances in postings_for(data.sorted(entries), [account], before=True):
        balance = balances[account]
        date = txn_posting.txn.date
        if date != previous_date:
            assert all(
                pos.units.number >= ZERO for pos in balance.get_positions()
            ), "Negative balance: {} at: {}".format(balance, txn_posting.txn.date)
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
        contents, log_errors=sys.stderr, extra_validations=validation.HARDCORE_VALIDATIONS
    )

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
        "BANK FEES",
        "Monthly bank fee",
        account,
        "Expenses:Financial:Fees",
        lambda: D("4.00"),
    )

    rent_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 2, 5),
        "RiverBank Properties",
        "Paying the rent",
        account,
        "Expenses:Home:Rent",
        lambda: random.normalvariate(float(rent_amount), 0),
    )

    electricity_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 7, 8),
        "EDISON POWER",
        "",
        account,
        "Expenses:Home:Electricity",
        lambda: random.normalvariate(65, 0),
    )

    internet_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 20, 22),
        "Wine-Tarner Cable",
        "",
        account,
        "Expenses:Home:Internet",
        lambda: random.normalvariate(80, 0.10),
    )

    phone_expenses = generate_periodic_expenses(
        delay_dates(rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end), 17, 19),
        "Verizon Wireless",
        "",
        account,
        "Expenses:Home:Phone",
        lambda: random.normalvariate(60, 10),
    )

    return data.sorted(
        fee_expenses
        + rent_expenses
        + electricity_expenses
        + internet_expenses
        + phone_expenses
    )


def generate_regular_credit_expenses(
    date_birth, date_begin, date_end, account_credit, account_checking
):
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
        RESTAURANT_NAMES,
        RESTAURANT_NARRATIONS,
        account_credit,
        "Expenses:Food:Restaurant",
        lambda: min(
            random.lognormvariate(math.log(30), math.log(1.5)), random.randint(200, 220)
        ),
    )

    groceries_expenses = generate_periodic_expenses(
        date_random_seq(date_begin, date_end, 5, 20),
        GROCERIES_NAMES,
        "Buying groceries",
        account_credit,
        "Expenses:Food:Groceries",
        lambda: min(
            random.lognormvariate(math.log(80), math.log(1.3)), random.randint(250, 300)
        ),
    )

    subway_expenses = generate_periodic_expenses(
        date_random_seq(date_begin, date_end, 27, 33),
        "Metro Transport Authority",
        "Tram tickets",
        account_credit,
        "Expenses:Transport:Tram",
        lambda: D("120.00"),
    )

    credit_expenses = data.sorted(
        restaurant_expenses + groceries_expenses + subway_expenses
    )

    # Entries to open accounts.
    credit_preamble = generate_open_entries(date_birth, [account_credit], "CCY")

    return data.sorted(credit_preamble + credit_expenses)


def compute_trip_dates(date_begin, date_end):
    """Generate dates at reasonable intervals for trips during the given time period.

    Args:
      date_begin: The start date.
      date_end: The end date.
    Yields:
      Pairs of dates for the trips within the period.
    """
    # Min and max number of days remaining at home.
    days_at_home = (4 * 30, 13 * 30)

    # Length of trip.
    days_trip = (8, 22)

    # Number of days to ensure no trip at the beginning and the end.
    days_buffer = 21

    date_begin += datetime.timedelta(days=days_buffer)
    date_end -= datetime.timedelta(days=days_buffer)

    date = date_begin
    while 1:
        duration_at_home = datetime.timedelta(days=random.randint(*days_at_home))
        duration_trip = datetime.timedelta(days=random.randint(*days_trip))
        date_trip_begin = date + duration_at_home
        date_trip_end = date_trip_begin + duration_trip
        if date_trip_end >= date_end:
            break
        yield (date_trip_begin, date_trip_end)
        date = date_trip_end


def generate_trip_entries(
    date_begin, date_end, tag, config, trip_city, home_city, account_credit
):
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
    p_day_generate = 0.3

    new_entries = []
    for date in date_iter(date_begin, date_end):
        for payee, account_expense, (mu, sigma3) in config:
            if random.random() < p_day_generate:
                amount = random.normalvariate(mu, sigma3 / 3.0)
                amount_neg = -amount
                new_entries.extend(
                    parse(f"""
                  {date} * "{payee}" "" #{tag}
                    {account_credit}     {amount_neg:.2f} CCY
                    {account_expense}    {amount:.2f} CCY
                """)
                )

    # Consume the vacation days.
    vacation_hrs = (date_end - date_begin).days * 8  # hrs/day
    new_entries.extend(
        parse(f"""
      {date_end} * "Consume vacation days"
        Assets:CC:Employer1:Vacation -{vacation_hrs:.2f} VACHR
        Expenses:Vacation             {vacation_hrs:.2f} VACHR
    """)
    )

    # Generate events for the trip.
    new_entries.extend(
        parse(f"""
      {date_begin} event "location" "{trip_city}"
      {date_end}   event "location" "{home_city}"
    """)
    )

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
    """Generate weekly or monthly price entries for the given currencies.

    Args:
      date_begin: The start date.
      date_end: The end date.
      currencies: A list of currency strings to generate prices for.
      cost_currency: A string, the cost currency.
    Returns:
      A list of Price directives.
    """
    digits = D("0.01")
    entries = []
    counter = itertools.count()
    for currency in currencies:
        start_price = random.uniform(30, 200)
        growth = random.uniform(0.02, 0.13)  # %/year
        mu = growth * (7 / 365)
        sigma = random.uniform(0.005, 0.02)  # Vol

        for dtime, price_float in zip(
            rrule.rrule(
                rrule.WEEKLY, byweekday=rrule.FR, dtstart=date_begin, until=date_end
            ),
            price_series(start_price, mu, sigma),
        ):
            price = D(price_float).quantize(digits)
            meta = data.new_metadata(generate_prices.__name__, next(counter))
            entry = data.Price(
                meta, dtime.date(), currency, amount.Amount(price, cost_currency)
            )
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
    output = textwrap.dedent(string)
    if strip:
        output = output.strip()
    for from_, to_ in replacements.items():
        if not isinstance(to_, str) and not callable(to_):
            to_ = str(to_)
        output = re.sub(r"\b{}\b".format(from_), to_, output)
    return output


def generate_commodity_entries(date_birth):
    """Create a list of Commodity entries for all the currencies we're using.

    Args:
      date_birth: A datetime.date instance, the date of birth of the user.
    Returns:
      A list of Commodity entries for all the commodities in use.
    """
    return parse(f"""

        1792-01-01 commodity USD
          name: "US Dollar"
          export: "CASH"

        {date_birth} commodity VACHR
          name: "Employer Vacation Hours"
          export: "IGNORE"

        {date_birth} commodity IRAUSD
          name: "US 401k and IRA Contributions"
          export: "IGNORE"

        2009-05-01 commodity RGAGX
          name: "American Funds The Growth Fund of America Class R-6"
          export: "MUTF:RGAGX"
          price: "USD:google/MUTF:RGAGX"

        1995-09-18 commodity VBMPX
          name: "Vanguard Total Bond Market Index Fund Institutional Plus Shares"
          export: "MUTF:VBMPX"
          price: "USD:google/MUTF:VBMPX"

        2004-01-20 commodity ITOT
          name: "iShares Core S&P Total U.S. Stock Market ETF"
          export: "NYSEARCA:ITOT"
          price: "USD:google/NYSEARCA:ITOT"

        2007-07-20 commodity VEA
          name: "Vanguard FTSE Developed Markets ETF"
          export: "NYSEARCA:VEA"
          price: "USD:google/NYSEARCA:VEA"

        2004-01-26 commodity VHT
          name: "Vanguard Health Care ETF"
          export: "NYSEARCA:VHT"
          price: "USD:google/NYSEARCA:VHT"

        2004-11-01 commodity GLD
          name: "SPDR Gold Trust (ETF)"
          export: "NYSEARCA:GLD"
          price: "USD:google/NYSEARCA:GLD"

        1900-01-01 commodity VMMXX
          export: "MUTF:VMMXX (MONEY:USD)"

    """)


def contextualize_file(contents, employer):
    """Replace generic strings in the generated file with realistic strings.

    Args:
      contents: A string, the generic file contents.
    Returns:
      A string, the contextualized version.
    """
    replacements = {
        "CC": "US",
        "Bank1": "BofA",
        "Bank1_Institution": "Bank of America",
        "Bank1_Address": "123 America Street, LargeTown, USA",
        "Bank1_Phone": "+1.012.345.6789",
        "CreditCard1": "Chase:Slate",
        "CreditCard2": "Amex:BlueCash",
        "Employer1": employer,
        "Retirement": "Vanguard",
        "Retirement_Institution": "Vanguard Group",
        "Retirement_Address": "P.O. Box 1110, Valley Forge, PA 19482-1110",
        "Retirement_Phone": "+1.800.523.1188",
        "Investment": "ETrade",
        # Commodities
        "CCY": "USD",
        "VACHR": "VACHR",
        "DEFCCY": "IRAUSD",
        "MFUND1": "VBMPX",
        "MFUND2": "RGAGX",
        "STK1": "ITOT",
        "STK2": "VEA",
        "STK3": "VHT",
        "STK4": "GLD",
    }
    new_contents = replace(contents, replacements)
    return new_contents, replacements


def write_example_file(date_birth, date_begin, date_end, reformat, file):
    """Generate the example file.

    Args:
      date_birth: A datetime.date instance, the birth date of our character.
      date_begin: A datetime.date instance, the beginning date at which to generate
        transactions.
      date_end: A datetime.date instance, the end date at which to generate
        transactions.
      reformat: A boolean, true if we should apply global reformatting to this file.
      file: A file object, where to write out the output.
    """
    # The following code entirely writes out the output to generic names, such
    # as "Employer1", "Bank1", and "CCY" (for principal currency). Those names
    # are purposely chosen to be unique, and only near the very end do we make
    # renamings to more specific and realistic names.

    # Name of the checking account.
    account_opening = "Equity:Opening-Balances"
    account_payable = "Liabilities:AccountsPayable"
    account_checking = "Assets:CC:Bank1:Checking"
    account_credit = "Liabilities:CC:CreditCard1"
    account_retirement = "Assets:CC:Retirement"
    account_investing = "Assets:CC:Investment:Cash"

    # Commodities.
    commodity_entries = generate_commodity_entries(date_birth)

    # Estimate the rent.
    rent_amount = round_to(ANNUAL_SALARY / RENT_DIVISOR, RENT_INCREMENT)

    # Get a random employer.
    employer_name, employer_address = random.choice(EMPLOYERS)

    logging.info("Generating Salary Employment Income")
    income_entries = generate_employment_income(
        employer_name,
        employer_address,
        ANNUAL_SALARY,
        account_checking,
        join(account_retirement, "Cash"),
        date_begin,
        date_end,
    )

    logging.info("Generating Expenses from Banking Accounts")
    banking_expenses = generate_banking_expenses(
        date_begin, date_end, account_checking, rent_amount
    )

    logging.info("Generating Regular Expenses via Credit Card")
    credit_regular_entries = generate_regular_credit_expenses(
        date_birth, date_begin, date_end, account_credit, account_checking
    )

    logging.info("Generating Credit Card Expenses for Trips")
    trip_entries = []
    destinations = sorted(TRIP_DESTINATIONS.items())
    destinations.extend(destinations)
    random.shuffle(destinations)
    for (date_trip_begin, date_trip_end), (destination_name, config) in zip(
        compute_trip_dates(date_begin, date_end), destinations
    ):
        # Compute a suitable tag.
        tag = "trip-{}-{}".format(
            destination_name.lower().replace(" ", "-"), date_trip_begin.year
        )
        # logging.info("%s -- %s %s", tag, date_trip_begin, date_trip_end)

        # Remove regular entries during this trip.
        credit_regular_entries = [
            entry
            for entry in credit_regular_entries
            if not (date_trip_begin <= entry.date < date_trip_end)
        ]

        # Generate entries for the trip.
        this_trip_entries = generate_trip_entries(
            date_trip_begin,
            date_trip_end,
            tag,
            config,
            destination_name.replace("-", " ").title(),
            HOME_NAME,
            account_credit,
        )

        trip_entries.extend(this_trip_entries)

    logging.info("Generating Credit Card Payment Entries")
    credit_payments = generate_clearing_entries(
        delay_dates(
            rrule.rrule(rrule.MONTHLY, dtstart=date_begin, until=date_end, bymonthday=7),
            0,
            4,
        ),
        "CreditCard1",
        "Paying off credit card",
        credit_regular_entries,
        account_credit,
        account_checking,
    )

    credit_entries = credit_regular_entries + trip_entries + credit_payments

    logging.info("Generating Tax Filings and Payments")
    tax_preamble = generate_tax_preamble(date_birth)

    # Figure out all the years we need tax accounts for.
    years = set()
    for account_name in getters.get_accounts(income_entries):
        match = re.match(r"Expenses:Taxes:Y(\d\d\d\d)", account_name)
        if match:
            years.add(int(match.group(1)))

    taxes = [(year, generate_tax_accounts(year, date_end)) for year in sorted(years)]
    tax_entries = tax_preamble + functools.reduce(
        operator.add, (entries for _, entries in taxes)
    )

    logging.info("Generating Opening of Banking Accounts")
    # Open banking accounts and gift the checking account with a balance that
    # will offset all the amounts to ensure a positive balance throughout its
    # lifetime.
    entries_for_banking = data.sorted(
        income_entries + banking_expenses + credit_entries + tax_entries
    )
    minimum = get_minimum_balance(entries_for_banking, account_checking, "CCY")
    banking_entries = generate_banking(
        entries_for_banking, date_begin, date_end, max(-minimum, ZERO)
    )

    logging.info("Generating Transfers to Investment Account")
    banking_transfers = generate_outgoing_transfers(
        data.sorted(
            income_entries
            + banking_entries
            + banking_expenses
            + credit_entries
            + tax_entries
        ),
        account_checking,
        account_investing,
        transfer_minimum=D("200"),
        transfer_threshold=D("3000"),
        transfer_increment=D("500"),
    )

    logging.info("Generating Prices")
    # Generate price entries for investment currencies and create a price map to
    # use for later for generating investment transactions.
    funds_allocation = {"MFUND1": 0.40, "MFUND2": 0.60}
    stocks = ["STK1", "STK2", "STK3", "STK4"]
    price_entries = generate_prices(
        date_begin, date_end, sorted(funds_allocation.keys()) + stocks, "CCY"
    )
    price_map = prices.build_price_map(price_entries)

    logging.info("Generating Employer Match Contribution")
    account_match = "Income:US:Employer1:Match401k"
    retirement_match = generate_retirement_employer_match(
        income_entries, join(account_retirement, "Cash"), account_match
    )

    logging.info("Generating Retirement Investments")
    retirement_entries = generate_retirement_investments(
        income_entries + retirement_match,
        account_retirement,
        sorted(funds_allocation.items()),
        price_map,
    )

    logging.info("Generating Taxes Investments")
    investment_entries = generate_taxable_investment(
        date_begin, date_end, banking_transfers, price_map, stocks
    )

    logging.info("Generating Expense Accounts")
    expense_accounts_entries = generate_expense_accounts(date_birth)

    logging.info("Generating Equity Accounts")
    equity_entries = generate_open_entries(date_birth, [account_opening, account_payable])

    logging.info("Generating Balance Checks")
    credit_checks = generate_balance_checks(
        credit_entries, account_credit, date_random_seq(date_begin, date_end, 20, 30)
    )

    banking_checks = generate_balance_checks(
        data.sorted(
            income_entries
            + banking_entries
            + banking_expenses
            + banking_transfers
            + credit_entries
            + tax_entries
        ),
        account_checking,
        date_random_seq(date_begin, date_end, 20, 30),
    )

    logging.info("Outputting and Formatting Entries")
    dcontext = display_context.DisplayContext()
    default_int_digits = 8
    for currency, precision in {
        "USD": 2,
        "CAD": 2,
        "VACHR": 0,
        "IRAUSD": 2,
        "VBMPX": 3,
        "RGAGX": 3,
        "ITOT": 0,
        "VEA": 0,
        "VHT": 0,
        "GLD": 0,
    }.items():
        int_digits = default_int_digits
        if precision > 0:
            int_digits += 1 + precision
        dcontext.update(D("{{:0{}.{}f}}".format(int_digits, precision).format(0)), currency)

    output = io.StringIO()

    def output_section(title, entries):
        output.write("\n\n\n{}\n\n".format(title))
        printer.print_entries(data.sorted(entries), dcontext, file=output)

    output.write(FILE_PREAMBLE.format(**locals()))
    output_section("* Commodities", commodity_entries)
    output_section("* Equity Accounts", equity_entries)
    output_section(
        "* Banking",
        data.sorted(
            banking_entries + banking_expenses + banking_transfers + banking_checks
        ),
    )
    output_section("* Credit-Cards", data.sorted(credit_entries + credit_checks))
    output_section("* Taxable Investments", investment_entries)
    output_section(
        "* Retirement Investments", data.sorted(retirement_entries + retirement_match)
    )
    output_section("* Sources of Income", income_entries)
    output_section("* Taxes", tax_preamble)
    for year, entries in taxes:
        output_section("** Tax Year {}".format(year), entries)
    output_section("* Expenses", expense_accounts_entries)
    output_section("* Prices", price_entries)
    output_section("* Cash", [])

    logging.info("Contextualizing to Realistic Names")
    contents, replacements = contextualize_file(output.getvalue(), employer_name)
    if reformat:
        contents = format.align_beancount(contents)

    logging.info("Writing contents")
    file.write(contents)

    logging.info("Validating Results")
    validate_output(
        contents,
        [replace(account, replacements) for account in [account_checking]],
        replace("CCY", replacements),
    )


def parse_date_liberally(string, parse_kwargs_dict=None):
    if parse_kwargs_dict is None:
        parse_kwargs_dict = {}
    return dateutil.parser.parse(string, **parse_kwargs_dict).date()


class LiberalDate(click.ParamType):
    name = "date"

    def convert(self, value, param, ctx):
        try:
            parse_date_liberally(value)
        except ValueError:
            self.fail("{!r} is not a valid date".format(value), param, ctx)


@click.command()
@click.option("--date-begin", type=LiberalDate(), help="Beginning date.")
@click.option("--date-end", type=LiberalDate(), help="End date.")
@click.option("--date-birth", type=LiberalDate(), help="Fictional date of birth.")
@click.option("--seed", "-s", type=int, help="Random seed.")
@click.option("--no-reformat", is_flag=True, help="Do not reformat the output.")
@click.option(
    "--output",
    "-o",
    type=click.File("w", encoding="utf-8"),
    default="-",
    help="Output filename.",
)
@click.option("--verbose", "-v", type=bool, help="Produce logging output.")
@click.version_option(message=VERSION)
def main(date_begin, date_end, date_birth, seed, no_reformat, output, verbose):
    """Generate a decently-sized example history, based on some rules.

    This script is used to generate some meaningful input to
    Beancount, input that looks as realistic as possible for a
    moderately complex mock individual. This can also be used as an
    input generator for a stress test for performance evaluation.

    """
    today = datetime.date.today()
    default_years = 2

    date_begin = date_begin or datetime.date(today.year - default_years, 1, 1)
    date_end = date_end or today
    date_birth = date_birth or datetime.date(1980, 5, 12)
    reformat = not no_reformat

    level = logging.DEBUG if verbose else logging.WARN
    logging.basicConfig(level=level, format="%(levelname)-8s: %(message)s")
    if seed is not None:
        logging.info("Seed = %s", seed)
        random.seed(seed)

    write_example_file(date_birth, date_begin, date_end, reformat, file=output)
