#!/usr/bin/env python3
"""Library code to compute returns from series of cash flows.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

from typing import List, Optional, Tuple
import collections
import datetime
import itertools
import typing

import numpy as np
from scipy.optimize import fsolve

from beancount.core import convert
from beancount.core import data
from beancount.core import prices
from beancount.core.number import ZERO
from beancount.core.inventory import Inventory

from investments import AccountData
from investments import CashFlow
from investments import Cat
from investments import compute_balance_at


# Basic type aliases.
Account = str
Currency = str
Date = datetime.date
Array = np.ndarray


class Pricer:
    """A price database that remembers the queried prices and dates."""

    def __init__(self, price_map: prices.PriceMap):
        self.price_map = price_map
        self.required_prices = collections.defaultdict(set)

    def get_value(self, pos, date):
        """Return the value and save the conversion rate."""
        price_dates = []
        price = convert.get_value(pos, self.price_map, date, price_dates)

        # Add prices found to the list of queried ones.
        for found_date, found_rate in price_dates:
            self.required_prices[(pos.units.currency, date)].add(
                (price.currency, found_date, found_rate))

        return price

    def convert_amount(self, amount, target_currency, date):
        """Convert an amount to a specific currency."""
        # TODO(blais): Save the amount here too.
        return convert.convert_amount(amount, target_currency, self.price_map, date=date)


def net_present_value(irr: float, cash_flows: Array, years: Array) -> float:
    """Net present value; objective function for optimizer."""
    return np.sum(cash_flows / (1. + irr) ** years)


def compute_irr(dated_flows: List[CashFlow],
                pricer: Pricer,
                target_currency: Currency,
                end_date: Date) -> float:
    """Compute the irregularly spaced IRR."""

    # Array of cash flows, converted to USD.
    usd_flows = []
    for flow in dated_flows:
        usd_amount = pricer.convert_amount(flow.amount, target_currency, date=flow.date)
        usd_flows.append(float(usd_amount.number))
    cash_flows = np.array(usd_flows)

    # Array of time in years.
    years = []
    for flow in dated_flows:
        years.append((flow.date - end_date).days / 365)
    years = np.array(years)

    # Start with something reasonably normal.
    estimated_irr = 0.20

    # Solve for the root of the NPV equation.
    irr, unused_infodict, unused_ier, unused_mesg = fsolve(
        net_present_value, x0=estimated_irr, args=(cash_flows, years),
        full_output=True)
    return irr.item()


Returns = typing.NamedTuple("Returns", [
    ('groupname', str),
    ('first_date', Date),
    ('last_date', Date),
    ('years', float),
    ("total", float),
    ("exdiv", float),
    ("div", float),
    ("flows", List[CashFlow]),
])


def compute_returns(flows: List[CashFlow],
                    pricer: Pricer,
                    target_currency: Currency,
                    end_date: Date) -> Returns:
    """Compute the returns from a list of cash flows."""
    if not flows:
        return Returns("?", Date.today(), Date.today(), 0, 0, 0, 0, [])

    flows = sorted(flows, key=lambda cf: cf.date)
    irr = compute_irr(flows, pricer, target_currency, end_date)

    flows_exdiv = [flow for flow in flows if not flow.is_dividend]
    irr_exdiv = compute_irr(flows_exdiv, pricer, target_currency, end_date)

    first_date = flows[0].date
    last_date = flows[-1].date
    years = (last_date - first_date).days / 365
    return Returns("?", first_date, last_date, years,
                   irr, irr_exdiv, (irr - irr_exdiv),
                   flows)


def truncate_cash_flows(
        pricer: Pricer,
        account_data: AccountData,
        date_start: Optional[Date],
        date_end: Optional[Date]) -> List[CashFlow]:
    """Truncate the cash flows for the given account data."""

    start_flows = []
    end_flows = []

    if date_start is not None:
        # Truncate before the start date.
        balance = compute_balance_at(account_data.transactions, date_start)
        if not balance.is_empty():
            cost_balance = balance.reduce(pricer.get_value, date_start)
            cost_position = cost_balance.get_only_position()
            if cost_position:
                start_flows.append(
                    CashFlow(date_start, -cost_position.units, False,
                             "open", account_data.account))

    if date_end is not None:
        # Truncate after the end date.
        # Note: Avoid redundant balance iteration by computing it once and
        # caching it on every single transaction.
        balance = compute_balance_at(account_data.transactions, date_end)
        if not balance.is_empty():
            cost_balance = balance.reduce(pricer.get_value, date_end)
            cost_position = cost_balance.get_only_position()
            if cost_position:
                end_flows.append(
                    CashFlow(date_end, cost_position.units, False,
                             "close", account_data.account))

    # Compute truncated flows.
    truncated_flows = []
    for flow in account_data.cash_flows:
        if date_start and flow.date < date_start:
            continue
        if date_end and flow.date >= date_end:
            break
        truncated_flows.append(flow)

    cash_flows = start_flows + truncated_flows + end_flows

    cash_flows_dates = [cf.date for cf in cash_flows]
    assert cash_flows_dates == sorted(cash_flows_dates)
    return cash_flows


def truncate_and_merge_cash_flows(
        pricer: Pricer,
        account_data_list: List[AccountData],
        date_start: Optional[Date],
        date_end: Optional[Date]) -> List[CashFlow]:
    """Truncate and merge the cash flows for given list of account data."""
    cash_flows = []
    for ad in account_data_list:
        cash_flows.extend(truncate_cash_flows(pricer, ad, date_start, date_end))
    cash_flows.sort(key=lambda item: item[0])
    return cash_flows


def compute_portfolio_values(price_map: prices.PriceMap,
                             transactions: data.Entries) -> Tuple[List[Date], List[float]]:
    """Compute a serie of portfolio values over time."""

    # Infer the list of required prices.
    currency_pairs = set()
    for entry in transactions:
        for posting in entry.postings:
            if posting.meta["category"] is Cat.ASSET:
                if posting.cost:
                    currency_pairs.add((posting.units.currency, posting.cost.currency))

    first = lambda x: x[0]
    price_dates = sorted(itertools.chain(
        ((date, None)
         for pair in currency_pairs
         for date, _ in prices.get_all_prices(price_map, pair)),
        ((entry.date, entry)
         for entry in transactions)), key=first)

    # Iterate computing the balance.
    value_dates = []
    value_values = []
    balance = Inventory()
    for date, group in itertools.groupby(price_dates, key=first):
        # Update balances.
        for _, entry in group:
            if entry is None:
                continue
            for posting in entry.postings:
                if posting.meta["category"] is Cat.ASSET:
                    balance.add_position(posting)

        # Convert to market value.
        value_balance = balance.reduce(convert.get_value, price_map, date)
        cost_balance = value_balance.reduce(convert.convert_position, "USD", price_map)
        pos = cost_balance.get_only_position()
        value = pos.units.number if pos else ZERO

        # Add one data point.
        value_dates.append(date)
        value_values.append(value)

    return value_dates, value_values
