#!/usr/bin/env python3
"""Calculate my true returns, including dividends and real costs.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"


# pylint: disable=wrong-import-order

from os import path
from typing import Any, Dict, List, Tuple, Optional
from functools import partial
import collections
import datetime
import io
import logging
import multiprocessing
import os
import subprocess
import tempfile
import typing

from dateutil.relativedelta import relativedelta

import numpy as np

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn
seaborn.set()

from beancount.core import data
from beancount.core import prices
from beancount.core.amount import Amount
from beancount.parser import printer

from returns_config_pb2 import Config
import returns as returnslib
from extract_investments import AccountData
from extract_investments import CashFlow


# Basic type aliases.
Account = str
Currency = str
Date = datetime.date
Array = np.ndarray


# The date at which we evaluate this.
TODAY = Date.today()


CurrencyPair = Tuple[Currency, Currency]


IRR_FORMAT = "{:32}: {:6.2%} ({:6.2%} ex-div, {:6.2%} div)"


STYLE = """
@media print {
    @page { margin: 0in; }
    body { margin: 0.2in; }
    .new-page { page-break-before: always; }
}

body, table { font: 9px Noto Sans, sans-serif; }

p { margin: 0.2em; }

table { border-collapse: collapse; }
table td, table th { border: thin solid black; }

table.full { width: 100%; }

/* p { margin-bottom: .1em } */
"""

RETURNS_TEMPLATE_PRE = """
<html>
  <head>
    <title>{title}</title>
    <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&display=swap" rel="stylesheet">

    <style>
      {style}
    </style>
  <head>
    <body>
      <h1>{title}</h1>
"""
RETURNS_TEMPLATE_POST = """
    </body>
</html>
"""


Table = typing.NamedTuple("Table", [("header", List[str]),
                                    ("rows", List[List[Any]])])

def render_table(table: Table,
                 floatfmt: Optional[str] = None,
                 classes: Optional[str] = None) -> str:
    """Render a simple data table to HTML."""
    oss = io.StringIO()
    fprint = partial(print, file=oss)
    fprint('<table class="{}">'.format(" ".join(classes or [])))
    fprint('<tr>')
    for heading in table.header:
        fprint("<th>{}</th>".format(heading))
    fprint('</tr>')
    for row in table.rows:
        fprint('<tr>')
        for value in row:
            if isinstance(value, float) and floatfmt:
                value = floatfmt.format(value)
            fprint("<td>{}</td>".format(value))
        fprint('</tr>')
    fprint("</table>")
    return oss.getvalue()


# A named date interval: (name, start date, end date).
Interval = Tuple[str, Date, Date]


def compute_returns_table(pricer: returnslib.Pricer,
                          target_currency: Currency,
                          cash_flows_list: List[List[CashFlow]],
                          intervals: List[Interval]):
    """Compute a table of sequential returns."""
    header = ["Return"]
    rows = [["Total"], ["Ex-div"], ["Div"]]
    for intname, date1, date2 in intervals:
        header.append(intname)
        cash_flows = returnslib.truncate_and_merge_cash_flows(pricer, cash_flows_list,
                                                              date1, date2)
        returns = returnslib.compute_returns(cash_flows, pricer, target_currency, date2)
        rows[0].append(returns.total)
        rows[1].append(returns.exdiv)
        rows[2].append(returns.div)
    return Table(header, rows)


def write_returns_pdf(pdf_filename: str, *args, **kwargs) -> subprocess.Popen:
    """Write out returns for a combined list of account account_data.."""
    logging.info("Writing returns file: %s", pdf_filename)

    with tempfile.TemporaryDirectory() as tmpdir:
        indexfile = write_returns_html(tmpdir, *args, **kwargs)
        command = ["google-chrome", "--headless", "--disable-gpu",
                   "--print-to-pdf={}".format(pdf_filename), indexfile]
        subprocess.check_call(command, stderr=open("/dev/null", "w"))
        assert path.exists(pdf_filename)
        logging.info("Done: file://%s", pdf_filename)


def write_returns_html(dirname: str,
                       pricer: returnslib.Pricer,
                       account_data: List[AccountData],
                       title: str,
                       end_date: Date,
                       target_currency: Optional[Currency] = None) -> subprocess.Popen:
    """Write out returns report to a directory with files in it."""

    logging.info("Writing returns dir for %s: %s", title, dirname)
    os.makedirs(dirname, exist_ok=True)
    with open(path.join(dirname, "index.html"), "w") as indexfile:
        fprint = partial(print, file=indexfile)
        fprint(RETURNS_TEMPLATE_PRE.format(style=STYLE, title=title))

        if not target_currency:
            cost_currencies = set(r.cost_currency for r in account_data)
            target_currency = cost_currencies.pop()
            assert not cost_currencies, (
                "Incompatible cost currencies {} for accounts {}".format(
                    cost_currencies, ",".join([r.account for r in account_data])))

        # TOOD(blais): Prices should be plot separately, by currency.
        # fprint("<h2>Prices</h2>")
        # pairs = set((r.currency, r.cost_currency) for r in account_data)
        # plots = plot_prices(dirname, pricer.price_map, pairs)
        # for _, filename in sorted(plots.items()):
        #     fprint('<img src={} style="width: 100%"/>'.format(filename))

        fprint("<h2>Cash Flows</h2>")
        cash_flows_list = [ad.cash_flows for ad in account_data]
        cash_flows = returnslib.truncate_and_merge_cash_flows(
            pricer, cash_flows_list, None, end_date)

        transactions = data.sorted([txn for ad in account_data for txn in ad.transactions])

        returns = returnslib.compute_returns(cash_flows, pricer, target_currency, end_date)
        # Note: This is where the vast majority of the time is spent.
        plots = plot_flows(dirname, pricer.price_map,
                           cash_flows, transactions, returns.total)
        fprint('<img src={} style="width: 100%"/>'.format(plots["flows"]))
        fprint('<img src={} style="width: 100%"/>'.format(plots["cumvalue"]))

        fprint("<h2>Returns</h2>")
        fprint(render_table(Table(["Total", "Ex-Div", "Div"],
                                  [[returns.total, returns.exdiv, returns.div]]),
                            floatfmt="{:.2%}"))

        # Compute table of returns over intervals.
        table = compute_returns_table(pricer, target_currency, cash_flows_list,
                                      get_calendar_intervals(TODAY))
        fprint("<p>", render_table(table, floatfmt="{:.1%}", classes=["full"]), "</p>")

        table = compute_returns_table(pricer, target_currency, cash_flows_list,
                                      get_cumulative_intervals(TODAY))
        fprint("<p>", render_table(table, floatfmt="{:.1%}", classes=["full"]), "</p>")

        fprint('<h2 class="new-page">Accounts</h2>')
        fprint("<p>Cost Currency: {}</p>".format(target_currency))
        for ad in account_data:
            fprint("<p>Account: {} ({})</p>".format(
                ad.account,
                ad.commodity.meta["name"] if ad.commodity else "N/A"))

        fprint(RETURNS_TEMPLATE_POST)

    return indexfile.name


def set_axis(ax_, date_min, date_max):
    """Setup X axis for dates."""

    years = mdates.YearLocator()
    years_fmt = mdates.DateFormatter('%Y')
    months = mdates.MonthLocator()

    ax_.xaxis.set_major_locator(years)
    ax_.xaxis.set_major_formatter(years_fmt)
    ax_.xaxis.set_minor_locator(months)

    if date_min and date_max:
        datemin = np.datetime64(date_min, 'Y')
        datemax = np.datetime64(date_max, 'Y') + np.timedelta64(1, 'Y')
        ax_.set_xlim(datemin, datemax)

    ax_.format_xdata = mdates.DateFormatter('%Y-%m-%d')
    ax_.format_ydata = "{:,}".format
    ax_.grid(True)


def plot_prices(output_dir: str,
                price_map: prices.PriceMap,
                pairs: List[CurrencyPair]) -> Dict[str, str]:
    """Render one or more plots of prices."""

    # Group by quote currencies.
    outplots = {}
    series = collections.defaultdict(list)
    for currency, quote_currency in pairs:
        series[quote_currency].append(currency)

    fig, axs = plt.subplots(len(series), 1, sharex=True, figsize=[10, 2 * len(series)])
    if len(series) == 1: axs = [axs]
    for index, (qc, currencies) in enumerate(sorted(series.items())):
        ax = axs[index]
        for currency in currencies:
            price_points = prices.get_all_prices(price_map, (currency, qc))

            # Render cash flows.
            dates = [date for date, _ in price_points]
            prices_ = [float(price) for _, price in price_points]

            set_axis(ax, dates[0] if dates else None, dates[-1] if dates else None)
            ax.plot(dates, prices_, linewidth=0.3)
            ax.scatter(dates, prices_, s=1.2)

    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["price"] = path.join(output_dir, "price.svg")
    plt.savefig(filename)
    plt.close(fig)

    return outplots


def plot_flows(output_dir: str,
               price_map: prices.PriceMap,
               flows: List[CashFlow],
               transactions: data.Entries,
               returns_rate: float) -> Dict[str, str]:
    """Produce plots from cash flows and returns, and more."""

    # Render cash flows.
    outplots = {}
    dates = [f.date for f in flows]
    dates_exdiv = [f.date for f in flows if not f.is_dividend]
    dates_div = [f.date for f in flows if f.is_dividend]
    #amounts = np.array([f.amount.number for f in flows])
    amounts_exdiv = np.array([f.amount.number for f in flows if not f.is_dividend])
    amounts_div = np.array([f.amount.number for f in flows if f.is_dividend])

    fig, axs = plt.subplots(2, 1, sharex=True, figsize=[10, 4],
                            gridspec_kw={'height_ratios': [3, 1]})
    for ax in axs:
        set_axis(ax, dates[0] if dates else None, dates[-1] if dates else None)
        ax.axhline(0, color='#000', linewidth=0.2)
        ax.vlines(dates_exdiv, 0, amounts_exdiv, linewidth=3, color='#000', alpha=0.7)
        ax.vlines(dates_div, 0, amounts_div, linewidth=3, color='#0A0', alpha=0.7)
    axs[1].set_yscale('symlog')

    axs[0].set_title("Cash Flows")
    axs[1].set_title("log(Cash Flows)")
    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["flows"] = path.join(output_dir, "flows.svg")
    plt.savefig(filename)
    plt.close(fig)

    # Render cumulative cash flows, with returns growth.
    lw = 0.8
    if dates:
        date_min = dates[0] - datetime.timedelta(days=1)
        date_max = dates[-1]
        num_days = (date_max - date_min).days
        dates_all = [dates[0] + datetime.timedelta(days=x) for x in range(num_days)]
        gamounts = np.zeros(num_days)
        rate = (1 + returns_rate) ** (1./365)
        for flow in flows:
            remaining_days = (date_max - flow.date).days
            amt = -float(flow.amount.number)
            if remaining_days > 0:
                gflow = amt * (rate ** np.arange(0, remaining_days))
                gamounts[-remaining_days:] += gflow
            else:
                gamounts[-1] += amt

        fig, ax = plt.subplots(figsize=[10, 4])
        ax.set_title("Cumulative value")
        set_axis(ax, dates[0] if dates else None, dates[-1] if dates else None)
        ax.axhline(0, color='#000', linewidth=lw)

        #ax.scatter(dates_all, gamounts, color='#000', alpha=0.2, s=1.0)
        ax.plot(dates_all, gamounts, color='#000', alpha=0.7, linewidth=lw)

    # Overlay value of assets over time.
    value_dates, value_values = returnslib.compute_portfolio_values(price_map, transactions)
    ax.plot(value_dates, value_values, color='#00F', alpha=0.5, linewidth=lw)
    ax.scatter(value_dates, value_values, color='#00F', alpha=lw, s=2)

    ax.legend(["Amortized value from flows", "Market value"], fontsize="xx-small")
    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["cumvalue"] = path.join(output_dir, "cumvalue.svg")
    plt.savefig(filename)
    plt.close(fig)

    return outplots


def write_price_directives(filename: str,
                           pricer: returnslib.Pricer,
                           days_price_threshold: int):
    """Write a list of required price directives as a Beancount file."""
    price_entries = []
    for (currency, required_date), found_dates in sorted(pricer.required_prices.items()):
        assert len(found_dates) == 1
        cost_currency, actual_date, rate = found_dates.pop()
        days_late = (required_date - actual_date).days
        if days_late < days_price_threshold:
            continue
        price = data.Price({}, required_date, currency, Amount(rate, cost_currency))
        price_entries.append(price)
    os.makedirs(path.dirname(filename), exist_ok=True)
    with open(filename, "w") as prfile:
        printer.print_entries(price_entries, file=prfile)


def get_calendar_intervals(date: Date) -> List[Interval]:
    """Return a list of date pairs for sequential intervals."""
    intervals = [
        (str(year), Date(year, 1, 1), Date(year + 1, 1, 1))
        for year in range(TODAY.year - 15, TODAY.year)]
    intervals.append(
        (str(TODAY.year), Date(TODAY.year, 1, 1), date))
    return intervals


def get_cumulative_intervals(date: Date) -> List[Interval]:
    """Return a list of date pairs for sequential intervals."""
    return [
        ("15_years_ago", Date(date.year - 15, 1, 1), date),
        ("10_years_ago", Date(date.year - 10, 1, 1), date),
        ("5_years_ago", Date(date.year - 5, 1, 1), date),
        ("4_years_ago", Date(date.year - 4, 1, 1), date),
        ("3_years_ago", Date(date.year - 3, 1, 1), date),
        ("2_years_ago", Date(date.year - 2, 1, 1), date),
        ("1_year_ago", Date(date.year - 1, 1, 1), date),
        ("ytd", Date(date.year, 1, 1), date),
        ("rolling_6_months_ago", date - relativedelta(months=6), date),
        ("rolling_3_months_ago", date - relativedelta(months=3), date),
    ]


def generate_reports(account_data_map: Dict[Account, AccountData],
                     config: Config,
                     price_map: prices.PriceMap,
                     end_date: Date,
                     output_dir: str,
                     parallel: bool,
                     pdf: bool) -> returnslib.Pricer:
    """Produce the list of requested reports."""

    pricer = returnslib.Pricer(price_map)

    # Write out a returns file for every account.
    os.makedirs(output_dir, exist_ok=True)
    multiprocessing.set_start_method('fork')
    calls = []
    for report in config.reports.report:
        adlist = [account_data_map[name] for name in report.investment]
        assert isinstance(adlist, list)
        assert all(isinstance(ad, AccountData) for ad in adlist)

        function = write_returns_pdf if pdf else write_returns_html
        filename = path.join(output_dir, report.name)
        if pdf: filename = filename + ".pdf"
        calls.append(partial(
            function, filename, pricer, adlist, report.name,
            end_date,
            report.currency))

    if parallel:
        with multiprocessing.Pool(5) as pool:
            asyns = []
            for func in calls:
                asyns.append(pool.apply_async(func))
            for asyn in asyns:
                asyn.wait()
                assert asyn.successful()
    else:
        for func in calls:
            func()

    return pricer
