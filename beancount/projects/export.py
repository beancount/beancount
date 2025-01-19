#!/usr/bin/env python3
"""List assets and attributes of account and commodities.

This script:

- produces a table of postings for the assets and liabilities,

- produces a table of per-account attributes,

- produces a table of per-commodity attributes,

- joins these tables,

- outputs them to a CSV file.

The upload-to-sheets program can then be used to replace the contents
of an existing sheet inside a Google Sheets doc from which various
reports to track one's portfolio can be produced, and updated with
live market data using the =GOOGLEFINANCE() function.

In theory, this script could eventually be replaced with an Beancount
Query Language query. However, BQL is not there yet.

"""

__copyright__ = "Copyright (C) 2018, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import csv
import datetime
import logging
import re
import sys
from decimal import Decimal
from typing import Any
from typing import NamedTuple

import click

from beancount import loader
from beancount.core import account
from beancount.core import account_types
from beancount.core import data
from beancount.core import getters
from beancount.core import prices
from beancount.core.number import ONE
from beancount.core.number import D
from beancount.ops import summarize
from beancount.parser import options

Header = list[str]
Rows = list[list[Any]]


class Table(NamedTuple):
    header: Header
    rows: Rows


def get_metamap_table(
    metamap: dict[str, data.Directive], attributes: list[str], getter
) -> Table:
    """Produce a Table of per-commodity attributes."""
    attrlist = attributes[1:]
    rows = []
    for key, value in metamap.items():
        row = [key]
        for attr in attrlist:
            row.append(getter(value, attr))
        rows.append(row)
    return Table(attributes, sorted(rows))


def get_commodities_table(entries: data.Entries, attributes: list[str]) -> Table:
    """Produce a Table of per-commodity attributes."""
    commodities = getters.get_commodity_directives(entries)
    header = ["currency"] + attributes
    getter = lambda entry, key: entry.meta.get(key, None)
    table = get_metamap_table(commodities, header, getter)
    return table


def get_accounts_table(entries: data.Entries, attributes: list[str]) -> Table:
    """Produce a Table of per-account attributes."""
    oc_map = getters.get_account_open_close(entries)
    accounts_map = {account: dopen for account, (dopen, _) in oc_map.items()}
    header = ["account"] + attributes
    defaults = {"tax": "taxable", "liquid": False}

    def getter(entry, key):
        """Lookup the value working up the accounts tree."""
        value = entry.meta.get(key, None)
        if value is not None:
            return value
        account_name = account.parent(entry.account)
        if not account_name:
            return defaults.get(key, None)
        parent_entry = accounts_map.get(account_name, None)
        if not parent_entry:
            return defaults.get(key, None)
        return getter(parent_entry, key)

    return get_metamap_table(accounts_map, header, getter), accounts_map


def abbreviate_account(acc: str, accounts_map: dict[str, data.Open]):
    """Compute an abbreviated version of the account name."""

    # Get the root of the account by inspecting the "root: TRUE" attribute up
    # the accounts tree.
    racc = acc
    while racc:
        racc = account.parent(racc)
        dopen = accounts_map.get(racc, None)
        if dopen and dopen.meta.get("root", False):
            acc = racc
            break

    # Remove the account type.
    acc = account.sans_root(acc)

    # Remove the two-letter country code if there is one.
    if re.match(r"[A-Z][A-Z]", acc):
        acc = account.sans_root(acc)

    return acc


def get_postings_table(
    entries: data.Entries,
    options_map: dict,
    accounts_map: dict[str, data.Open],
    threshold: Decimal = D("0.01"),
) -> Table:
    """Enumerate all the postings."""
    header = [
        "account",
        "account_abbrev",
        "number",
        "currency",
        "cost_number",
        "cost_currency",
        "cost_date",
    ]
    balances, _ = summarize.balance_by_account(entries, compress_unbooked=True)
    acctypes = options.get_account_types(options_map)
    rows = []
    for acc, balance in sorted(balances.items()):
        # Keep only the balance sheet accounts.
        acctype = account_types.get_account_type(acc)
        if acctype not in (acctypes.assets, acctypes.liabilities):
            continue

        # Create a posting for each of the positions.
        for pos in balance:
            acc_abbrev = abbreviate_account(acc, accounts_map)
            row = [
                acc,
                acc_abbrev,
                pos.units.number,
                pos.units.currency,
                pos.cost.number if pos.cost else ONE,
                pos.cost.currency if pos.cost else pos.units.currency,
                pos.cost.date if pos.cost else None,
            ]
            rows.append(row)

    return Table(header, rows)


PRICE_Q = D("0.0000001")


def get_prices_table(entries: data.Entries, main_currency: str) -> Table:
    """Enumerate all the prices seen."""
    price_map = prices.build_price_map(entries)
    header = ["currency", "cost_currency", "price_file"]
    rows = []
    for base_quote in price_map.keys():
        _, price = prices.get_latest_price(price_map, base_quote)
        if price is None:
            continue
        base, quote = base_quote
        rows.append([base, quote, price.quantize(PRICE_Q)])
    return Table(header, rows)


def get_rates_table(
    entries: data.Entries, currencies: set[str], main_currency: str
) -> Table:
    """Enumerate all the exchange rates."""
    price_map = prices.build_price_map(entries)
    header = ["cost_currency", "rate_file"]
    rows = []
    for currency in currencies:
        _, rate = prices.get_latest_price(price_map, (currency, main_currency))
        if rate is None:
            continue
        rows.append([currency, rate.quantize(PRICE_Q)])
    return Table(header, rows)


def join(main_table: Table, *col_tables: tuple[tuple[tuple[str], Table]]) -> Table:
    """Join a table with a number of other tables.
    col_tables is a tuple of (column, table) pairs."""

    new_header = list(main_table.header)
    for cols, col_table in col_tables:
        header = list(col_table.header)
        for col in cols:
            assert col in main_table.header
            header.remove(col)
        new_header.extend(header)

    col_maps = []
    for cols, col_table in col_tables:
        indexes_main = [main_table.header.index(col) for col in cols]
        indexes_col = [col_table.header.index(col) for col in cols]
        # indexes_notcol = sorted(set(range(len(col_table.header))) - set(indexes_col))
        col_map = {}
        for row in col_table.rows:
            key = tuple(row[index] for index in indexes_col)
            col_map[key] = row
        assert len(col_map) == len(col_table.rows), cols
        col_maps.append((indexes_main, indexes_col, col_map))

    rows = []
    for row in main_table.rows:
        row = list(row)
        empty_row = [None] * (len(col_table.header) - len(indexes_col))
        for indexes_main, indexes_col, col_map in col_maps:
            key = tuple(row[index] for index in indexes_main)
            other_row = col_map.get(key, None)
            if other_row is not None:
                other_row = list(other_row)
                for index in reversed(indexes_col):
                    del other_row[index]
            else:
                other_row = empty_row
            row.extend(other_row)
        rows.append(row)

    return Table(new_header, rows)


def reorder_columns(table: Table, new_headers: list[str]) -> Table:
    """Reorder the columns of a table to a desired new headers."""
    assert len(table.header) == len(new_headers)
    indexes = [table.header.index(header) for header in new_headers]
    rows = [[row[index] for index in indexes] for row in table.rows]
    return Table(new_headers, rows)


def write_table(table: Table, outfile: str):
    """Write a table to a CSV file."""
    writer = csv.writer(outfile)
    writer.writerow(table.header)
    writer.writerows(table.rows)


# python 3.15 will make utf-8 mode enabled by default
# so use a locale encoding for backward compatibility
if sys.version_info < (3, 10):
    # locale encoding is added in 3.10
    OUTPUT_ENCODING = None
else:
    OUTPUT_ENCODING = "locale"


@click.command(help=__doc__)
@click.argument("filename")
@click.option(
    "--currency", "-C", help="Output currency (default is first operating currency)."
)
@click.option(
    "--ignore-options",
    is_flag=True,
    help=(
        "Ignore options symbols before export. "
        "This assumes a separate options trading strategy."
    ),
)
@click.option("--dry-run", "-n", is_flag=True)
@click.option(
    "--insert-date", is_flag=True, help="Insert the date in the header of the output."
)
@click.option(
    "--output",
    "-o",
    type=click.File("w", encoding=OUTPUT_ENCODING),
    help="CSV filename to write out the final joined table to.",
)
@click.option(
    "--output_commodities",
    "-c",
    type=click.File("w", encoding=OUTPUT_ENCODING),
    help="CSV filename to write out the commodities table to.",
)
@click.option(
    "--output_accounts",
    "-a",
    type=click.File("w", encoding=OUTPUT_ENCODING),
    help="CSV filename to write out the accounts table to.",
)
@click.option(
    "--output_prices",
    "-p",
    type=click.File("w", encoding=OUTPUT_ENCODING),
    help="CSV filename to write out the prices table to.",
)
@click.option(
    "--output_rates",
    "-r",
    type=click.File("w", encoding=OUTPUT_ENCODING),
    help="CSV filename to write out the rates table to.",
)
@click.option(
    "--output_postings",
    "-m",
    type=click.File("w", encoding=OUTPUT_ENCODING),
    help="CSV filename to write out the postings table to.",
)
def main(
    filename,
    currency,
    ignore_options,
    dry_run,
    insert_date,
    output,
    output_commodities,
    output_accounts,
    output_prices,
    output_rates,
    output_postings,
):
    # Load the file contents.
    entries, errors, options_map = loader.load_file(filename)

    # Initialize main output currency.
    main_currency = currency or options_map["operating_currency"][0]
    logging.info("Operating currency: %s", main_currency)

    # Get the map of commodities to their meta tags.
    commodities_table = get_commodities_table(
        entries, ["export", "assetcls", "strategy", "issuer"]
    )
    if output_commodities is not None:
        write_table(commodities_table, output_commodities)

    # Get a table of the commodity names.
    #
    # Note: We're fetching the table separately in order to avoid changes to the
    # spreadsheet upstream, and want to tack on the values as new columns on the
    # right.
    names_table = get_commodities_table(entries, ["name"])

    # Get the map of accounts to their meta tags.
    accounts_table, accounts_map = get_accounts_table(entries, ["tax", "liquid"])
    if output_accounts is not None:
        write_table(accounts_table, output_accounts)

    # Enumerate the list of assets.
    postings_table = get_postings_table(entries, options_map, accounts_map)
    if output_postings is not None:
        write_table(postings_table, output_postings)

    # Get the list of prices.
    prices_table = get_prices_table(entries, main_currency)
    if output_prices is not None:
        write_table(prices_table, output_prices)

    # Get the list of exchange rates.
    index = postings_table.header.index("cost_currency")
    currencies = set(row[index] for row in postings_table.rows)
    rates_table = get_rates_table(entries, currencies, main_currency)
    if output_rates is not None:
        write_table(rates_table, output_rates)

    # Join all the tables.
    joined_table = join(
        postings_table,
        (("currency",), commodities_table),
        (("account",), accounts_table),
        (("currency", "cost_currency"), prices_table),
        (("cost_currency",), rates_table),
        (("currency",), names_table),
    )

    # Reorder columns.
    # We do this in order to avoid having to change the spreadsheet when we add new columns.
    headers = list(joined_table.header)
    headers.remove("issuer")
    headers.append("issuer")
    final_table = reorder_columns(joined_table, headers)

    # Filter table removing rows to ignore (rows not to export).
    index = final_table.header.index("export")
    rows = [
        row
        for row in final_table.rows
        if row[index] is None or row[index].lower() != "ignore"
    ]

    # Filter out options if requested.
    if ignore_options:
        index = final_table.header.index("currency")
        is_option = re.compile(r"[A-Z]+_\d{6,}[CP]\d+", re.I).match
        rows = [row for row in rows if row[index] is None or not is_option(row[index])]

    table = Table(final_table.header, rows)

    if output is not None:
        if insert_date:
            table[0][0] += " ({:%Y-%m-%d %H:%M})".format(datetime.datetime.now())
        write_table(table, output)


if __name__ == "__main__":
    main()
