#!/usr/bin/env python3
"""List assets and join with attributes of account and commodities to a CSV file.

The purpose of this script is

- Produce a table of postings for the assets and liabilities
- Produce a table of per-account attributes
- Produce a table of per-commodity attributes
- Join these tables
- Output them to a CSV file.

The purpose of this script is to then invoke upload-to-sheets to replace the
contents of an existing sheet inside a Google Sheets doc from which various
reports to track one's portfolio can be produced, and updated with live market
data using the =GOOGLEFINANCE() function.

(In theory, this script eventually be replaceable with an SQL shell query; in
practice, the shell is not quite there yet, so we maintain this script.)
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from typing import NamedTuple, Tuple, List, Set, Any, Dict
from decimal import Decimal
import argparse
import csv
import datetime
import logging
import re

from beancount.core.number import ONE
from beancount.core.number import D
from beancount.core import data
from beancount.core import flags
from beancount.core import account
from beancount.core import account_types
from beancount.core import getters
from beancount.ops import summarize
from beancount.core import prices
from beancount.parser import options
from beancount import loader


def validate_entries(entries: data.Entries):
    """Check for problematic entries resulting from some plugins."""
    for entry in data.filter_txns(entries):
        if entry.flag == flags.FLAG_UNREALIZED:
            raise ValueError("Unrealized entries will fail with this plugin.")


Header = List[str]
Rows = List[List[Any]]
Table = NamedTuple('Table', [('header', Header), ('rows', Rows)])


def get_metamap_table(metamap: Dict[str, data.Directive],
                      attributes: List[str],
                      getter) -> Table:
    """Produce a Table of per-commodity attributes."""
    header = attributes
    attrlist = attributes[1:]
    rows = []
    for key, value in metamap.items():
        row = [key]
        for attr in attrlist:
            row.append(getter(value, attr))
        rows.append(row)
    return Table(attributes, sorted(rows))


def get_commodities_table(entries: data.Entries, attributes: List[str]) -> Table:
    """Produce a Table of per-commodity attributes."""
    commodities = getters.get_commodity_directives(entries)
    header = ['currency'] + attributes
    getter = lambda entry, key: entry.meta.get(key, None)
    table = get_metamap_table(commodities, header, getter)
    return table


def get_accounts_table(entries: data.Entries, attributes: List[str]) -> Table:
    """Produce a Table of per-account attributes."""
    oc_map = getters.get_account_open_close(entries)
    accounts_map = {account: dopen for account, (dopen, _) in oc_map.items()}
    header = ['account'] + attributes
    defaults = {'tax': 'taxable',
                'liquid': False}
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


def abbreviate_account(acc: str, accounts_map: Dict[str, data.Open]):
    """Compute an abbreviated version of the account name."""

    # Get the root of the account by inspecting the "root: TRUE" attribute up
    # the accounts tree.
    racc = acc
    while racc:
        racc = account.parent(racc)
        dopen = accounts_map.get(racc, None)
        if dopen and dopen.meta.get('root', False):
            acc = racc
            break

    # Remove the account type.
    acc = account.sans_root(acc)

    # Remove the two-letter country code if there is one.
    if re.match(r'[A-Z][A-Z]', acc):
        acc = account.sans_root(acc)

    return acc


def get_postings_table(entries: data.Entries, options_map: Dict,
                       accounts_map: Dict[str, data.Open],
                       threshold: Decimal = D('0.01')) -> Table:
    """Enumerate all the postings."""
    header = ['account',
              'account_abbrev',
              'number',
              'currency',
              'cost_number',
              'cost_currency',
              'cost_date']
    balances, _ = summarize.balance_by_account(entries, compress_unbooked=True)
    acctypes = options.get_account_types(options_map)
    rows = []
    for acc, balance in sorted(balances.items()):
        # Keep only the balance sheet accounts.
        acctype = account_types.get_account_type(acc)
        if not acctype in (acctypes.assets, acctypes.liabilities):
            continue

        # Create a posting for each of the positions.
        for pos in balance:
            acc_abbrev = abbreviate_account(acc, accounts_map)
            row = [acc,
                   acc_abbrev,
                   pos.units.number,
                   pos.units.currency,
                   pos.cost.number if pos.cost else ONE,
                   pos.cost.currency if pos.cost else pos.units.currency,
                   pos.cost.date if pos.cost else None]
            rows.append(row)

    return Table(header, rows)


PRICE_Q = D('0.0000001')


def get_prices_table(entries: data.Entries, main_currency: str) -> Table:
    """Enumerate all the prices seen."""
    price_map = prices.build_price_map(entries)
    header = ['currency', 'cost_currency', 'price_file']
    rows = []
    for base_quote in price_map.keys():
        _, price = prices.get_latest_price(price_map, base_quote)
        if price is None:
            continue
        base, quote = base_quote
        rows.append([base, quote, price.quantize(PRICE_Q)])
    return Table(header, rows)


def get_rates_table(entries: data.Entries,
                    currencies: Set[str],
                    main_currency: str) -> Table:
    """Enumerate all the exchange rates."""
    price_map = prices.build_price_map(entries)
    header = ['cost_currency', 'rate_file']
    rows = []
    for currency in currencies:
        _, rate = prices.get_latest_price(price_map, (currency, main_currency))
        if rate is None:
            continue
        rows.append([currency, rate.quantize(PRICE_Q)])
    return Table(header, rows)


def join(main_table: Table, *col_tables: Tuple[Tuple[Tuple[str], Table]]) -> Table:
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
        #indexes_notcol = sorted(set(range(len(col_table.header))) - set(indexes_col))
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


def reorder_columns(table: Table, new_headers: List[str]) -> Table:
    """Reorder the columns of a table to a desired new headers."""
    assert len(table.header) == len(new_headers)
    indexes = [table.header.index(header) for header in new_headers]
    rows = [[row[index] for index in indexes]
            for row in table.rows]
    return Table(new_headers, rows)


def write_table(table: Table, outfile: str):
    """Write a table to a CSV file."""
    with outfile:
        writer = csv.writer(outfile)
        writer.writerow(table.header)
        writer.writerows(table.rows)


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input file')

    parser.add_argument('-C', '--currency', action='store',
                        help=("Override the default output currency "
                              "(default is first operating currency)"))

    parser.add_argument('-n', '--dry-run', action='store_true')

    for shortname, longname in [('-c', 'commodities'),
                                ('-a', 'accounts'),
                                ('-p', 'prices'),
                                ('-r', 'rates'),
                                ('-m', 'postings')]:
        parser.add_argument(
            shortname, '--output_{}'.format(longname),
            type=argparse.FileType('w'),
            help="CSV filename to write out the {} table to.".format(longname))

    parser.add_argument('-o', '--output',
                        type=argparse.FileType('w'),
                        help="CSV filename to write out the final joined table to.")

    args = parser.parse_args()

    # Load the file contents.
    entries, errors, options_map = loader.load_file(args.filename)
    validate_entries(entries)

    # Initialize main output currency.
    main_currency = args.currency or options_map['operating_currency'][0]
    logging.info("Operating currency: %s", main_currency)

    # Get the map of commodities to their meta tags.
    commodities_table = get_commodities_table(
        entries, ['export', 'assetcls', 'strategy', 'issuer'])
    if args.output_commodities is not None:
        write_table(commodities_table, args.output_commodities)

    # Get a table of the commodity names.
    #
    # Note: We're fetching the table separately in order to avoid changes to the
    # spreadsheet upstream, and want to tack on the values as new columns on the
    # right.
    names_table = get_commodities_table(entries, ['name'])

    # Get the map of accounts to their meta tags.
    accounts_table, accounts_map = get_accounts_table(
        entries, ['tax', 'liquid'])
    if args.output_accounts is not None:
        write_table(accounts_table, args.output_accounts)

    # Enumerate the list of assets.
    postings_table = get_postings_table(entries, options_map, accounts_map)
    if args.output_postings is not None:
        write_table(postings_table, args.output_postings)

    # Get the list of prices.
    prices_table = get_prices_table(entries, main_currency)
    if args.output_prices is not None:
        write_table(prices_table, args.output_prices)

    # Get the list of exchange rates.
    index = postings_table.header.index('cost_currency')
    currencies = set(row[index] for row in postings_table.rows)
    rates_table = get_rates_table(entries, currencies, main_currency)
    if args.output_rates is not None:
        write_table(rates_table, args.output_rates)

    # Join all the tables.
    joined_table = join(postings_table,
                        (('currency',), commodities_table),
                        (('account',), accounts_table),
                        (('currency', 'cost_currency'), prices_table),
                        (('cost_currency',), rates_table),
                        (('currency',), names_table))

    # Reorder columns.
    # We do this in order to avoid having to change the spreadsheet when we add new columns.
    headers = list(joined_table.header)
    headers.remove('issuer')
    headers.append('issuer')
    final_table = reorder_columns(joined_table, headers)

    # Filter table.
    rows = [row for row in final_table.rows if row[7].lower() != 'ignore']
    table = Table(final_table.header, rows)

    if args.output is not None:
        table[0][0] += ' ({:%Y-%m-%d %H:%M})'.format(datetime.datetime.now())
        write_table(table, args.output)

    return 0


if __name__ == '__main__':
    main()
