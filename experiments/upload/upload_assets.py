#!/usr/bin/env python3
"""Upload your list of assets to a Google Spreadsheet.

The purpose of this is manyfold:

- To fully automate the updating of an online source of assets. Using the
  'exportpf' report is annoying because the user has to manually upload an OFX
  to Google Finance, deleting and recreating a portfolio. These manual steps are
  annoying.

- Google Finace is limited; I think upload all the positions to a spreadsheet
  and letting the user recreate the views they need using the GOOGLEFINANCE()
  function can produce an equivalent, or better.

- The 'exportpf' code uses Holding, and I intend on removing this object in
  favor of a simplified Position object from the 'booking' branch. This code
  intends to be a bit of a simplified reboot of the export code.

"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import bisect
import csv
import collections
import codecs
import logging
import os
import re
import unittest
import json
import pprint
from os import path

from beancount.core.number import ZERO
from beancount.core.number import ONE
from beancount.core import data
from beancount.core import flags
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import account
from beancount.core import account_types
from beancount.core import getters
from beancount.ops import summarize
from beancount.ops import prices
from beancount.parser import options
from beancount import loader

import oauth2client.client
from oauth2client import tools
from oauth2client.file import Storage
from oauth2client.client import SignedJwtAssertionCredentials
import gspread


def clean_entries_for_balances(entries):
    """
    Remove the entries inserted by unrealized gains/losses. Those entries do
    affect asset accounts, and we don't want them to appear in holdings.

    Note: Perhaps it would make sense to generalize this concept of "inserted
    unrealized gains."
    """
    return [entry
            for entry in entries
            if (not isinstance(entry, data.Transaction) or
                entry.flag != flags.FLAG_UNREALIZED)]


def get_assets(entries, options_map):
    """Enumerate all the assets and liabilities.

    Args:
      entries: A list of directives, as per the loader.
      options_map: An options map, as per the parser.
    Yields:
      Instances of Posting.
    """
    balances, _ = summarize.balance_by_account(entries)
    date = entries[-1].date
    acctypes = options.get_account_types(options_map)
    for account, balance in sorted(balances.items()):
        # Keep only the balance sheet accounts.
        acctype = account_types.get_account_type(account)
        if not acctype in (acctypes.assets, acctypes.liabilities):
            continue
        # Create a posting for each of the positions.
        for position in balance:
            yield data.Posting(account, position.units, position.cost, None, None, None)


def add_prices_to_postings(entries, postings):
    # Compute a price map, to extract most current prices.
    price_map = prices.build_price_map(entries)
    for posting in postings:
        if posting.cost is not None:
            cbase = posting.units.currency
            cquote = posting.cost.currency
            (price_date,
             price_number) = prices.get_price(price_map, (cbase, cquote), None)
            posting = posting._replace(price=amount.Amount(price_number, cquote))
        yield posting


def get_connection():
    """Connect to the Drive API, with authentication.

    Returns:
      An initialized gspread.client.Client instance ready for combat.
    """
    scopes = ['https://spreadsheets.google.com/feeds']
    json_filename = path.join(os.environ['HOME'], '.google-apis-service-account.json')
    json_key = json.load(open(json_filename))
    credentials = oauth2client.client.SignedJwtAssertionCredentials(
        json_key['client_email'],
        json_key['private_key'].encode(),
        scopes)
    return gspread.authorize(credentials)


class Model:
    """A model of the spreadsheet that makes it easy to configure the output."""

    def __init__(self, postings, exports, asset_type, cash_currency='USD'):
        self.cash_currency = cash_currency
        self.postings = postings
        self.exports = exports
        self.asset_type = asset_type

        columns = [
            ('Account', self._account),
            ('Currency', self._units_currency),
            ('Symbol', self._symbol),
            ('Cost Currency', self._cost_currency),
            ('Units', self._units_number),
            ('Unit Cost', self._cost_number),
            ('Unit Price', self._price_number),
            ('Conversion Factor', self._conversion),
            ('Asset Type', self._asset_type),
            # ('Label', self._cost_label),
            # ('Date', self._cost_date),
            # ('Price Currency', self._price_currency),
        ]
        self.columns = {index: head_fun
                        for index, head_fun in enumerate(columns, start=1)}

    def num_rows(self):
        return len(self.postings) + 1

    def num_cols(self):
        return len(self.columns)

    def get(self, row, col):
        header, method = self.columns[col]
        if row == 1:
            value = header
        else:
            posting = self.postings[row-2]
            value = method(posting)
        return value

    def _account(self, posting):
        return posting.account

    def _symbol(self, posting):
        symbol = self.exports.get(posting.units.currency, '')
        return ('' if symbol in ('CASH', 'IGNORE')
                else symbol)

    def _units_number(self, posting):
        return posting.units.number

    def _units_currency(self, posting):
        return posting.units.currency

    def _cost_number(self, posting):
        cost = posting.cost
        return cost.number if cost is not None else ''

    def _cost_currency(self, posting):
        cost = posting.cost
        return cost.currency if cost is not None else ''

    def _cost_date(self, posting):
        date = posting.cost.date if posting.cost else None
        return date or ''

    def _cost_label(self, posting):
        return ''  # Not supported yet; see booking branch.

    def _price_number(self, posting):
        price = posting.price
        return price.number if price is not None else ''

    def _price_currency(self, posting):
        price = posting.price
        return price.currency if price is not None else ''

    CURRENCY_MAP = {'NIS': 'ILS'}

    def _conversion(self, posting):
        currency = (posting.units.currency
                    if posting.cost is None else
                    posting.cost.currency)

        if self.exports.get(currency, None) in 'IGNORE':
            return ZERO

        currency = self.CURRENCY_MAP.get(currency, currency)
        if currency == self.cash_currency:
            return ONE
        else:
            return '=GOOGLEFINANCE("CURRENCY:{}{}")'.format(currency, self.cash_currency)

    def _asset_type(self, posting):
        return self.asset_type.get(posting.units.currency)


def get_root_accounts(postings):
    """Compute a mapping of accounts to root account name.

    This removes sub-accounts where the leaf of the account name is equal to the
    currency held within, and all other leaf accounts of such root accounts.

    Args:
      postings: A list of Posting instances.
    Returns:
      A dict of account names to root account names.
    """
    roots = {posting.account: (account.parent(posting.account)
                               if (account.leaf(posting.account) ==
                                   posting.units.currency)
                               else posting.account)
             for posting in postings}
    values = set(roots.values())
    for root in list(roots):
        parent = account.parent(root)
        if parent in values:
            roots[root] = parent
    return roots


def aggregate_postings(postings):
    """Aggregate postings by account and currency. Handle cash by aggregating by currency.

    Args:
      postings: A list of Posting instances.
    Returns:
      A list of aggregated postings.
    """
    balances = collections.defaultdict(inventory.Inventory)
    for posting in postings:
        # key = ('Multiple' if posting.cost is None else posting.account,
        #        posting.units.currency)
        # balances[key].add_position(posting)
        key = (posting.account, posting.units.currency)
        balances[key].add_position(posting)

    agg_postings = []
    for (account, currency), balance in balances.items():
        units = balance.units()
        assert len(units) == 1
        units = units[0].units

        cost = balance.cost()
        assert len(cost) == 1
        total_cost = cost[0].units

        average_cost = position.Cost(total_cost.number/units.number,
                                     total_cost.currency,
                                     None, None)
        posting = data.Posting(account, units, average_cost, None, None, None)
        agg_postings.append(posting)

    return agg_postings


def upload_postings_to_sheet(model, doc, index):
    """Upload a model to a spreadsheet.

    Args:
      model: An instance of Model.
      doc: A gspread.Spreadsheet instance.
      index: An index of a sheet in the spreadsheet.
    """
    sheet = doc.get_worksheet(index)
    sheet.resize(model.num_rows(), model.num_cols())
    cells = sheet.range(':'.join([sheet.get_addr_int(1, 1),
                                  sheet.get_addr_int(model.num_rows(),
                                                     model.num_cols())]))
    for cell in cells:
        cell.value = model.get(cell.row, cell.col)
    sheet.update_cells(cells)


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input file')
    parser.add_argument('docid', help="Spreadsheets doc id to update")
    args = parser.parse_args()

    # Connect to the API.
    gc = get_connection()
    doc = gc.open_by_key(args.docid)
    # Note: You have to share the sheet with the "client_email" address.

    # Load the file contents.
    entries, errors, options_map = loader.load_file(args.filename)

    # Enumerate the list of assets.
    def keyfun(posting):
        if posting.cost is None:
            return (1, posting.units.currency, posting.account)
        else:
            return (0, posting.account, posting.cost.currency)

    postings = sorted(get_assets(clean_entries_for_balances(entries), options_map),
                      key=keyfun)

    # Simplify the accounts to their root accounts.
    root_accounts = get_root_accounts(postings)
    postings = [posting._replace(account=root_accounts[posting.account])
                for posting in postings]

    # Aggregate postings by account/currency.
    agg_postings = sorted(aggregate_postings(postings), key=keyfun)

    # Add prices to the postings.
    agg_postings = add_prices_to_postings(entries, agg_postings)

    # Get the map of commodities to export meta tags.
    commodities_map = getters.get_commodity_map(entries)
    exports = getters.get_values_meta(commodities_map, 'export')
    asset_type = getters.get_values_meta(commodities_map, 'assets')

    # Create models and update the sheets.
    upload_postings_to_sheet(Model(list(agg_postings), exports, asset_type), doc, 0)
    #upload_postings_to_sheet(Model(postings, exports), doc, 0)


if __name__ == '__main__':
    main()
