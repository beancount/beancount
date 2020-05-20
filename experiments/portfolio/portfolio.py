#!/usr/bin/env python3
"""A dashboard for portfolio composition and changes.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import functools
import logging

from beancount.core.number import D
from beancount import loader
from beancount.core import prices
from beancount.ops import holdings
from beancount.reports import holdings_reports
from beancount.parser import options


def join(holdings_list, features_map, keyfun):
    """Join a list of holdings with a vector of arbitrary features.

    This function joins a list of holdings with a features_map, where the join
    key between the two mappings is provided by a configuration callable
    'keyfun'. keyfun() is run on every holding, and the resulting key is looked
    up from features_map.

    All the values from features_map are first normalized to 100%, and when
    aggregating, the 'market_value' attribute of the holdings is multiplied by
    the normalized feature value.

    Args:
      holdings_list: A list of holdings.Holding instances.
      features_map: A dict of some key to a dict of features. The values are
        dicts of strings to a number.
      keyfun: A function that produces the join key from a Holding instance.
        The key is used to look up a feature vector from the features_map dict.
    Returns:
      A dict of labels (from the values of features_map) to a pair of: Decimal
      number of market_value amounts, and a list of corresponding scaled
      holdings.
    """
    # Get the full list of features.
    all_labels = set(label
                     for features in features_map.values()
                     for label in features)
    features_total = {label: D('0') for label in all_labels}
    features_holdings = {label: [] for label in all_labels}

    # Normalize the feature vectors.
    norm_features_map = {key: normalize_features(features)
                         for key, features in features_map.items()}

    # Accumulate the market value of each holding in buckets for each label.
    for holding in holdings_list:
        key = keyfun(holding)

        if key is None:
            logging.debug("Key not found: %s, %s, %s",
                          holding.account, holding.currency, holding.cost_currency)

        try:
            features = norm_features_map[key]
        except KeyError as exc:
            raise KeyError("Key '{}' not found in map {}".format(key, norm_features_map))
        for label, fraction in features.items():
            if not holding.market_value:
                continue
            scaled_holding = holdings.scale_holding(holding, D(fraction))
            features_total[label] += scaled_holding.market_value
            features_holdings[label].append(scaled_holding)

    return {label: (features_total[label], features_holdings[label])
            for label in all_labels}


def normalize_features(features_dict):
    """Normalize the values of a dictionary to sum to 1.0.

    Args:
      features_dict: A dict of any type of key to a number.
    Returns:
      A new dict, whose values have been scaled so that they
      all sum to 1.0.
    """
    total_values = sum(features_dict.values())
    return {label: value/total_values
            for label, value in features_dict.items()}


def getitem_startswith(adict, key):
    """A dict getter which returns the longest key of dict matching the start of 'key'.

    Args:
      adict: a dict object.
      key: A string, the value to match against the dict keys.
    Returns:
      A key and value from 'adict'.
    """
    assert isinstance(key, str)
    longest_key = None
    longest_value = None
    for dict_key, dict_value in adict.items():
        if dict_key and key.startswith(dict_key):
            if (not longest_key or longest_key < dict_key):
                longest_key = dict_key
                longest_value = dict_value
    return longest_key, longest_value


def holding_account_prefix_getter(features_map, holding):
    """Return first key from features_map that matches the holding's account name.

    Args:
      features_map: A dict of key to features (also dicts).
      holding: An instance of Holding.
    Returns:
      One of the keys of features_map.
    """
    key, _ = getitem_startswith(features_map, holding.account)
    return key


def print_features(title, features, currency, relative=False, print_holdings=False):
    """Print a features aggregation.

    Args:
      title: A string, the title to print for this section.
      features: A dict of label strings to (number, list of scaled holdings).
      currency: A string, the currency to output.
      print_holdings: A boolean, if true, print the holdings detail in each category.
    """
    if not features:
        return

    print(title)
    label_width = max(24, max(len(label) for label in features))
    total_value = sum(value for value, _ in features.values())
    for label, (value, holdings_list) in sorted(features.items(), key=lambda x: x[1], reverse=1):
        frac = value / total_value

        if not relative:
            print('  {:{width}}  {:>16.2f} {} ( {:>6.1%} )'.format(
                label, value, currency, frac,
                width=label_width))
        else:
            print('  {:{width}}  {:>6.1%}'.format(
                label, frac,
                width=label_width))
        if print_holdings:
            for holding in holdings_list:
                print('      {:60} {:12} {:>16.2f} {:12}'.format(holding.account, holding.currency, holding.market_value, holding.cost_currency))

    print()


def load_csv_and_prices(holdings_filename, prices_filename, currency):
    """Load the holdings and prices from filenames and convert to a common currency.

    Args:
      holdings_filename: A string, the name of a CSV file containing the list of Holdings.
      prices_filename: A string, the name of a Beancount file containing price directives.
      currency: A string, the target currency to convert all the holdings to.
    Returns:
      Two lists of holdings: a list in the original currencies, and a list all
      converted to the target currency.
    """
    # Load the price database.
    # Generate with "bean-query LEDGER holdings"
    price_entries, errors, options_map = loader.load(prices_filename)
    price_map = prices.build_price_map(price_entries)

    # Load the holdings list.
    # Generate with "bean-query LEDGER print_prices"
    mixed_holdings_list = list(holdings_reports.load_from_csv(open(holdings_filename)))

    # Convert all the amounts to a common currency (otherwise summing market
    # values makes no sense).
    holdings_list = holdings.convert_to_currency(price_map, currency,
                                                 mixed_holdings_list)

    return mixed_holdings_list, holdings_list
