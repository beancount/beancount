"""A plugin that verifies that all seen commodities have a Commodity directive.

This is useful if you're a bit pedantic and like to make sure that you're
declared attributes for each of the commodities you use. It's useful if you use
the portfolio export, for example.

You can provide a mapping of (account-regexp, currency-regexp) as options, to
specify which commodities to ignore from this plugin selectively. Use this
sparingly, as it is an out from the checks that this plugin provides. However,
in an active options trading account, a ton of products get inserted and the
number of commodity directives can be overwhelming and it's not productive to
declare each of the options contracts - names with an embedded strike and
expiration date, such as 'SPX_121622P3300' - individually.

Note that if a symbol has been ignored in at least one account, it will
therefore be further in all Price directives and Metadata values.
"""

__copyright__ = "Copyright (C) 2015-2017, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re

from beancount.core import data
from beancount.core.amount import CURRENCY_RE

__plugins__ = ("validate_commodity_directives",)


ConfigError = collections.namedtuple("ConfigError", "source message entry")
CheckCommodityError = collections.namedtuple("CheckCommodityError", "source message entry")


# Account placeholder for commodities appearing in Price directives.
PRICE_CONTEXT = "Price Directive Context"
METADATA_CONTEXT = "Metadata Value Context"
ANONYMOUS = {PRICE_CONTEXT, METADATA_CONTEXT}


def get_commodity_map_ex(entries, metadata=False):
    """Find and extract commodities in the stream of directives."""

    # Find commodity names in metadata.
    #
    # TODO(dnicolodi) Unfortunately detecting commodities in metadata
    # values may result in false positives: common used string are
    # matched by the regular expression. Revisit this when commodities
    # will be represented with their own type.
    ignore = set(["filename", "lineno", "__automatic__"])
    regexp = re.compile(CURRENCY_RE)

    def currencies_in_meta(entry):
        if entry.meta is not None:
            for key, value in entry.meta.items():
                if isinstance(value, str) and key not in ignore:
                    if regexp.fullmatch(value):
                        yield value

    commodities_map = {}
    occurrences = set()
    for entry in entries:
        if isinstance(entry, data.Commodity):
            commodities_map[entry.currency] = entry

        elif isinstance(entry, data.Open):
            if entry.currencies:
                for currency in entry.currencies:
                    occurrences.add((entry.account, currency))

        elif isinstance(entry, data.Transaction):
            for posting in entry.postings:
                # Main currency.
                units = posting.units
                occurrences.add((posting.account, units.currency))

                # Currency in cost.
                cost = posting.cost
                if cost:
                    occurrences.add((posting.account, cost.currency))

                # Currency in price.
                price = posting.price
                if price:
                    occurrences.add((posting.account, price.currency))

                # Currency in posting metadata.
                if metadata:
                    for currency in currencies_in_meta(posting):
                        occurrences.add((posting.account, currency))

        elif isinstance(entry, data.Balance):
            occurrences.add((entry.account, entry.amount.currency))

        elif isinstance(entry, data.Price):
            occurrences.add((PRICE_CONTEXT, entry.currency))
            occurrences.add((PRICE_CONTEXT, entry.amount.currency))

        # Entry metadata.
        if metadata:
            for currency in currencies_in_meta(entry):
                occurrences.add((METADATA_CONTEXT, currency))

    return occurrences, commodities_map


def validate_commodity_directives(entries, options_map, config_str=None):
    """Find all commodities used and ensure they have a corresponding Commodity directive.

    Args:
      entries: A list of directives.
      options_map: An options map.
      config_str: The configuration as a string version of a float.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []

    if config_str:
        config_obj = eval(config_str, {}, {})
        if not isinstance(config_obj, dict):
            errors.append(
                ConfigError(
                    data.new_metadata("<commodity_attr>", 0),
                    "Invalid configuration for check_commodity plugin; skipping.",
                    None,
                )
            )
            return entries, errors
    else:
        config_obj = {}

    # Compile the regular expressions, producing an error if invalid.
    ignore_map = {}
    for key, value in config_obj.items():
        kv = []
        for pattern in key, value:
            try:
                kv.append(re.compile(pattern).match)
            except re.error:
                meta = data.new_metadata("<check_commodity>", 0)
                errors.append(
                    CheckCommodityError(
                        meta, "Invalid regexp: '{}' for {}".format(value, key), None
                    )
                )
        if len(kv) == 2:
            ignore_map[kv[0]] = kv[1]

    # Get all the occurrences of commodities and a mapping of the directives.
    #
    # TODO(blais): Establish a distinction at the parser level for commodities
    # and strings, so that we can turn detection of them in metadata.
    occurrences, commodity_map = get_commodity_map_ex(entries, metadata=False)

    # Process all currencies with context.
    issued = set()
    ignored = set()
    anonymous = set()
    for context, currency in sorted(occurrences):
        if context in ANONYMOUS:
            anonymous.add((context, currency))
            continue
        commodity_entry = commodity_map.get(currency, None)

        # Skip if the commodity was declared, or if an error for that commodity
        # has already been issued.
        if commodity_entry is not None or currency in issued:
            continue

        # If any of the ignore patterns matches, ignore and record ignored.
        if any(
            (context_re(context) and currency_re(currency))
            for context_re, currency_re in ignore_map.items()
        ):
            ignored.add(currency)
            continue

        # Issue error.
        meta = data.new_metadata("<check_commodity>", 0)
        errors.append(
            CheckCommodityError(
                meta,
                "Missing Commodity directive for '{}' in '{}'".format(currency, context),
                None,
            )
        )

        # Process it only once.
        issued.add(currency)

    # Process all currencies out of context, automatically ignoring those which
    # have already been issued with account context..
    for context, currency in sorted(anonymous):
        commodity_entry = commodity_map.get(currency, None)

        # Skip if (a) the commodity was declared, any of the ignore patterns
        # matches, or an error for that commodity has already been issued.
        if commodity_entry is not None or currency in issued or currency in ignored:
            continue

        # Issue error.
        meta = data.new_metadata("<check_commodity>", 0)
        errors.append(
            CheckCommodityError(
                meta,
                "Missing Commodity directive for '{}' in '{}'".format(currency, context),
                None,
            )
        )

    return entries, errors
