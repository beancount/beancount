"""Builder for Beancount grammar."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2021, 2023-2026  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import copy
import traceback
from datetime import date
from decimal import Decimal
from os import path
from typing import Any
from typing import NamedTuple

import regex

from beancount.core import account
from beancount.core import data
from beancount.core import display_context
from beancount.core.amount import Amount
from beancount.core.data import EMPTY_SET
from beancount.core.data import Balance
from beancount.core.data import Booking
from beancount.core.data import Close
from beancount.core.data import Commodity
from beancount.core.data import Custom
from beancount.core.data import Document
from beancount.core.data import Event
from beancount.core.data import Meta
from beancount.core.data import Note
from beancount.core.data import Open
from beancount.core.data import Pad
from beancount.core.data import Posting
from beancount.core.data import Price
from beancount.core.data import Query
from beancount.core.data import Transaction
from beancount.core.data import new_metadata
from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.position import CostSpec
from beancount.parser import _grammar
from beancount.parser import lexer
from beancount.parser import options


class ParserError(NamedTuple):
    source: Meta
    message: str
    entry: None = None


class ParserSyntaxError(NamedTuple):
    source: Meta
    message: str
    entry: None = None


class DeprecatedError(NamedTuple):
    source: Meta
    message: str
    entry: None = None


class KeyValue(NamedTuple):
    """Key-value pairs. This is used to hold meta-data attachments temporarily.

    Attributes:
      key: A string, the name of the key.
      value: Any object.
    """

    key: str
    value: Any


class ValueType(NamedTuple):
    """
    Value-type pairs. This is used to represent custom values where the concrete
    datatypes aren't matching those which are found in the parser.

    Attributes:
        value: Any object.
        dtype: The datatype of the object.
    """

    value: Any
    dtype: type


class CompoundAmount(NamedTuple):
    """Convenience holding class for amounts with per-share and total value.

    Attributes:
      number_per: A Decimal instance, the cost/price per unit.
      number_total: A Decimal instance, the total cost/price.
      currency: A string, the commodity of the amount.
    """

    number_per: Decimal
    number_total: Decimal
    currency: str


# A unique token used to indicate a merge of the lots of an inventory.
MERGE_COST = "***"


def valid_account_regexp(options):
    """Build a regexp to validate account names from the options.

    Args:
      options: A dict of options, as per beancount.parser.options.
    Returns:
      A string, a regular expression that will match all account names.
    """
    names = map(
        options.__getitem__,
        ("name_assets", "name_liabilities", "name_equity", "name_income", "name_expenses"),
    )

    # Replace the first term of the account regular expression with the specific
    # names allowed under the options configuration. This code is kept in sync
    # with {5672c7270e1e}.
    return regex.compile(
        "(?:{})(?:{}{})+".format("|".join(names), account.sep, account.ACC_COMP_NAME_RE)
    )


class TagsLinks(NamedTuple):
    """A temporary data structure used during parsing to hold and accumulate the
    fields being parsed on a transaction line. Because we want to be able to parse
    these in arbitrary order, we have to accumulate the fields and then unpack
    them intelligently in the transaction callback.

    Attributes:
      tags: A set object  of the tags to be applied to this transaction.
      links: A set of link strings to be applied to this transaction.
    """

    tags: set[str]
    links: set[str]


class Builder(lexer.LexBuilder):
    """A builder used by the lexer and grammar parser as callbacks to create
    the data objects corresponding to rules parsed from the input file."""

    def __init__(self):
        lexer.LexBuilder.__init__(self)

        # A stack of the current active tags.
        self.tags = []

        # A dict of the current active metadata fields (not a stack).
        self.meta = collections.defaultdict(list)

        # The result from running the parser, a list of entries.
        self.entries = []

        # Accumulated and unprocessed options.
        self.options = copy.deepcopy(options.OPTIONS_DEFAULTS)

        # A mapping of all the accounts created.
        self.accounts = {}

        # Make the account regexp more restrictive than the default: check
        # types. Warning: This overrides the value in the base class.
        self.account_regexp = valid_account_regexp(self.options)

        # A display context builder.
        self.dcontext = display_context.DisplayContext()
        self.display_context_update = self.dcontext.update

    def _dcupdate(self, number, currency):
        """Update the display context."""
        if isinstance(number, Decimal) and currency and currency is not MISSING:
            self.display_context_update(number, currency)

    def finalize(self):
        """Finalize the parser, check for final errors and return the triple.

        Returns:
          A triple of
            entries: A list of parsed directives, which may need completion.
            errors: A list of errors, hopefully empty.
            options_map: A dict of options.
        """
        # If the user left some tags unbalanced, issue an error.
        for tag in self.tags:
            meta = new_metadata(self.options["filename"], 0)
            self.errors.append(ParserError(meta, "Unbalanced pushed tag: '{}'".format(tag)))

        # If the user left some metadata unpopped, issue an error.
        for key, value_list in self.meta.items():
            meta = new_metadata(self.options["filename"], 0)
            self.errors.append(
                ParserError(
                    meta,
                    ("Unbalanced metadata key '{}'; leftover metadata '{}'").format(
                        key, ", ".join(value_list)
                    ),
                )
            )

        # Weave the commas option in the DisplayContext itself, so it propagates
        # everywhere it is used automatically.
        self.dcontext.set_commas(self.options["render_commas"])

        # Set the fixed precisions.
        for currency, example_number in sorted(self.options["display_precision"].items()):
            num_tuple = example_number.as_tuple()
            self.dcontext.set_fixed_precision(currency, -num_tuple.exponent)

        return (self.get_entries(), self.errors, self.get_options())

    def get_entries(self):
        """Return the accumulated entries.

        Returns:
          A list of sorted directives.
        """
        return sorted(self.entries, key=data.entry_sortkey)

    def get_options(self):
        """Return the final options map.

        Returns:
          A dict of option names to options.
        """
        # Build and store the inferred DisplayContext instance.
        self.options["dcontext"] = self.dcontext

        return self.options

    def get_long_string_maxlines(self):
        """See base class."""
        return self.options["long_string_maxlines"]

    def store_result(self, filename, lineno, entries):
        """Start rule stores the final result here.

        Args:
          filename: The name of the file being processed.
          lineno: The current line number.
          entries: A list of entries to store.
        """
        if entries:
            self.entries = entries
        # Also record the name of the processed file.
        self.options["filename"] = filename

    def build_grammar_error(
        self, filename, lineno, exc_value, exc_type=None, exc_traceback=None
    ):
        """Build a grammar error and appends it to the list of pending errors.

        Args:
          filename: The current filename
          lineno: The current line number
          exc_value: The exception value, or a str, the message of the error.
          exc_type: An exception type, if an exception occurred.
          exc_traceback: A traceback object.
        """
        if exc_type is not None:
            assert not isinstance(exc_value, str)
            strings = traceback.format_exception_only(exc_type, exc_value)
            tblist = traceback.extract_tb(exc_traceback)
            filename, lineno, _, __ = tblist[0]
            message = "{} ({}:{})".format(strings[0], filename, lineno)
        else:
            message = str(exc_value)
        meta = new_metadata(filename, lineno)
        self.errors.append(ParserSyntaxError(meta, message))

    def account(self, filename, lineno, account):
        return _grammar.account(self, filename, lineno, account)

    def pipe_deprecated_error(self, filename, lineno):
        return _grammar.pipe_deprecated_error(self, filename, lineno)

    def pushtag(self, filename, lineno, tag):
        return _grammar.pushtag(self, filename, lineno, tag)

    def poptag(self, filename, lineno, tag):
        return _grammar.poptag(self, filename, lineno, tag)

    def pushmeta(self, filename, lineno, key_value):
        return _grammar.pushmeta(self, filename, lineno, key_value)

    def popmeta(self, filename, lineno, key):
        return _grammar.popmeta(self, filename, lineno, key)

    def option(self, filename, lineno, key, value):
        return _grammar.option(self, filename, lineno, key, value)

    def include(self, filename, lineno, include_filename):
        return _grammar.include(self, filename, lineno, include_filename)

    def plugin(self, filename, lineno, plugin_name, plugin_config):
        return _grammar.plugin(self, filename, lineno, plugin_name, plugin_config)

    def amount(self, filename, lineno, number, currency):
        return _grammar.amount(self, filename, lineno, number, currency)

    def compound_amount(self, filename, lineno, number_per, number_total, currency):
        return _grammar.compound_amount(self, filename, lineno, number_per, number_total, currency)

    def cost_merge(self, filename, lineno, _):
        return _grammar.cost_merge(self, filename, lineno, _)

    def cost_spec(self, filename, lineno, cost_comp_list, is_total):
        return _grammar.cost_spec(self, filename, lineno, cost_comp_list, is_total)

    def handle_list(self, filename, lineno, object_list, new_object):
        return _grammar.handle_list(self, filename, lineno, object_list, new_object)

    def open(self, filename, lineno, date, account, currencies, booking_str, kvlist):
        return _grammar.open(self, filename, lineno, date, account, currencies, booking_str, kvlist)

    def close(self, filename, lineno, date, account, kvlist):
        return _grammar.close(self, filename, lineno, date, account, kvlist)

    def commodity(self, filename, lineno, date, currency, kvlist):
        return _grammar.commodity(self, filename, lineno, date, currency, kvlist)

    def pad(self, filename, lineno, date, account, source_account, kvlist):
        return _grammar.pad(self, filename, lineno, date, account, source_account, kvlist)

    def balance(self, filename, lineno, date, account, amount, tolerance, kvlist):
        return _grammar.balance(self, filename, lineno, date, account, amount, tolerance, kvlist)

    def event(self, filename, lineno, date, event_type, description, kvlist):
        return _grammar.event(self, filename, lineno, date, event_type, description, kvlist)

    def query(self, filename, lineno, date, query_name, query_string, kvlist):
        return _grammar.query(self, filename, lineno, date, query_name, query_string, kvlist)

    def price(self, filename, lineno, date, currency, amount, kvlist):
        return _grammar.price(self, filename, lineno, date, currency, amount, kvlist)

    def note(self, filename, lineno, date, account, comment, tags_links, kvlist):
        return _grammar.note(self, filename, lineno, date, account, comment, tags_links, kvlist)

    def document(
        self, filename, lineno, date, account, document_filename, tags_links, kvlist
    ):
        return _grammar.document(
            self, filename, lineno, date, account, document_filename, tags_links, kvlist
        )

    def custom(self, filename, lineno, date, dir_type, custom_values, kvlist):
        return _grammar.custom(self, filename, lineno, date, dir_type, custom_values, kvlist)

    def custom_value(self, filename, lineno, value, dtype=None):
        return _grammar.custom_value(self, filename, lineno, value, dtype)

    def key_value(self, filename, lineno, key, value):
        return _grammar.key_value(self, filename, lineno, key, value)

    def posting(self, filename, lineno, account, units, cost, price, istotal, flag):
        return _grammar.posting(self, filename, lineno, account, units, cost, price, istotal, flag)

    def tag_link_new(self, filename, lineno):
        return _grammar.tag_link_new(self, filename, lineno)

    def tag_link_TAG(self, filename, lineno, tags_links, tag):
        return _grammar.tag_link_TAG(self, filename, lineno, tags_links, tag)

    def tag_link_LINK(self, filename, lineno, tags_links, link):
        return _grammar.tag_link_LINK(self, filename, lineno, tags_links, link)

    def _unpack_txn_strings(self, txn_strings, meta):
        return _grammar._unpack_txn_strings(self, txn_strings, meta)

    def _finalize_tags_links(self, tags, links):
        return _grammar._finalize_tags_links(self, tags, links)

    def transaction(
        self, filename, lineno, date, flag, txn_strings, tags_links, posting_or_kv_list
    ):
        return _grammar.transaction(
            self, filename, lineno, date, flag, txn_strings, tags_links, posting_or_kv_list
        )
