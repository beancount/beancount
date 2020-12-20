"""Builder for Beancount grammar.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import copy
import re
import sys
import traceback
from os import path
from datetime import date
from decimal import Decimal

from beancount.core.number import ZERO
from beancount.core.number import MISSING
from beancount.core.amount import Amount
from beancount.core import display_context
from beancount.core.position import CostSpec
from beancount.core.data import Transaction
from beancount.core.data import Balance
from beancount.core.data import Open
from beancount.core.data import Close
from beancount.core.data import Commodity
from beancount.core.data import Pad
from beancount.core.data import Event
from beancount.core.data import Query
from beancount.core.data import Price
from beancount.core.data import Note
from beancount.core.data import Document
from beancount.core.data import Custom
from beancount.core.data import new_metadata
from beancount.core.data import Posting
from beancount.core.data import Booking
from beancount.core.data import EMPTY_SET

from beancount.parser import lexer
from beancount.parser import options
from beancount.core import account
from beancount.core import data


ParserError = collections.namedtuple('ParserError', 'source message entry')
ParserSyntaxError = collections.namedtuple('ParserSyntaxError', 'source message entry')
DeprecatedError = collections.namedtuple('DeprecatedError', 'source message entry')



# Key-value pairs. This is used to hold meta-data attachments temporarily.
#
# Attributes:
#  key: A string, the name of the key.
#  value: Any object.
KeyValue = collections.namedtuple('KeyValue', 'key value')

# Value-type pairs. This is used to represent custom values where the concrete
# datatypes aren't matching those which are found in the parser.
#
# Attributes:
#  value: Any object.
#  dtype: The datatype of the object.
ValueType = collections.namedtuple('ValueType', 'value dtype')

# Convenience holding class for amounts with per-share and total value.
#
# Attributes:
#   number_per: A Decimal instance, the cost/price per unit.
#   number_total: A Decimal instance, the total cost/price.
#   currency: A string, the commodity of the amount.
CompoundAmount = collections.namedtuple('CompoundAmount',
                                        'number_per number_total currency')


# A unique token used to indicate a merge of the lots of an inventory.
MERGE_COST = '***'


def valid_account_regexp(options):
    """Build a regexp to validate account names from the options.

    Args:
      options: A dict of options, as per beancount.parser.options.
    Returns:
      A string, a regular expression that will match all account names.
    """
    names = map(options.__getitem__, ('name_assets',
                                      'name_liabilities',
                                      'name_equity',
                                      'name_income',
                                      'name_expenses'))

    # Replace the first term of the account regular expression with the specific
    # names allowed under the options configuration. This code is kept in sync
    # with {5672c7270e1e}.
    return re.compile("(?:{})(?:{}{})+".format('|'.join(names),
                                               account.sep,
                                               account.ACC_COMP_NAME_RE))


# A temporary data structure used during parsing to hold and accumulate the
# fields being parsed on a transaction line. Because we want to be able to parse
# these in arbitrary order, we have to accumulate the fields and then unpack
# them intelligently in the transaction callback.
#
# Attributes:
#  tags: a set object  of the tags to be applied to this transaction.
#  links: a set of link strings to be applied to this transaction.
TagsLinks = collections.namedtuple('TagsLinks', 'tags links')


class Builder(lexer.LexBuilder):
    """A builder used by the lexer and grammar parser as callbacks to create
    the data objects corresponding to rules parsed from the input file."""

    # pylint: disable=too-many-instance-attributes
    def __init__(self):
        lexer.LexBuilder.__init__(self)

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

    def finalize(self):
        """Finalize the parser, check for final errors and return the triple.

        Returns:
          A triple of
            entries: A list of parsed directives, which may need completion.
            errors: A list of errors, hopefully empty.
            options_map: A dict of options.
        """
        # Weave the commas option in the DisplayContext itself, so it propagates
        # everywhere it is used automatically.
        self.dcontext.set_commas(self.options['render_commas'])

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
        self.options['dcontext'] = self.dcontext

        return self.options

    def get_long_string_maxlines(self):
        """See base class."""
        return self.options['long_string_maxlines']

    def store_result(self, filename, lineno, entries):
        """Start rule stores the final result here.

        Args:
          entries: A list of entries to store.
        """
        if entries:
            self.entries = entries
        # Also record the name of the processed file.
        self.options['filename'] = filename

    def build_grammar_error(self, filename, lineno, exc_value,
                            exc_type=None, exc_traceback=None):
        """Build a grammar error and appends it to the list of pending errors.

        Args:
          filename: The current filename
          lineno: The current line number
          excvalue: The exception value, or a str, the message of the error.
          exc_type: An exception type, if an exception occurred.
          exc_traceback: A traceback object.
        """
        if exc_type is not None:
            assert not isinstance(exc_value, str)
            strings = traceback.format_exception_only(exc_type, exc_value)
            tblist = traceback.extract_tb(exc_traceback)
            filename, lineno, _, __ = tblist[0]
            message = '{} ({}:{})'.format(strings[0], filename, lineno)
        else:
            message = str(exc_value)
        meta = new_metadata(filename, lineno)
        self.errors.append(
            ParserSyntaxError(meta, message, None))

    def option(self, filename, lineno, key, value):
        """Process an option directive.

        Args:
          filename: current filename.
          lineno: current line number.
          key: option's key (str)
          value: option's value
        """
        if key not in self.options:
            meta = new_metadata(filename, lineno)
            self.errors.append(
                ParserError(meta, "Invalid option: '{}'".format(key), None))

        elif key in options.READ_ONLY_OPTIONS:
            meta = new_metadata(filename, lineno)
            self.errors.append(
                ParserError(meta, "Option '{}' may not be set".format(key), None))

        else:
            option_descriptor = options.OPTIONS[key]

            # Issue a warning if the option is deprecated.
            if option_descriptor.deprecated:
                assert isinstance(option_descriptor.deprecated, str), "Internal error."
                meta = new_metadata(filename, lineno)
                self.errors.append(
                    DeprecatedError(meta, option_descriptor.deprecated, None))

            # Rename the option if it has an alias.
            if option_descriptor.alias:
                key = option_descriptor.alias
                option_descriptor = options.OPTIONS[key]

            # Convert the value, if necessary.
            if option_descriptor.converter:
                try:
                    value = option_descriptor.converter(value)
                except ValueError as exc:
                    meta = new_metadata(filename, lineno)
                    self.errors.append(
                        ParserError(meta,
                                    "Error for option '{}': {}".format(key, exc),
                                    None))
                    return

            option = self.options[key]
            if isinstance(option, list):
                # Append to a list of values.
                option.append(value)

            elif isinstance(option, dict):
                # Set to a dict of values.
                if not (isinstance(value, tuple) and len(value) == 2):
                    self.errors.append(
                        ParserError(
                            meta, "Error for option '{}': {}".format(key, value), None))
                    return
                dict_key, dict_value = value
                option[dict_key] = dict_value

            elif isinstance(option, bool):
                # Convert to a boolean.
                if not isinstance(value, bool):
                    value = (value.lower() in {'true', 'on'}) or (value == '1')
                self.options[key] = value

            else:
                # Set the value.
                self.options[key] = value

            # Refresh the list of valid account regexps as we go along.
            if key.startswith('name_'):
                # Update the set of valid account types.
                self.account_regexp = valid_account_regexp(self.options)
            elif key == 'insert_pythonpath':
                # Insert the PYTHONPATH to this file when and only if you
                # encounter this option.
                sys.path.insert(0, path.dirname(filename))
