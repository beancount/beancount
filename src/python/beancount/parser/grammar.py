"""Builder for Beancount grammar.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import copy
import os
import re
import traceback
from os import path
from datetime import date

from beancount.core.number import ZERO
from beancount.core.number import MISSING
from beancount.core.number import Decimal
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

from beancount.parser import lexer
from beancount.parser import options
from beancount.core import account
from beancount.core import data


# FIXME: This environment variable enables temporary support for negative
# prices. If you've updated across 2015-01-10 and you're getting a lot of
# errors, you need to fix all the signs on your @@ total price values (they are
# to be positive only). Just set this environment variable to disable this
# change if you need to post-pone this.
__allow_negative_prices__ = os.environ.get('BEANCOUNT_ALLOW_NEGATIVE_PRICES', False)


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
    return re.compile('({})(:[A-Z][A-Za-z0-9\-]*)*$'.format('|'.join(names)))


# A temporary data structure used during parsing to hold and accumulate the
# fields being parsed on a transaction line. Because we want to be able to parse
# these in arbitrary order, we have to accumulate the fields and then unpack
# them intelligently in the transaction callback.
#
# Attributes:
#  strings: a list of strings, for the payee and the narration.
#  tags: a set object  of the tags to be applied to this transaction.
#  links: a set of link strings to be applied to this transaction.
#  has_pipe: True if a pipe has been seen somewhere in the list. This is used
#    to issue an error if only a single string is present, because the PIPE
#    character does not carry any special meaning anymore.
TxnFields = collections.namedtuple('TxnFields', 'strings tags links has_pipe')


class Builder(lexer.LexBuilder):
    """A builder used by the lexer and grammar parser as callbacks to create
    the data objects corresponding to rules parsed from the input file."""

    def __init__(self, filename):
        lexer.LexBuilder.__init__(self)

        # A stack of the current active tags.
        self.tags = []

        # A dict of the current active metadata fields (not a stack).
        self.meta = collections.defaultdict(list)

        # The result from running the parser, a list of entries.
        self.entries = []

        # Accumulated and unprocessed options.
        self.options = copy.deepcopy(options.OPTIONS_DEFAULTS)

        # Set the filename we're processing.
        self.options['filename'] = filename

        # Make the account regexp more restrictive than the default: check
        # types.
        self.account_regexp = valid_account_regexp(self.options)

        # A display context builder.
        self.dcontext = display_context.DisplayContext()
        self.dcupdate = self.dcontext.update

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
            meta = new_metadata(self.options['filename'], 0)
            self.errors.append(
                ParserError(meta, "Unbalanced pushed tag: '{}'".format(tag), None))

        # If the user left some metadata unpopped, issue an error.
        for key, value_list in self.meta.items():
            meta = new_metadata(self.options['filename'], 0)
            self.errors.append(
                ParserError(meta, (
                    "Unbalanced metadata key '{}'; leftover metadata '{}'").format(
                        key, ', '.join(value_list)), None))

        # Weave the commas option in the DisplayContext itself, so it propagages
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

        # Add the full list of seen commodities.
        #
        # IMPORTANT: This is currently where the list of all commodities seen
        # from the parser lives. The
        # beancount.core.getters.get_commodities_map() routine uses this to
        # automatically generate a full list of directives. An alternative would
        # be to implement a plugin that enforces the generate of these
        # post-parsing so that they are always guaranteed to live within the
        # flow of entries. This would allow us to keep all the data in that list
        # of entries and to avoid depending on the options to store that output.
        self.options['commodities'] = self.commodities

        return self.options

    def get_invalid_account(self):
        """See base class."""
        return account.join(self.options['name_equity'], 'InvalidAccountName')

    def get_long_string_maxlines(self):
        """See base class."""
        return self.options['long_string_maxlines']

    def store_result(self, entries):
        """Start rule stores the final result here.

        Args:
          entries: A list of entries to store.
        """
        if entries:
            self.entries = entries

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

    def pushtag(self, tag):
        """Push a tag on the current set of tags.

        Note that this does not need to be stack ordered.

        Args:
          tag: A string, a tag to be added.
        """
        self.tags.append(tag)

    def poptag(self, tag):
        """Pop a tag off the current set of stacks.

        Args:
          tag: A string, a tag to be removed from the current set of tags.
        """
        try:
            self.tags.remove(tag)
        except ValueError:
            meta = new_metadata(self.options['filename'], 0)
            self.errors.append(
                ParserError(meta, "Attempting to pop absent tag: '{}'".format(tag), None))

    def pushmeta(self, key, value):
        """Set a metadata field on the current key-value pairs to be added to transactions.

        Args:
          key_value: A KeyValue instance, to be added to the dict of metadata.
        """
        self.meta[key].append(value)

    def popmeta(self, key):
        """Removed a key off the current set of stacks.

        Args:
          key: A string, a key to be removed from the meta dict.
        """
        try:
            if key not in self.meta:
                raise IndexError
            value_list = self.meta[key]
            value_list.pop(-1)
            if not value_list:
                self.meta.pop(key)
        except IndexError:
            meta = new_metadata(self.options['filename'], 0)
            self.errors.append(
                ParserError(meta,
                            "Attempting to pop absent metadata key: '{}'".format(key),
                            None))

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
                # Fix up account_rounding to be a subaccount if the user specified a
                # full account name. This is intended to ease transition in the
                # change of semantics that occurred on 2015-09-05, whereby the value
                # of this option became defined as a subaccount of Equity instead of
                # a full account name. See Issue #67.
                # This should eventually be deprecated, say, in a year (after Sep 2016).
                if key == 'account_rounding':
                    root = account.root(1, value)
                    if root in (self.options['name_{}'.format(name)]
                                for name in ['assets', 'liabilities', 'equity',
                                             'income', 'expenses']):
                        self.errors.append(
                            ParserError(self.get_lexer_location(),
                                        "'account_rounding' option should now refer to "
                                        "a subaccount.", None))
                        value = account.sans_root(value)

                # Set the value.
                self.options[key] = value

            # Refresh the list of valid account regexps as we go along.
            if key.startswith('name_'):
                # Update the set of valid account types.
                self.account_regexp = valid_account_regexp(self.options)


    def include(self, filename, lineno, include_filename):
        """Process an include directive.

        Args:
          filename: current filename.
          lineno: current line number.
          include_name: A string, the name of the file to include.
        """
        self.options['include'].append(include_filename)

    def plugin(self, filename, lineno, plugin_name, plugin_config):
        """Process a plugin directive.

        Args:
          filename: current filename.
          lineno: current line number.
          plugin_name: A string, the name of the plugin module to import.
          plugin_config: A string or None, an optional configuration string to
            pass in to the plugin module.
        """
        self.options['plugin'].append((plugin_name, plugin_config))

    def amount(self, number, currency):
        """Process an amount grammar rule.

        Args:
          number: a Decimal instance, the number of the amount.
          currency: a currency object (a str, really, see CURRENCY above)
        Returns:
          An instance of Amount.
        """
        # Update the mapping that stores the parsed precisions.
        # Note: This is relatively slow, adds about 70ms because of number.as_tuple().
        if isinstance(number, Decimal) and currency:
            self.dcupdate(number, currency)
        return Amount(number, currency)

    def compound_amount(self, number_per, number_total, currency):
        """Process an amount grammar rule.

        Args:
          number_per: a Decimal instance, the number of the cost per share.
          number_total: a Decimal instance, the number of the cost over all shares.
          currency: a currency object (a str, really, see CURRENCY above)
        Returns:
          A triple of (Decimal, Decimal, currency string) to be processed further when
          creating the final per-unit cost number.
        """
        # Update the mapping that stores the parsed precisions.
        # Note: This is relatively slow, adds about 70ms because of number.as_tuple().
        if isinstance(number_per, Decimal):
            self.dcupdate(number_per, currency)
        if isinstance(number_total, Decimal):
            self.dcupdate(number_total, currency)

        # Note that we are not able to reduce the value to a number per-share
        # here because we only get the number of units in the full lot spec.
        return CompoundAmount(number_per, number_total, currency)

    def cost_merge(self, _):
        """Create a 'merge cost' token."""
        return MERGE_COST

    def cost_spec(self, cost_comp_list):
        """Process a cost_spec grammar rule.

        Args:
          cost_comp_list: A list of CompoundAmount, a datetime.date, or
            label ID strings.
        Returns:
          A cost-info tuple of CompoundAmount, lot date and label string. Any of these
          may be set to a sentinel indicating "unset".
        """
        if not cost_comp_list:
            return CostSpec(MISSING, None, MISSING, None, None, False)
        assert isinstance(cost_comp_list, list), (
            "Internal error in parser: {}".format(cost_comp_list))

        compound_cost = None
        date_ = None
        label = None
        merge = None
        for comp in cost_comp_list:
            if isinstance(comp, CompoundAmount):
                if compound_cost is None:
                    compound_cost = comp
                else:
                    self.errors.append(
                        ParserError(self.get_lexer_location(),
                                    "Duplicate cost: '{}'.".format(comp), None))

            elif isinstance(comp, date):
                if date_ is None:
                    date_ = comp
                else:
                    self.errors.append(
                        ParserError(self.get_lexer_location(),
                                    "Duplicate date: '{}'.".format(comp), None))

            elif comp is MERGE_COST:
                if merge is None:
                    merge = True
                else:
                    self.errors.append(
                        ParserError(self.get_lexer_location(),
                                    "Duplicate merge-cost spec", None))

            else:
                assert isinstance(comp, str), (
                    "Currency component is not string: '{}'".format(comp))
                if label is None:
                    label = comp
                else:
                    self.errors.append(
                        ParserError(self.get_lexer_location(),
                                    "Duplicate label: '{}'.".format(comp), None))

        # If there was a cost_comp_list, thus a "{...}" cost basis spec, you must
        # indicate that by creating a CompoundAmount(), always.

        if compound_cost is None:
            number_per, number_total, currency = MISSING, None, MISSING
        else:
            number_per, number_total, currency = compound_cost

        if merge is None:
            merge = False

        return CostSpec(number_per, number_total, currency, date_, label, merge)

    def cost_spec_total_legacy(self, cost, date):
        """Process a deprecated legacy 'total cost' specification.

        Args:
          cost: An instance of Amount, the total cost.
          date: A datetime.date instance, the lot date for the lot.
        Returns:
          Same as cost_spec().
        """
        return CostSpec(ZERO, cost.number, cost.currency, date, None, False)

    def handle_list(self, object_list, new_object):
        """Handle a recursive list grammar rule, generically.

        Args:
          object_list: the current list of objects.
          new_object: the new object to be added.
        Returns:
          The new, updated list of objects.
        """
        if object_list is None:
            object_list = []
        if new_object is not None:
            object_list.append(new_object)
        return object_list

    def open(self, filename, lineno, date, account, currencies, booking_str, kvlist):
        """Process an open directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the name of the account.
          currencies: A list of constraint currencies.
          booking_str: A string, the booking method, or None if none was specified.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Open object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        error = False
        if booking_str:
            try:
                # Note: Somehow the 'in' membership operator is not defined on Enum.
                booking = Booking[booking_str]
            except KeyError:
                # If the per-account method is invalid, set it to the global
                # default method and continue.
                booking = self.options['booking_method']
                error = True
        else:
            booking = None

        entry = Open(meta, date, account, currencies, booking)
        if error:
            self.errors.append(ParserError(meta,
                                           "Invalid booking method: {}".format(booking_str),
                                           entry))
        return entry

    def close(self, filename, lineno, date, account, kvlist):
        """Process a close directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the name of the account.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Close object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Close(meta, date, account)

    def commodity(self, filename, lineno, date, currency, kvlist):
        """Process a close directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          currency: A string, the commodity being declared.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Close object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Commodity(meta, date, currency)

    def pad(self, filename, lineno, date, account, source_account, kvlist):
        """Process a pad directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the account to be padded.
          source_account: A string, the account to pad from.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Pad object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Pad(meta, date, account, source_account)

    def balance(self, filename, lineno, date, account, amount, tolerance, kvlist):
        """Process an assertion directive.

        We produce no errors here by default. We replace the failing ones in the
        routine that does the verification later one, that these have succeeded
        or failed.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the account to balance.
          amount: The expected amount, to be checked.
          tolerance: The tolerance number.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Balance object.
        """
        diff_amount = None
        meta = new_metadata(filename, lineno, kvlist)

        # Only support explicit tolerance syntax if the experiment is enabled.
        if (tolerance is not None and
            not self.options["experiment_explicit_tolerances"]):
            self.errors.append(
                ParserError(meta, "Tolerance syntax is not supported", None))
            tolerance = '__tolerance_syntax_not_supported__'

        return Balance(meta, date, account, amount, tolerance, diff_amount)

    def event(self, filename, lineno, date, event_type, description, kvlist):
        """Process an event directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          event_type: a str, the name of the event type.
          description: a str, the event value, the contents.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Event object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Event(meta, date, event_type, description)

    def query(self, filename, lineno, date, query_name, query_string, kvlist):
        """Process a document directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          query_name: a str, the name of the query.
          query_string: a str, the SQL query itself.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Query object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Query(meta, date, query_name, query_string)

    def price(self, filename, lineno, date, currency, amount, kvlist):
        """Process a price directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          currency: the currency to be priced.
          amount: an instance of Amount, that is the price of the currency.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Price object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Price(meta, date, currency, amount)

    def note(self, filename, lineno, date, account, comment, kvlist):
        """Process a note directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the account to attach the note to.
          comment: A str, the note's comments contents.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Note object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Note(meta, date, account, comment)

    def document(self, filename, lineno, date, account, document_filename, kvlist):
        """Process a document directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance.
          document_filename: a str, the name of the document file.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Document object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        if not path.isabs(document_filename):
            document_filename = path.abspath(path.join(path.dirname(filename),
                                                       document_filename))
        return Document(meta, date, account, document_filename)

    def custom(self, filename, lineno, date, dir_type, custom_values, kvlist):
        """Process a custom directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          dir_type: A string, a type for the custom directive being parsed.
          custom_values: A list of the various tokens seen on the same line.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Custom object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        return Custom(meta, date, dir_type, custom_values)

    def custom_value(self, value, dtype=None):
        """Create a custom value object, along with its type.

        Args:
          value: One of the accepted custom values.
        Returns:
          A pair of (value, dtype) where 'dtype' is the datatype is that of the
          value.
        """
        if dtype is None:
            dtype = type(value)
        return ValueType(value, dtype)

    def key_value(self, key, value):
        """Process a document directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the account the document relates to.
          document_filename: A str, the name of the document file.
        Returns:
          A new KeyValue object.
        """
        return KeyValue(key, value)

    def posting(self, filename, lineno, account, units, cost, price, istotal, flag):
        """Process a posting grammar rule.

        Args:
          filename: the current filename.
          lineno: the current line number.
          account: A string, the account of the posting.
          position: An instance of Position from the grammar rule.
          price: Either None, or an instance of Amount that is the cost of the position.
          istotal: A bool, True if the price is for the total amount being parsed, or
                   False if the price is for each lot of the position.
          flag: A string, one-character, the flag associated with this posting.
        Returns:
          A new Posting object, with no parent entry.
        """
        # Prices may not be negative.
        if not __allow_negative_prices__:
            if price and isinstance(price.number, Decimal) and price.number < ZERO:
                meta = new_metadata(filename, lineno)
                self.errors.append(
                    ParserError(meta, (
                        "Negative prices are not allowed: {} "
                        "(see http://furius.ca/beancount/doc/bug-negative-prices "
                        "for workaround)"
                    ).format(price), None))
                # Fix it and continue.
                price = Amount(abs(price.number), price.currency)

        # If the price is specified for the entire amount, compute the effective
        # price here and forget about that detail of the input syntax.
        if istotal:
            if units.number == ZERO:
                number = ZERO
            else:
                if __allow_negative_prices__:
                    number = price.number/units.number
                else:
                    number = price.number/abs(units.number)
            price = Amount(number, price.currency)

        # Note: Allow zero prices because we need them for round-trips for
        # conversion entries.
        #
        # if price is not None and price.number == ZERO:
        #     self.errors.append(
        #         ParserError(meta, "Price is zero: {}".format(price), None))

        meta = new_metadata(filename, lineno)
        return Posting(account, units, cost, price, chr(flag) if flag else None, meta)

    def txn_field_new(self, _):
        """Create a new TxnFields instance.

        Returns:
          An instance of TxnFields, initialized with expected attributes.
        """
        return TxnFields([], set(), set(), [])

    def txn_field_TAG(self, txn_fields, tag):
        """Add a tag to the TxnFields accumulator.

        Args:
          txn_fields: The current TxnFields accumulator.
          tag: A string, the new tag to insert.
        Returns:
          An updated TxnFields instance.
        """
        txn_fields.tags.add(tag)
        return txn_fields

    def txn_field_LINK(self, txn_fields, link):
        """Add a link to the TxnFields accumulator.

        Args:
          txn_fields: The current TxnFields accumulator.
          link: A string, the new link to insert.
        Returns:
          An updated TxnFields instance.
        """
        txn_fields.links.add(link)
        return txn_fields

    def txn_field_STRING(self, txn_fields, string):
        """Add a tag to the TxnFields accumulator.

        Args:
          txn_fields: The current TxnFields accumulator.
          string: A string, the new string to insert in the list.
        Returns:
          An updated TxnFields instance.
        """
        txn_fields.strings.append(string)
        return txn_fields

    def txn_field_PIPE(self, txn_fields, _):
        """Mark the PIPE as present, in order to raise a backwards compatibility error.

        Args:
          txn_fields: The current TxnFields accumulator.
          _: This second argument is only there to prevent the caller method to
             unbundle the arguments; if you call with only a tuple, it gets applied.
             (I think this may be a bug in the Python C-API. When you upgrade to
             Python 3.4, check if this is still the case.)
        Returns:
          An updated TxnFields instance.
        """
        # Note: we're using a list because it runs faster than creating a new
        # tuple and there are possibly many of these.
        txn_fields.has_pipe.append(1)
        return txn_fields

    def unpack_txn_strings(self, txn_fields, meta):
        """Unpack a txn_fields accumulator to its payee and narration fields.

        Args:
          txn_fields: The current TxnFields accumulator.
          meta: A metadata dict for errors generated in this routine.
        Returns:
          A pair of (payee, narration) strings or None objects, or None, if
          there was an error.
        """
        num_strings = len(txn_fields.strings)
        if num_strings == 1:
            payee, narration = None, txn_fields.strings[0]
            if txn_fields.has_pipe:
                self.errors.append(
                    ParserError(meta,
                                "One string with a | symbol yields only a narration: "
                                "{}".format(txn_fields.strings), None))
        elif num_strings == 2:
            payee, narration = txn_fields.strings
        elif num_strings == 0:
            payee, narration = None, ""
        else:
            self.errors.append(
                ParserError(meta,
                            "Too many strings on transaction description: {}".format(
                                txn_fields.strings), None))
            return None
        return payee, narration

    def transaction(self, filename, lineno, date, flag, txn_fields, posting_or_kv_list):
        """Process a transaction directive.

        All the postings of the transaction are available at this point, and so the
        the transaction is balanced here, incomplete postings are completed with the
        appropriate position, and errors are being accumulated on the builder to be
        reported later on.

        This is the main routine that takes up most of the parsing time; be very
        careful with modifications here, they have an impact on performance.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          flag: a str, one-character, the flag associated with this transaction.
          txn_fields: A tuple of transaction fields, which includes descriptions
            (payee and narration), tags, and links.
          posting_or_kv_list: a list of Posting or KeyValue instances, to be inserted in
            this transaction, or None, if no postings have been declared.
        Returns:
          A new Transaction object.
        """
        meta = new_metadata(filename, lineno)

        # Separate postings and key-values.
        explicit_meta = {}
        postings = []
        if posting_or_kv_list:
            last_posting = None
            for posting_or_kv in posting_or_kv_list:
                if isinstance(posting_or_kv, Posting):
                    postings.append(posting_or_kv)
                    last_posting = posting_or_kv
                else:
                    if last_posting is None:
                        value = explicit_meta.setdefault(posting_or_kv.key,
                                                         posting_or_kv.value)
                        if value is not posting_or_kv.value:
                            self.errors.append(ParserError(
                                meta, "Duplicate metadata field on entry: {}".format(
                                    posting_or_kv), None))
                    else:
                        if last_posting.meta is None:
                            last_posting = last_posting._replace(meta={})
                            postings.pop(-1)
                            postings.append(last_posting)

                        value = last_posting.meta.setdefault(posting_or_kv.key,
                                                             posting_or_kv.value)
                        if value is not posting_or_kv.value:
                            self.errors.append(ParserError(
                                meta, "Duplicate posting metadata field: {}".format(
                                    posting_or_kv), None))



        # Initialize the metadata fields from the set of active values.
        if self.meta:
            for key, value_list in self.meta.items():
                meta[key] = value_list[-1]

        # Add on explicitly defined values.
        if explicit_meta:
            meta.update(explicit_meta)

        # Unpack the transaction fields.
        payee_narration = self.unpack_txn_strings(txn_fields, meta)
        if payee_narration is None:
            return None
        payee, narration = payee_narration

        # We now allow a single posting when its balance is zero, so we
        # commented out the check below. If a transaction has a single posting
        # with a non-zero balance, it'll get caught below in the
        # balance_incomplete_postings() code.
        #
        # # Detect when a transaction does not have at least two legs.
        # if postings is None or len(postings) < 2:
        #     self.errors.append(
        #         ParserError(meta,
        #                     "Transaction with only one posting: {}".format(postings),
        #                     None))
        #     return None

        # If there are no postings, make sure we insert a list object.
        if postings is None:
            postings = []

        # Merge the tags from the stack with the explicit tags of this
        # transaction, or make None.
        tags = txn_fields.tags
        assert isinstance(tags, (set, frozenset)), "Tags is not a set: {}".format(tags)
        if self.tags:
            tags.update(self.tags)
        tags = frozenset(tags) if tags else None

        # Make links to None if empty.
        links = txn_fields.links
        links = frozenset(links) if links else None

        # Create the transaction.
        return Transaction(meta, date, chr(flag),
                           payee, narration, tags, links, postings)
