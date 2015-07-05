"""Builder for Bewancount grammar.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import copy
import os
import re
from os import path

from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount.core.amount import amount_div
from beancount.core import display_context
from beancount.core.position import Lot
from beancount.core.position import Position
from beancount.core.data import Transaction
from beancount.core.data import Balance
from beancount.core.data import Open
from beancount.core.data import Close
from beancount.core.data import Commodity
from beancount.core.data import Pad
from beancount.core.data import Event
from beancount.core.data import Price
from beancount.core.data import Note
from beancount.core.data import Document
from beancount.core.data import new_metadata
from beancount.core.data import Posting
from beancount.core.data import BOOKING_METHODS
from beancount.core.interpolate import balance_incomplete_postings
from beancount.core.interpolate import compute_residual
from beancount.core.interpolate import infer_tolerances

from beancount.parser import lexer
from beancount.parser import options
from beancount.core import account
from beancount.core import data


__sanity_checks__ = False

# FIXME: This environment variable enables temporary support for negative
# prices. If you've updated across 2015-01-10 and you're getting a lot of
# errors, you need to fix all the signs on your @@ total price values (they are
# to be positive only). Just set this environment variable to disable this
# change if you need to post-pone this.
__allow_negative_prices__ = os.environ.get('BEANCOUNT_ALLOW_NEGATIVE_PRICES', False)


ParserError = collections.namedtuple('ParserError', 'source message entry')
ParserSyntaxError = collections.namedtuple('ParserSyntaxError', 'source message entry')
DeprecatedError = collections.namedtuple('DeprecatedError', 'source message entry')



# Temporary holder for key-value pairs.
KeyValue = collections.namedtuple('KeyValue', 'key value')


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
    return re.compile('({})(:[A-Z][A-Za-z0-9\-]+)*$'.format('|'.join(names)))


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
                ParserError(meta, "Unbalanced tag: '{}'".format(tag), None))

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
        self.options['display_context'] = self.dcontext

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
                meta = new_metadata(filename, lineno)
                self.errors.append(
                    DeprecatedError(meta, option_descriptor.deprecated, None))

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
        self.dcupdate(number, currency)
        return Amount(number, currency)

    def lot_cost_date(self, cost, lot_date, istotal):
        """Process a lot_cost_date grammar rule.

        Args:
          cost: an instance of Amount.
          lot_date: either None or a datetime instance.
        Returns:
          A pair of the input. We do very little here.
        """
        assert isinstance(cost, Amount)
        return (cost, lot_date, istotal)

    def position(self, filename, lineno, amount, lot_cost_date):
        """Process a position grammar rule.

        Args:
          filename: the current filename.
          lineno: the current line number.
          amount: an instance of Amount for the position.
          lot_cost_date: a tuple of (cost, lot-date)
        Returns:
          A new instance of Position.
        """
        cost, lot_date, istotal = lot_cost_date if lot_cost_date else (None, None, False)

        # We don't allow a cost nor a price of zero. (Conversion entries may use
        # a price of zero as the only special case, but never for costs.)
        if cost is not None:
            if amount.number == ZERO:
                meta = new_metadata(filename, lineno)
                self.errors.append(
                    ParserError(meta,
                                'Amount is zero: "{}"'.format(amount), None))

            if cost.number < ZERO:
                meta = new_metadata(filename, lineno)
                self.errors.append(
                    ParserError(meta, 'Cost is negative: "{}"'.format(cost), None))

        if istotal:
            cost = amount_div(cost, abs(amount.number))
        lot = Lot(amount.currency, cost, lot_date)

        return Position(lot, amount.number)

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

    def build_grammar_error(self, filename, lineno, message, exc_type=None):
        """Build a grammar error and appends it to the list of pending errors.

        Args:
          filename: The current filename
          lineno: The current line number
          message: The message of the error.
          exc_type: An exception type, if an exception occurred.
        """
        if not isinstance(message, str):
            message = str(message)
        if exc_type is not None:
            message = '{}: {}'.format(exc_type.__name__, message)
        meta = new_metadata(filename, lineno)
        self.errors.append(
            ParserSyntaxError(meta, message, None))

    def open(self, filename, lineno, date, account, currencies, booking, kvlist):
        """Process an open directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the name of the account.
          currencies: A list of constraint currencies.
          booking: A string, the booking method, or None if none was specified.
          kvlist: a list of KeyValue instances.
        Returns:
          A new Open object.
        """
        meta = new_metadata(filename, lineno, kvlist)
        entry = Open(meta, date, account, currencies, booking)
        if booking and booking not in BOOKING_METHODS:
            self.errors.append(
                ParserError(meta, "Invalid booking method: {}".format(booking), entry))
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

    def key_value(self, key, value):
        """Process a document directive.

        Args:
          filename: The current filename.
          lineno: The current line number.
          date: A datetime object.
          account: A string, the account the document relates to.
          document_filename: A str, the name of the document file.
        Returns:
          A new Document object.
        """
        return KeyValue(key, value)

    def posting(self, filename, lineno, account, position, price, istotal, flag):
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
            if price and price.number < ZERO:
                meta = new_metadata(filename, lineno)
                self.errors.append(
                    ParserError(meta, (
                        "Negative prices are not allowed: {} "
                        "(see http://furius.ca/beancount/doc/bug-negative-prices "
                        "for workaround)"
                    ).format(price), None))
                # Fix it and continue.
                price.number = abs(price.number)

        # If the price is specified for the entire amount, compute the effective
        # price here and forget about that detail of the input syntax.
        if istotal:
            if position.number == ZERO:
                number = ZERO
            else:
                if __allow_negative_prices__:
                    number = price.number/position.number
                else:
                    number = price.number/abs(position.number)
            price = Amount(number, price.currency)

        # Note: Allow zero prices because we need them for round-trips for
        # conversion entries.
        #
        # if price is not None and price.number == ZERO:
        #     self.errors.append(
        #         ParserError(meta, "Price is zero: {}".format(price), None))

        meta = new_metadata(filename, lineno)
        return Posting(None, account, position, price, chr(flag) if flag else None, meta)


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
          meta: An AttrDict metadata for errors generated in this routine.
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

        # Separate postings and key-valus.
        postings = []
        if posting_or_kv_list:
            last_posting = None
            for posting_or_kv in posting_or_kv_list:
                if isinstance(posting_or_kv, Posting):
                    postings.append(posting_or_kv)
                    last_posting = posting_or_kv
                else:
                    if last_posting is None:
                        value = meta.setdefault(posting_or_kv.key,
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

        # Unpack the transaction fields.
        payee_narration = self.unpack_txn_strings(txn_fields, meta)
        if payee_narration is None:
            return None
        payee, narration = payee_narration

        # We now allow a single posting when its balance is zero, so we
        # commented out the check below. If a transaction has a single posting
        # with a non-zero balance, it'll get caught below int he
        # balance_incomplete_postings code.
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
        assert isinstance(tags, (set, frozenset))
        if self.tags:
            tags.update(self.tags)
        tags = frozenset(tags) if tags else None

        # Make links to None if empty.
        links = txn_fields.links
        links = frozenset(links) if links else None

        # Create the transaction. Note: we need to parent the postings.
        entry = Transaction(meta, date, chr(flag),
                            payee, narration, tags, links, postings)

        # Balance incomplete auto-postings and set the parent link to this entry as well.
        balance_errors = balance_incomplete_postings(entry, self.options)

        if balance_errors:
            self.errors.extend(balance_errors)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = compute_residual(entry.postings)
            tolerances = infer_tolerances(entry.postings, self.options)
            assert residual.is_small(tolerances, self.options['default_tolerance']), (
                "Invalid residual {}".format(residual))

        return entry
