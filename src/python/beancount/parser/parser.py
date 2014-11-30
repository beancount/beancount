"""Beancount syntax parser.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import functools
import inspect
import textwrap
import copy
import re
from os import path

from beancount.core.amount import ZERO
from beancount.core.amount import Amount
from beancount.core.amount import amount_div
from beancount.core import display_context
from beancount.core.position import Lot
from beancount.core.position import Position
from beancount.core.data import Transaction
from beancount.core.data import Balance
from beancount.core.data import Open
from beancount.core.data import Close
from beancount.core.data import Pad
from beancount.core.data import Event
from beancount.core.data import Price
from beancount.core.data import Note
from beancount.core.data import Document
from beancount.core.data import Source
from beancount.core.data import Posting
from beancount.core.data import BOOKING_METHODS
from beancount.core.interpolate import balance_incomplete_postings
from beancount.core.interpolate import compute_residual
from beancount.core.interpolate import SMALL_EPSILON

from beancount.parser import _parser
from beancount.parser import lexer
from beancount.parser import options
from beancount.core import account
from beancount.core import data


__sanity_checks__ = False


ParserError = collections.namedtuple('ParserError', 'source message entry')
ParserSyntaxError = collections.namedtuple('ParserSyntaxError', 'source message entry')


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

    def __init__(self):
        lexer.LexBuilder.__init__(self)

        # A stack of the current active tags.
        self.tags = []

        # The result from running the parser, a list of entries.
        self.entries = []

        # Accumulated and unprocessed options.
        self.options = copy.deepcopy(options.DEFAULT_OPTIONS)

        # Make the account regexp more restrictive than the default: check
        # types.
        self.account_regexp = valid_account_regexp(self.options)

        # A display context builder.
        self.dcbuilder = display_context.DisplayContextBuilder()
        self.dcupdate = self.dcbuilder.update

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
        # Normalize the option to a boolean object.
        self.options['render_commas'] = (
            self.options['render_commas'].lower() in ('1', 'true'))

        # Build and store the inferred DisplayContext instance.
        dcontext = self.dcbuilder.build()
        dcontext.set_commas(self.options['render_commas'])
        self.options['display_context'] = dcontext

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
        self.tags.remove(tag)

    def option(self, filename, lineno, key, value):
        """Process an option directive.

        Args:
          filename: current filename.
          lineno: current line number.
          key: option's key (str)
          value: option's value
        """
        if key not in self.options:
            source = Source(filename, lineno)
            self.errors.append(
                ParserError(source, "Invalid option: '{}'".format(key), None))
        elif key in options.READ_ONLY_OPTIONS:
            source = Source(filename, lineno)
            self.errors.append(
                ParserError(source, "Option '{}' may not be set".format(key), None))
        else:
            option = self.options[key]
            if isinstance(option, list):
                # Process the 'plugin' option specially: accept an optional
                # argument from it. NOTE: We will eventually phase this out and
                # replace it by a dedicated 'plugin' directive.
                if key == 'plugin':
                    match = re.match('(.*):(.*)', value)
                    if match:
                        plugin_name, plugin_config = match.groups()
                    else:
                        plugin_name, plugin_config = value, None
                    value = (plugin_name, plugin_config)

                # Append to a list of values.
                option.append(value)
            else:
                # Validate some option values.
                if key == 'plugin_processing_mode':
                    if value not in ('raw', 'default'):
                        source = Source(filename, lineno)
                        self.errors.append(
                            ParserError(source,
                                        ("Invalid value for '{}': '{}'").format(key, value),
                                        None))
                        return
                # Set the value.
                self.options[key] = value

            # Refresh the list of valid account regexps as we go.
            if key.startswith('name_'):
                # Update the set of valid account types.
                self.account_regexp = valid_account_regexp(self.options)

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
        if istotal:
            cost = amount_div(cost, amount.number)
        lot = Lot(amount.currency, cost, lot_date)

        # We don't allow a cost nor a price of zero. (Conversion entries may use
        # a price of zero as the only special case, but never for costs.)
        if cost is not None:
            if amount.number == ZERO:
                source = Source(filename, lineno)
                self.errors.append(
                    ParserError(source,
                                "Amount is zero or negative: {}".format(cost), None))

            if cost.number <= ZERO:
                source = Source(filename, lineno)
                self.errors.append(
                    ParserError(source, "Cost is zero or negative: {}".format(cost), None))

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

    def error(self, message, filename, lineno):
        """Process an error rule.

        Args:
          message: the message to be printed.
          filename: the current filename
          lineno: the current line number
        Returns:
        """
        source = Source(filename, lineno)
        self.errors.append(ParserSyntaxError(source, message, None))

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
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        entry = Open(source, date, account, currencies, booking, metadata)
        if booking and booking not in BOOKING_METHODS:
            self.errors.append(
                ParserError(source, "Invalid booking method: {}".format(booking), entry))
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
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        return Close(source, date, account, metadata)

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
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        return Pad(source, date, account, source_account, metadata)

    def balance(self, filename, lineno, date, account, amount, kvlist):
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
          kvlist: a list of KeyValue instances.
        Returns:
          A new Balance object.
        """
        diff_amount = None
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        return Balance(source, date, account, amount, diff_amount, metadata)

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
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        return Event(source, date, event_type, description, metadata)

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
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        return Price(source, date, currency, amount, metadata)

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
        source = Source(filename, lineno)
        metadata = None if kvlist is None else dict(kvlist)
        return Note(source, date, account, comment, metadata)

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
        source = Source(filename, lineno)
        if not path.isabs(document_filename):
            document_filename = path.abspath(path.join(path.dirname(filename),
                                                       document_filename))

        metadata = None if kvlist is None else dict(kvlist)
        return Document(source, date, account, document_filename, metadata)


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
        # If the price is specified for the entire amount, compute the effective
        # price here and forget about that detail of the input syntax.
        if istotal:
            price = Amount(ZERO
                           if position.number == ZERO
                           else price.number / position.number, price.currency)

        # Note: Allow zero prices becuase we need them for round-trips.
        # if price is not None and price.number == ZERO:
        #     source = Source(filename, lineno)
        #     self.errors.append(
        #         ParserError(source, "Price is zero: {}".format(price), None))

        return Posting(None, account, position, price, chr(flag) if flag else None, None)


    def txn_field_new(self):
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

    def unpack_txn_strings(self, txn_fields, source):
        """Unpack a txn_fields accumulator to its payee and narration fields.

        Args:
          txn_fields: The current TxnFields accumulator.
          stirng: A string, the new string to insert in the list.
        Returns:
          A pair of (payee, narration) strings or None objects, or None, if
          there was an error.
        """
        num_strings = len(txn_fields.strings)
        if num_strings == 1:
            payee, narration = None, txn_fields.strings[0]
            if txn_fields.has_pipe:
                self.errors.append(
                    ParserError(source,
                                "One string with a | symbol yields only a narration: "
                                "{}".format(txn_fields.strings), None))
        elif num_strings == 2:
            payee, narration = txn_fields.strings
        elif num_strings == 0:
            payee, narration = None, ""
        else:
            self.errors.append(
                ParserError(source,
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
        source = Source(filename, lineno)

        # Separate postings and key-valus.
        postings = []
        entry_metadata = {}
        if posting_or_kv_list:
            last_posting = None
            for posting_or_kv in posting_or_kv_list:
                if isinstance(posting_or_kv, Posting):
                    postings.append(posting_or_kv)
                    last_posting = posting_or_kv
                else:
                    if last_posting is None:
                        value = entry_metadata.setdefault(posting_or_kv.key,
                                                          posting_or_kv.value)
                        if value is not posting_or_kv.value:
                            self.errors.append(ParserError(
                                source, "Duplicate metadata field on entry: {}".format(
                                    posting_or_kv), None))
                    else:
                        if last_posting.metadata is None:
                            last_posting = last_posting._replace(metadata={})
                            postings.pop(-1)
                            postings.append(last_posting)

                        value = last_posting.metadata.setdefault(posting_or_kv.key,
                                                                 posting_or_kv.value)
                        if value is not posting_or_kv.value:
                            self.errors.append(ParserError(
                                source, "Duplicate posting metadata field: {}".format(
                                    posting_or_kv), None))

        # Unpack the transaction fields.
        payee_narration = self.unpack_txn_strings(txn_fields, source)
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
        #         ParserError(source,
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
        entry = Transaction(source, date, chr(flag),
                            payee, narration, tags, links, postings,
                            entry_metadata or None)

        # Balance incomplete auto-postings and set the parent link to this entry as well.
        balance_errors = balance_incomplete_postings(entry)

        if balance_errors:
            self.errors.extend(balance_errors)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = compute_residual(entry.postings)
            assert residual.is_small(SMALL_EPSILON), residual

        return entry


def parse_file(filename, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts.

    Args:
      filename: the name of the file to be parsed.
      kw: a dict of keywords to be applied to the C parser.
    Returns:
      A tuple of (
        list of entries parsed in the file,
        list of errors that were encountered during parsing, and
        a dict of the option values that were parsed from the file.)
    """
    builder = Builder()
    if filename:
        abs_filename = path.abspath(filename)
        builder.options["filename"] = abs_filename
    _parser.parse_file(filename, builder, **kw)
    return (builder.get_entries(), builder.errors, builder.get_options())

# Alias, for compatibility.
# pylint: disable=invalid-name
parse = parse_file


def parse_string(string, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts.

    Args:
      string: a str, the contents to be parsed instead of a file's.
    Return:
      Same as the output of parse_file().
    """
    builder = Builder()
    builder.options["filename"] = "<string>"
    _parser.parse_string(string, builder, **kw)
    return (builder.get_entries(), builder.errors, builder.get_options())


def parsedoc(fun):
    """Decorator that parses the function's docstring as an argument.

    Note that this only runs the parser on the tests, not the loader, so is no
    validation nor fixup applied to the list of entries.

    Args:
      fun: the function object to be decorated.
    Returns:
      The decorated function.

    """
    filename = inspect.getfile(fun)
    lines, lineno = inspect.getsourcelines(fun)

    # decorator line + function definition line (I realize this is largely
    # imperfect, but it's only for reporting in our tests) - empty first line
    # stripped away.
    lineno += 1

    @functools.wraps(fun)
    def newfun(self):
        entries, errors, options_map = parse_string(textwrap.dedent(fun.__doc__),
                                                    report_filename=filename,
                                                    report_firstline=lineno)
        return fun(self, entries, errors, options_map)
    newfun.__doc__ = None
    return newfun
