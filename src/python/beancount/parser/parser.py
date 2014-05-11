"""Beancount syntax parser.
"""
import collections
import datetime
import functools
import inspect
import textwrap
import copy
import tempfile
import re
from collections import namedtuple
from os import path

from beancount.parser import _parser
from beancount.parser import options
from beancount.core.account_types import AccountTypes, account_name_type
from beancount.core import account
from beancount.core import account_types
from beancount.core import data
from beancount.core.amount import ZERO, Decimal, Amount, amount_div
from beancount.core.position import Lot, Position
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Event, Price, Note, Document, FileLocation, Posting
from beancount.core.balance import balance_incomplete_postings
from beancount.core.balance import compute_residual, SMALL_EPSILON


__sanity_checks__ = False


ParserError = collections.namedtuple('ParserError', 'fileloc message entry')
ParserSyntaxError = collections.namedtuple('ParserError', 'fileloc message entry')


def valid_account_regexp(options):
    """Build a regexp to validate account names from the options."""
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
TxnFields = collections.namedtuple('TxnFields', 'strings tags links')


class Builder(object):
    """A builder used by the lexer and grammer parser as callbacks to create
    the data objects corresponding to rules parsed from the input file."""

    def __init__(self):
        # A stack of the current active tags.
        self.tags = []

        # The result from running the parser, a list of entries.
        self.entries = []

        # A mapping of all the accounts created.
        self.accounts = {}

        # Errors that occurred during parsing.
        self.errors = []

        # Accumulated and unprocessed options.
        self.options = copy.deepcopy(options.DEFAULT_OPTIONS)
        self.account_regexp = valid_account_regexp(self.options)

        # Re-initialize the parsing options. FIXME: This is because of some
        # globals in beancount.core.account, we don't want to leak options
        # between invocations.
        account_types.update_valid_account_names()

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
            fileloc = FileLocation(filename, lineno)
            self.errors.append(
                ParserError(fileloc, "Invalid option: '{}'".format(key), None))
        else:
            option = self.options[key]
            if isinstance(option, list):
                option.append(value)
            else:
                self.options[key] = value

            # Refresh the list of valid account regexps as we go.
            if key.startswith('name_'):
                self.account_regexp = valid_account_regexp(self.options)

                # Update the globals that check whether this account is valid.
                # FIXME: This is a known globals kludge we know we have to remove,
                # but has ties in many places. Will remove later.
                account_types.update_valid_account_names(
                    options.get_account_types(self.options))

    def DATE(self, year, month, day):
        """Process a DATE token.

        Args:
          year: integer year.
          month: integer month.
          day: integer day
        Returns:
          A new datetime object.
        """
        return datetime.date(year, month, day)

    def ACCOUNT(self, account_name):
        """Process an ACCOUNT token.

        This function attempts to reuse an existing account if one exists,
        otherwise creates one on-demand.

        Args:
          account_name: a str, the valid name of an account.
        Returns:
          A new Account object.
        """
        # Check account name validity.
        if not self.account_regexp.match(account_name):
            fileloc = FileLocation('<ACCOUNT>', 0)
            self.errors.append(
                ParserError(fileloc, "Invalid account name: {}".format(account_name), None))
            return account.join(self.options['name_equity'], 'InvalidAccountName')

        # Check account type validity.
        account_type = account_name_type(account_name)
        if account_type not in account_types.ACCOUNT_TYPES:
            self.errors.append(
                ParserError(fileloc, "Invalid account type for: {}".format(account_name), None))
            return account.join(self.options['name_equity'], 'InvalidAccountType')

        # Create an account, reusing their strings as we go.
        try:
            account_name = self.accounts[account_name]
        except KeyError:
            self.accounts[account_name] = account_name

        return account_name

    def CURRENCY(self, currency_name):
        """Process a CURRENCY token.

        Args:
          currency_name: the name of the currency.
        Returns:
          A new currency object; for now, these are simply represented
          as the currency name.
        """
        return currency_name

    def STRING(self, string):
        """Process a STRING token.

        Args:
          string: the string to process.
        Returns:
          The string. Nothing to be done or cleaned up. Eventually we might
          do some decoding here.
        """
        return string

    def TAG(self, tag):
        """Process a TAG token.

        Args:
          tag: a str, the tag to be processed.
        Returns:
          The tag string itself. For now we don't need an object to represent
          those; keeping it simple.
        """
        return tag

    def LINK(self, link):
        """Process a LINK token.

        Args:
          link: a str, the name of the string.
        Returns:
          The link string itself. For now we don't need to represent this by
          an object.
        """
        return link

    def NUMBER(self, number):
        """Process a NUMBER token. Convert into Decimal.

        Args:
          number: a str, the number to be converted.
        Returns:
          A Decimal instance built of the number string.
        """
        try:
            return Decimal(number)
        except Exception as e:
            raise e.__class__("Error: {} for token '{}' at line {}".format(
                e, number, _parser.get_yylineno()))

    def amount(self, number, currency):
        """Process an amount grammar rule.

        Args:
          number: a Decimal instance, the number of the amount.
          currency: a currency object (a str, really, see CURRENCY above)
        Returns:
          An instance of Amount.
        """
        assert isinstance(number, Decimal)
        assert isinstance(currency, str)
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

    def position(self, amount, lot_cost_date):
        """Process a position grammar rule.

        Args:
          amount: an instance of Amount for the position.
          lot_cost_date: a tuple of (cost, lot-date)
        Returns:
          A new instance of Position.
        """
        cost, lot_date, istotal = lot_cost_date if lot_cost_date else (None, None, False)
        if istotal:
            cost = amount_div(cost, amount.number)
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

    def error(self, message, filename, lineno):
        """Process an error rule.

        Args:
          message: the message to be printed.
          filename: the current filename
          lineno: the current line number
        Returns:
        """
        fileloc = FileLocation(filename, lineno)
        self.errors.append(ParserSyntaxError(fileloc, message, None))

    def open(self, filename, lineno, date, account, currencies):
        """Process an open directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance.
          currencies: a list of constraint currencies.
        Returns:
          A new Open object.
        """
        fileloc = FileLocation(filename, lineno)
        return Open(fileloc, date, account, currencies)

    def close(self, filename, lineno, date, account):
        """Process a close directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance.
        Returns:
          A new Close object.
        """
        fileloc = FileLocation(filename, lineno)
        return Close(fileloc, date, account)

    def pad(self, filename, lineno, date, account, account_pad):
        """Process a pad directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance, account to be padded.
          account_pad: an Account instance, account to pad from.
        Returns:
          A new Pad object.
        """
        fileloc = FileLocation(filename, lineno)
        return Pad(fileloc, date, account, account_pad)

    def balance(self, filename, lineno, date, account, amount):
        """Process an assertion directive.

        We produce no errors here by default. We replace the failing ones in the
        routine that does the verification later one, that these have succeeded
        or failed.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance.
          amount: the expected amount, to be checked.
        Returns:
          A new Balance object.
        """
        diff_amount = None
        fileloc = FileLocation(filename, lineno)
        return Balance(fileloc, date, account, amount, diff_amount)

    def event(self, filename, lineno, date, event_type, description):
        """Process an event directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          event_type: a str, the name of the event type.
          description: a str, the event value, the contents.
        Returns:
          A new Event object.
        """
        fileloc = FileLocation(filename, lineno)
        return Event(fileloc, date, event_type, description)

    def price(self, filename, lineno, date, currency, amount):
        """Process a price directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          currency: the currency to be priced.
          amount: an instance of Amount, that is the price of the currency.
        Returns:
          A new Price object.
        """
        fileloc = FileLocation(filename, lineno)
        return Price(fileloc, date, currency, amount)

    def note(self, filename, lineno, date, account, comment):
        """Process a note directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance.
          comment: a str, the note's comments contents.
        Returns:
          A new Note object.
        """
        fileloc = FileLocation(filename, lineno)
        return Note(fileloc, date, account, comment)

    def document(self, filename, lineno, date, account, document_filename):
        """Process a document directive.

        Args:
          filename: the current filename.
          lineno: the current line number.
          date: a datetime object.
          account: an Account instance.
          document_filename: a str, the name of the document file.
        Returns:
          A new Document object.
        """
        fileloc = FileLocation(filename, lineno)
        if not path.isabs(document_filename):
          document_filename = path.abspath(path.join(path.dirname(filename),
                                                     document_filename))

        return Document(fileloc, date, account, document_filename)

    def posting(self, account, position, price, istotal, flag):
        """Process a posting grammar rule.

        Args:
          account: an Account instance for the posting.
          position: an instance of Position from the grammar rule.
          price: Either None, or an instance of Amount that is the cost of the position.
          istotal: a bool, True if the price is for the total amount being parsed, or
                   False if the price is for each lot of the position.
          flag: a str, one-character, the flag associated with this posting.
        Returns:
          A new Posting object, with no parent entry.
        """
        # If the price is specified for the entire amount, compute the effective
        # price here and forget about that detail of the input syntax.
        if istotal:
            price = Amount(ZERO if position.number == ZERO else price.number / position.number, price.currency)
        return Posting(None, account, position, price, chr(flag) if flag else None)


    def txn_field_new(self):
        """Create a new TxnFields instance.

        Returns:
          An instance of TxnFields, initialized with expected attributes.
        """
        return TxnFields([], set(), set())

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
          stirng: A string, the new string to insert in the list.
        Returns:
          An updated TxnFields instance.
        """
        txn_fields.strings.append(string)
        return txn_fields

    def unpack_txn_strings(self, txn_fields, fileloc):
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
        elif num_strings == 2:
            payee, narration = txn_fields.strings
        elif num_strings == 0:
            payee, narration = None, ""
        else:
            self.errors.append(
                ParserError(fileloc, "Too many strings on transaction description: {}".format(
                    txn_fields.strings), None))
            return None
        return payee, narration

    def transaction(self, filename, lineno, date, flag, txn_fields, postings):
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
          postings: a list of Posting instances, to be inserted in this transaction.
        Returns:
          A new Transaction object.
        """
        fileloc = FileLocation(filename, lineno)

        # Unpack the transaction fields.
        payee_narration = self.unpack_txn_strings(txn_fields, fileloc)
        if payee_narration is None:
            return None
        payee, narration = payee_narration

        # Detect when a transaction does not have at least two legs.
        if postings is None or len(postings) < 2:
            self.errors.append(
                ParserError(fileloc, "Invalid number of postings: {}".format(postings), None))
            return None

        # Merge the tags from the stack with the explicit tags of this
        # transaction, or make None.
        tags = txn_fields.tags
        assert isinstance(tags, set)
        if self.tags:
            tags.update(self.tags)
        tags = frozenset(tags) if tags else None

        # Make links to None if empty.
        links = txn_fields.links
        links = frozenset(links) if links else None

        # Create the transaction. Note: we need to parent the postings.
        entry = Transaction(fileloc, date, chr(flag),
                            payee, narration, tags, links, postings)

        # Balance incomplete auto-postings and set the parent link to this entry as well.
        balance_errors = balance_incomplete_postings(entry)

        if balance_errors:
            self.errors.extend(balance_errors)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = compute_residual(entry.postings)
            assert residual.is_small(SMALL_EPSILON), residual

        return entry


def parse(filename, **kw):
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
    _parser.parse(path.abspath(filename), builder, **kw)
    entries = sorted(builder.entries, key=data.entry_sortkey)
    return (entries, builder.errors, builder.options)


def parse_string(input_string, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts.

    Args:
      input_string: a str, the contents to be parsed instead of a file's.
    Return:
      Same as the output of parse().
    """
    with tempfile.NamedTemporaryFile('w') as tmp_file:
        tmp_file.write(input_string)
        tmp_file.flush()
        return parse(tmp_file.name, **kw)


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
        entries, errors, options = parse_string(textwrap.dedent(fun.__doc__),
                                                report_filename=filename,
                                                report_firstline=lineno)
        return fun(self, entries, errors, options)
    newfun.__doc__ = None
    return newfun


class LexOnlyBuilder(object):
    """A builder used only for getting the lexer to pass.
    The methods do nothing."""

    def DATE(self, year, month, day): pass
    def ACCOUNT(self, s):             pass
    def CURRENCY(self, s):            pass
    def STRING(self, s):              pass
    def TAG(self, s):                 pass
    def NUMBER(self, s):              pass


def dump_lexer(filename, outfile):
    """Parse a beancount input file and print a list of transactions to stdout.

    Args:
      filename: a str, the name of the file to be parsed.
    """
    _parser.lexer_init(filename, LexOnlyBuilder())
    while 1:
        x = _parser.lexer_next()
        if x is None:
            break
        token, text, lineno = x
        outfile.write('{:12} {:6d} {}\n'.format(token, lineno, repr(text)))


def dump_lexer_string(input_string):
    """Parse a beancount input file and print a list of transactions.

    Args:
      input_string: a str, the contents of the ledger to be parsed.
    """
    with tempfile.NamedTemporaryFile('w') as tmp_file:
        tmp_file.write(input_string)
        tmp_file.flush()
        return dump_lexer(tmp_file.name)
