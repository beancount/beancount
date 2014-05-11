"""Basic data structures used to represent the Ledger entries.
"""
import datetime
from collections import namedtuple, defaultdict

# Note: this file is mirrorred into ledgerhub. Relative imports only.
from .amount import Amount, Decimal, to_decimal
from .position import Position, create_position


# All possible types of entries. These are the main data structrues in use
# within the program. They are all treated as immutable.
#
# Common Attributes:
#   fileloc: A FileLocation instance, denotes where the directive was parsed from.
#   date: A datetime.date instance; all directives have an associated date. Note:
#     Beancount does not consider time, only dates. The line where the directive
#     shows up in the file is used as a secondary sort key beyond the date.

# An "open account" directive.
#
# Attributes:
#   account: An Account, the account that is being opened.
#   currencies: A list of strings, currencies that are allowed in this account.
#     May be None, in which case it means that there are no restrictions on which
#     currencies may be stored in this account.
Open = namedtuple('Open', 'fileloc date account currencies')

# A "close account" directive.
#
# Attributes:
#   account: An Account, the account that is being closed.
Close = namedtuple('Close', 'fileloc date account')

# A "pad this account with this other account" directive. This directive
# automatically inserts transactions that will make the next chronological
# balance directive succeeds. It can be used to fill in missing date ranges of
# transactions, as a convenience. You don't have to use this, it's sugar coating
# in case you need it, while you're enterering past history into your Ledger.
#
# Attributes:
#   account: The Account which needs to be filled.
#   account_pad: The Account which is used to debit from in order to fill
#     'account'.
Pad = namedtuple('Pad', 'fileloc date account account_pad')

# A "check the balance of this account" directive. This directive asserts that
# the declared account should have a known number of units of a particular
# currency at the beginning of its date. This is essentially an assertion, and
# corresponds to the final "Statement Balance" line of a real-world statement.
# These assertions act as checkpoints to help ensure that you have entered your
# transactions correctly.
#
# Attributes:
#   account: The Account whose balance to check at the given date.
#   amount: An Amount, the number of units of the given currency you're
#     expecting 'account' to have at this date.
#   diff_amount: None if the balance check succeeds. This value is set to
#     an Amount instance if the balance fails, the amount of the difference.
Balance = namedtuple('Balance', 'fileloc date account amount diff_amount')

# A transaction! This is the main type of object that we manipulate, and the
# entire reason this whole project exists in the first place, because
# representing these types of structures with a spreadsheet is difficult.
#
# Attributes:
#   flag: A single-character string or None. This user-specified string
#     represents some custom/user-defined state of the transaction. You can use
#     this for various purposes. Otherwise common, pre-defined flags are defined
#     under beancount.core.flags, to flags transactions that are automatically
#     generated.
#   payee: A free-form string that identifies the payee, or None, if absent.
#   narration: A free-form string that provides a description for the transaction.
#     All transactions have at least a narration string, this is never None.
#   tags: A set of tag strings (without the '#'), or None, if an empty set.
#   links: A set of link strings (without the '^'), or None, if an empty set.
#   postings: A list of Posting instances, the legs of this transaction. See the
#     doc under Posting below.
Transaction = namedtuple('Transaction', 'fileloc date flag payee narration tags links postings')

# A note directive, a general note that is attached to an account. These are
# used to attach text at a particular date in a specific account. The notes can
# be anything; a typical use would be to jot down an answer from a phone call to
# the institution represented by the account. It should show up in an account's
# journal. If you don't want this rendered, use the comment syntax in the input
# file, which does not get parsed and stored.
#
# Attributes:
#   account: An Account which the note is to be attached to. This is never None,
#     notes always have an account they correspond to.
#   comment: A free-form string, the text of the note. This can be logn if you
#     want it to.
Note = namedtuple('Note', 'fileloc date account comment')

# An "event value change" directive. These directives are used as string
# variables that have different values over time. You can use these to track an
# address, your location, your current employer, anything you like. The kind of
# reporting that is made of these generic events is based on days and a
# timeline. For instance, if you need to track the number of days you spend in
# each country or state, create a "location" event and whenever you travel, add
# an event directive to indicate its new value. You should be able to write
# simple scripts against those in order to compute if you were present somewhere
# for a particular number of days. Here's an illustrative example usage, in
# order to maintain your health insurance coverage in Canada, you need to be
# present in the country for 183 days or more, excluding trips of less than 30
# days. There is a similar test to be done in the US by aliens to figure out if
# they need to be considered as residents for tax purposes (the so-called
# "subtantial presence test"). By integrating these directives into your
# bookkeeping, you can easily have a little program that computes the tests for
# you. This is, of course, entirely optional and somewhat auxiliary to the main
# purpose of double-entry bookkeeping, but correlates strongly with the
# transactions you insert in it, and so it's a really convenient thing to have
# in the same input file.
#
# Attributes:
#   type: A short string, typically a single lowercase word, that defines a
#     unique variable whose value changes over time. For example, 'location'.
#   description: A free-form string, the value of the variable as of the date
#     of the transaction.
Event = namedtuple('Event', 'fileloc date type description')

# A price declaration directive. This establishes the price of a currency in
# terms of another currency as of the directive's date. A history of the prices
# for each currency pairs is built and can be queried within the bookkeeping
# system. Note that because Beancount does not store any data at time-of-day
# resolution, it makes no sense to have multiple price directives at the same
# date. (Beancount will not attempt to solve this problem; this is beyond the
# general scope of double-entry bookkeeping and if you need to build a day
# trading system, you should probably use something else).
#
# Attributes:
#  currency: A string, the currency that is being priced, e.g. GOOG.
#  amount: An instance of Amount, the number of units and currency that
#    'currency' is worth, for instance 1200.12 USD.
Price = namedtuple('Price', 'fileloc date currency amount')

# A document file declaration directive. This directive is used to attach a
# statement to an account, at a particular date. A typical usage would be to
# render PDF files or scans of your bank statements into the account's journal.
# While you can explicitly create those directives in the input syntax, it is
# much more convenient to provide Beancount with a root directory to search for
# filenames in a hirerarchy mirroring the chart of accounts, filenames which
# should match the following dated format: "YYYY-MM-DD.*". See options for
# detail. Beancount will automatically create these documents directives based
# on the file hierarchy, and you can get them by parsing the list of entries.
#
# Attributes:
#   account: An Account, which the statement or document is associated with.
#   filename: The absolute filename of the document file.
Document = namedtuple('Document', 'fileloc date account filename')


# A list of all the valid directive types.
ALL_DIRECTIVES = (
  Open, Close, Pad, Balance, Transaction, Note, Event, Price, Document,
)


# The location in a source file where the directive was read from. These are
# attached to all the directives above.
#
# Attributes:
#   filename: A string, the name of the input that the directive was read from.
#   lineno: An integer, the line number where the directive was found. For
#     automatically created directives, this may be None.
FileLocation = namedtuple('FileLocation', 'filename lineno')


# Postings are contained in Transaction entries. These represent the individual
# legs of a transaction. Note: a posting may only appear within a single entry
# (multiple transactions may not share a Posting instance), and that's what the
# entry field should be set to.
#
# Attributes:
#   entry: A Transaction instance (see above), which the posting applies to.
#     It is convenient to have Posting instances point to their parent entries,
#     because account journals contain lists of Postings and non-Transaction
#     entries and though it creates a circular dependency between Transaction
#     and Posting, it allows us to easily resolve the lists of Postings to their
#     transactions for rendering.
#   account: An Account, the account that is modified by this posting.
#   position: An instance of Position (see position.py), the amount and lot that
#     is to be posted to this leg's account.
#   price: The price at which the position took place, or None, where not
#     relevant. Providing a price member to a posting automatically adds a
#     price in the prices database at the date of the transaction.
#   flag: An optional flag, a one-character string or None, which is to be
#     associated with the posting. Most postings don't have a flag, but it can
#     be convenient to mark a particular posting as problematic or pending to
#     be reconciled for a future import of its account.
Posting = namedtuple('Posting', 'entry account position price flag')


def strip_back_reference(entry):
    """Strip the postings back-reference to its transaction.
    This is used for testing, because the Python comparison routines
    for tuples/namedtuples don't deal with circular references too well.

    Args:
      entry: An instance of Transaction.
    Returns:
      A new instance of Transaction, with everything the same except
      for the backreference of posting.entry to the entry. These are
      replaced by None.
    """
    return entry._replace(
        postings=[posting._replace(entry=None)
                  for posting in entry.postings])


def create_simple_posting(entry, account, number, currency):
    """Create a simple posting on the entry, with just a number and currency (no cost).

    Args:
      entry: the entry instance to add the posting to
      account: an instance of Account to use on the posting
      number: a Decimal number or string to use in the posting's Amount
      currency: a string, the currency for the Amount
    Returns:
      An instance of Posting, and as a side-effect the entry has had its list of
      postings modified with the new Posting instance.
    """
    if isinstance(account, str):
        pass
    if number is None:
        position = None
    else:
        if not isinstance(number, Decimal):
            number = to_decimal(number)
        position = create_position(number, currency)
    posting = Posting(entry, account, position, None, None)
    if entry is not None:
        entry.postings.append(posting)
    return posting

def create_simple_posting_with_cost(entry, account, number, currency, cost_number, cost_currency):
    """Create a simple posting on the entry, with just a number and currency (no cost).

    Args:
      entry: the entry instance to add the posting to
      account: an instance of Account to use on the posting
      number: a Decimal number or string to use in the posting's Amount
      currency: a string, the currency for the Amount
      cost_number: a Decimal number or string to use for the posting's cost Amount
      cost_currency: a string, the currency for the cost Amount
    Returns:
      An instance of Posting, and as a side-effect the entry has had its list of
      postings modified with the new Posting instance.
    """
    if isinstance(account, str):
        pass
    if not isinstance(number, Decimal):
        number = to_decimal(number)
    if cost_number and not isinstance(cost_number, Decimal):
        cost_number = to_decimal(cost_number)
    cost = Amount(cost_number, cost_currency)
    position = create_position(number, currency, cost)
    posting = Posting(entry, account, position, None, None)
    if entry is not None:
        entry.postings.append(posting)
    return posting


NoneType = type(None)

def sanity_check_types(entry):
    """Check that the entry and its postings has all correct data types.

    Args:
      entry: an instance of one of the entries to be checked.
    """
    assert isinstance(entry, (Transaction, Open, Close, Pad, Balance, Note, Event, Price))
    assert isinstance(entry.fileloc, FileLocation)
    assert isinstance(entry.date, datetime.date)
    if isinstance(entry, Transaction):
        assert isinstance(entry.flag, (NoneType, str))
        assert isinstance(entry.payee, (NoneType, str))
        assert isinstance(entry.narration, (NoneType, str))
        assert isinstance(entry.tags, (NoneType, set, frozenset))
        assert isinstance(entry.links, (NoneType, set, frozenset))
        assert isinstance(entry.postings, list)
        for posting in entry.postings:
            assert isinstance(posting, Posting)
            assert posting.entry is entry
            assert isinstance(posting.account, str)
            assert isinstance(posting.position, (Position, NoneType))
            assert isinstance(posting.price, (Amount, NoneType))
            assert isinstance(posting.flag, (str, NoneType))


def entry_replace(entry, **replacements):
    """Replace components of an entry, reparenting postings automatically.
    This is necessary because we use immutable namedtuple instances, with
    circular references between entry and postings. It is a bit annoying,
    but it does not occur in many places, so we live with it, enjoying the
    extra convenience that circular refs provide, especially in lists of
    postings.

    Args:
      entry: the entry whose components to replace
      **replacements: replacements to apply to the entry
    Returns:
      A new entry, with postings correctly reparented.
    """
    new_entry = entry._replace(postings=[], **replacements)
    new_entry.postings.extend(posting._replace(entry=new_entry)
                              for posting in entry.postings)
    return new_entry


def reparent_posting(posting, entry):
    """Create a new posting entry that has the parent field set.

    Note that this does not modify the list of postings in 'entry', i.e. entry
    is left unmodified.

    Args:
      posting: a posting whose parent to set to 'entry'.
      entry: the entry to set on the posting.
    Return:
      The modified posting. Note that the unmodified posting itself it returned
      if the given entry is already the one on the posting.
    """
    if posting.entry is entry:
        return posting
    else:
        return posting._replace(entry=entry)


def posting_has_conversion(posting):
    """Return true if this position involves a conversion.

    A conversion is when there is a price attached to the amount but no cost.
    This is used on transactions to convert between units.

    Args:
      posting: an instance of Posting
    Return:
      A boolean, true if this posting has a price conversion.
    """
    return (posting.position.lot.cost is None and
            posting.price is not None)


def transaction_has_conversion(transaction):
    """Given a Transaction entry, return true if at least one of
    the postings has a price conversion (without an associated
    cost). These are the source of non-zero conversion balances.

    Args:
      transaction: an instance of a Transaction entry.
    Returns:
      A boolean, true if this transacation contains at least one posting with a
      price conversion.
    """
    assert isinstance(transaction, Transaction)
    for posting in transaction.postings:
        if posting_has_conversion(posting):
            return True
    return False


def get_entry(posting_or_entry):
    """Return the entry associated with the posting or entry.

    Args:
      entry: A Posting or entry instance
    Returns:
      A datetime instance.
    """
    return (posting_or_entry.entry
            if isinstance(posting_or_entry, Posting)
            else posting_or_entry)


# Sort with the checks at the BEGINNING of the day.
SORT_ORDER = {Open: -2, Balance: -1, Close: 1}


def entry_sortkey(entry):
    """Sort-key for entries. We sort by date, except that checks
    should be placed in front of every list of entries of that same day,
    in order to balance linearly.

    Args:
      entry: An entry instance.
    Returns:
      A tuple of (date, integer, integer), that forms the sort key for the
      entry.
    """
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.fileloc.lineno)


def posting_sortkey(entry):
    """Sort-key for entries or postings. We sort by date, except that checks
    should be placed in front of every list of entries of that same day,
    in order to balance linearly.

    Args:
      entry: A Posting or entry instance
    Returns:
      A tuple of (date, integer, integer), that forms the sort key for the
      posting or entry.
    """
    if isinstance(entry, Posting):
        entry = entry.entry
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.fileloc.lineno)
