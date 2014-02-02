"""Basic data structures used to represent the Ledger entries.
"""
import io
import datetime
import textwrap
from collections import namedtuple

from beancount.core.amount import Amount, Decimal, to_decimal
from beancount.core.account import Account, account_from_name
from beancount.core.position import Lot, Position


# All possible types of entries. See the documentation for these.
Open        = namedtuple('Open'        , 'fileloc date account currencies')
Close       = namedtuple('Close'       , 'fileloc date account')
Pad         = namedtuple('Pad'         , 'fileloc date account account_pad')
Balance     = namedtuple('Balance'     , 'fileloc date account amount errdiff')
Transaction = namedtuple('Transaction' , 'fileloc date flag payee narration tags links postings')
Note        = namedtuple('Note'        , 'fileloc date account comment')
Event       = namedtuple('Event'       , 'fileloc date type description')
Price       = namedtuple('Price'       , 'fileloc date currency amount')
Document    = namedtuple('Document'    , 'fileloc date account filename')


# The location in a source file where the directive was read from.
FileLocation = namedtuple('FileLocation', 'filename lineno')


# Postings are contained in Transaction entries.
# Note: a posting may only list within a single entry, and that's what the entry
# field should be set to.
Posting = namedtuple('Posting', 'entry account position price flag')


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
        account = account_from_name(account)
    if number is None:
        position = None
    else:
        if not isinstance(number, Decimal):
            number = to_decimal(number)
        position = Position(Lot(currency, None, None), Decimal(number))
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
        account = account_from_name(account)
    if not isinstance(number, Decimal):
        number = to_decimal(number)
    if cost_number and not isinstance(cost_number, Decimal):
        cost_number = to_decimal(cost_number)
    cost = Amount(cost_number, cost_currency)
    position = Position(Lot(currency, cost, None), Decimal(number))
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
            assert isinstance(posting.account, Account)
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

# Sort with the checks at the END of the day.
#SORT_ORDER = {Open: -2, Balance: 1, Close: 2}


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


def filter_link(link, entries):
    """Yield all the entries which have the given link.

    Args:
      link: A string, the link we are interested in.
    Yields:
      Every entry in 'entries' that links to 'link.
    """
    for entry in entries:
        if (isinstance(entry, Transaction) and
            entry.links and link in entry.links):
            yield entry
