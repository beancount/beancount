"""Basic data structures used to represent the Ledger entries."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import builtins
import datetime
import enum
import sys
from decimal import Decimal
from typing import Any
from typing import Iterator
from typing import NamedTuple
from typing import Optional
from typing import Protocol
from typing import Union
from typing import overload

from beancount.core.account import has_component
from beancount.core.amount import Amount
from beancount.core.number import D
from beancount.core.position import Cost
from beancount.core.position import CostSpec
from beancount.utils.bisect_key import bisect_left_with_key

# Type declarations.
Account = str
Currency = str
Flag = str
Meta = dict[str, Any]


class BeancountError(Protocol):
    """Beancount errors are objects with these attributes"""

    @property
    def source(self) -> Meta: ...
    @property
    def message(self) -> str: ...
    @property
    def entry(self) -> Directive | None: ...


# An immutable constant for all empty sets. This is used to set links and tags
# and ensure that they never has a None value. This makes some of the processing
# code a bit simpler.
EMPTY_SET: frozenset[str] = frozenset()


# A set of valid booking method names for positions on accounts.
# See http://furius.ca/beancount/doc/inventories for a full explanation.
@enum.unique
class Booking(enum.Enum):
    # Reject ambiguous matches with an error.
    STRICT = "STRICT"

    # Strict booking method, but disambiguate further with sizes. Reject
    # ambiguous matches with an error but if a lot matches the size exactly,
    # accept it the oldest.
    STRICT_WITH_SIZE = "STRICT_WITH_SIZE"

    # Disable matching and accept the creation of mixed inventories.
    NONE = "NONE"

    # Average cost booking: merge all matching lots before and after.
    AVERAGE = "AVERAGE"

    # First-in first-out in the case of ambiguity.
    FIFO = "FIFO"

    # Last-in first-out in the case of ambiguity.
    LIFO = "LIFO"

    # Highest-in first-out in the case of ambiguity.
    HIFO = "HIFO"


# All possible types of entries. These are the main data structures in use
# within the program. They are all treated as immutable.
#
# Common Attributes (prepended to declared list):
#   meta: A dict of strings to objects, potentially attached to each of the
#     directive types. The values may be strings, account names, tags, dates,
#     numbers, amounts and currencies. There are two special attributes which
#     are always present on all directives: 'filename' and 'lineno'.
#   date: A datetime.date instance; all directives have an associated date. Note:
#     Beancount does not consider time, only dates. The line where the directive
#     shows up in the file is used as a secondary sort key beyond the date.


class Open(NamedTuple):
    """
    An "open account" directive.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the name of the account that is being opened.
      currencies: A list of strings, currencies that are allowed in this account.
        May be None, in which case it means that there are no restrictions on which
        currencies may be stored in this account.
      booking: A Booking enum, the booking method to use to disambiguate
        postings to this account (when zero or more than one postings match the
        specification), or None if not specified. In practice, this attribute will
        be should be left unspecified (None) in the vast majority of cases. See
        Booking below for a selection of valid methods.
    """

    meta: Meta
    date: datetime.date
    account: Account
    currencies: list[Currency]
    booking: Optional[Booking]


class Close(NamedTuple):
    """
    A "close account" directive.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the name of the account that is being closed.
    """

    meta: Meta
    date: datetime.date
    account: Account


class Commodity(NamedTuple):
    """
    An optional commodity declaration directive. Commodities generally do not need
    to be declared, but they may, and this is mainly created as intended to be
    used to attach meta-data on a commodity name. Whenever a plugin needs
    per-commodity meta-data, you would define such a commodity directive. Another
    use is to define a commodity that isn't otherwise (yet) used anywhere in an
    input file. (At the moment the date is meaningless but is specified for
    coherence with all the other directives; if you can think of a good use case,
    let us know).

    Attributes:
      meta: See above.
      date: See above.
      currency: A string, the commodity under consideration.
    """

    meta: Meta
    date: datetime.date
    currency: Currency


class Pad(NamedTuple):
    """
    A "pad this account with this other account" directive. This directive
    automatically inserts transactions that will make the next chronological
    balance directive succeeds. It can be used to fill in missing date ranges of
    transactions, as a convenience. You don't have to use this, it's sugar coating
    in case you need it, while you're entering past history into your Ledger.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the name of the account which needs to be filled.
      source_account: A string, the name of the account which is used to debit from
        in order to fill 'account'.
    """

    meta: Meta
    date: datetime.date
    account: Account
    source_account: Account


class Balance(NamedTuple):
    """
    A "check the balance of this account" directive. This directive asserts that
    the declared account should have a known number of units of a particular
    currency at the beginning of its date. This is essentially an assertion, and
    corresponds to the final "Statement Balance" line of a real-world statement.
    These assertions act as checkpoints to help ensure that you have entered your
    transactions correctly.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the account whose balance to check at the given date.
      amount: An Amount, the number of units of the given currency you're
        expecting 'account' to have at this date.
      diff_amount: None if the balance check succeeds. This value is set to
        an Amount instance if the balance fails, the amount of the difference.
      tolerance: A Decimal object, the amount of tolerance to use in the
        verification.
    """

    meta: Meta
    date: datetime.date
    account: Account
    amount: Amount
    tolerance: Optional[Decimal]
    diff_amount: Optional[Amount]


class Posting(NamedTuple):
    """
    Postings are contained in Transaction entries. These represent the individual
    legs of a transaction. Note: a posting may only appear within a single entry
    (multiple transactions may not share a Posting instance), and that's what the
    entry field should be set to.

    Attributes:
      account: A string, the account that is modified by this posting.
      units: An Amount, the units of the position, or None if it is to be
        inferred from the other postings in the transaction.
      cost: A Cost or CostSpec instances, the units of the position.
      price: An Amount, the price at which the position took place, or
        None, where not relevant. Providing a price member to a posting
        automatically adds a price in the prices database at the date of the
        transaction.
      flag: An optional flag, a one-character string or None, which is to be
        associated with the posting. Most postings don't have a flag, but it can
        be convenient to mark a particular posting as problematic or pending to
        be reconciled for a future import of its account.
      meta: A dict of strings to values, the metadata that was attached
        specifically to that posting, or None, if not provided. In practice, most
        of the instances will be unlikely to have metadata.
    """

    account: Account
    units: Optional[Amount]
    cost: Optional[Union[Cost, CostSpec]]
    price: Optional[Amount]
    flag: Optional[Flag]
    meta: Optional[Meta]


class Transaction(NamedTuple):
    """
    A transaction! This is the main type of object that we manipulate, and the
    entire reason this whole project exists in the first place, because
    representing these types of structures with a spreadsheet is difficult.

    Attributes:
      meta: See above.
      date: See above.
      flag: A single-character string or None. This user-specified string
        represents some custom/user-defined state of the transaction. You can use
        this for various purposes. Otherwise common, pre-defined flags are defined
        under beancount.core.flags, to flags transactions that are automatically
        generated.
      payee: A free-form string that identifies the payee, or None, if absent.
      narration: A free-form string that provides a description for the transaction.
        All transactions have at least a narration string, this is never None.
      tags: A set of tag strings (without the '#'), or EMPTY_SET.
      links: A set of link strings (without the '^'), or EMPTY_SET.
      postings: A list of Posting instances, the legs of this transaction. See the
        doc under Posting above.
    """

    meta: Meta
    date: datetime.date
    flag: Flag
    payee: Optional[str]
    narration: Optional[str]
    tags: frozenset[str]
    links: frozenset[str]
    postings: list[Posting]


class TxnPosting(NamedTuple):
    """
    A pair of a Posting and its parent Transaction. This is inserted as
    temporaries in lists of postings-of-entries, which is the product of a
    realization.

    Attributes:
      txn: The parent Transaction instance.
      posting: The Posting instance.
    """

    txn: Transaction
    posting: Posting


class Note(NamedTuple):
    """
    A note directive, a general note that is attached to an account. These are
    used to attach text at a particular date in a specific account. The notes can
    be anything; a typical use would be to jot down an answer from a phone call to
    the institution represented by the account. It should show up in an account's
    journal. If you don't want this rendered, use the comment syntax in the input
    file, which does not get parsed and stored.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the account which the note is to be attached to. This is
        never None, notes always have an account they correspond to.
      comment: A free-form string, the text of the note. This can be long if you
        want it to.
    """

    meta: Meta
    date: datetime.date
    account: Account
    comment: str
    tags: Optional[frozenset[str]]
    links: Optional[frozenset[str]]


class Event(NamedTuple):
    """
    An "event value change" directive. These directives are used as string
    variables that have different values over time. You can use these to track an
    address, your location, your current employer, anything you like. The kind of
    reporting that is made of these generic events is based on days and a
    timeline. For instance, if you need to track the number of days you spend in
    each country or state, create a "location" event and whenever you travel, add
    an event directive to indicate its new value. You should be able to write
    simple scripts against those in order to compute if you were present somewhere
    for a particular number of days. Here's an illustrative example usage, in
    order to maintain your health insurance coverage in Canada, you need to be
    present in the country for 183 days or more, excluding trips of less than 30
    days. There is a similar test to be done in the US by aliens to figure out if
    they need to be considered as residents for tax purposes (the so-called
    "substantial presence test"). By integrating these directives into your
    bookkeeping, you can easily have a little program that computes the tests for
    you. This is, of course, entirely optional and somewhat auxiliary to the main
    purpose of double-entry bookkeeping, but correlates strongly with the
    transactions you insert in it, and so it's a really convenient thing to have
    in the same input file.

    Attributes:
      meta: See above.
      date: See above.
      "type": A short string, typically a single lowercase word, that defines a
        unique variable whose value changes over time. For example, 'location'.
      description: A free-form string, the value of the variable as of the date
        of the transaction.
    """

    meta: Meta
    date: datetime.date
    type: str
    description: str


class Query(NamedTuple):
    """
    A named query declaration. This directive is used to create pre-canned queries
    that can then be automatically run or made available to the shell, or perhaps be
    rendered as part of a web interface. The purpose of this routine is to define
    useful queries for the context of the particular given Beancount input file.

    Attributes:
      meta: See above.
      date: The date at which this query should be run. All directives following
        this date will be ignored automatically. This is essentially equivalent to
        the CLOSE modifier in the shell syntax.
      name: A string, the unique identifier for the query.
      query_string: The SQL query string to be run or made available.
    """

    meta: Meta
    date: datetime.date
    name: str
    query_string: str


class Price(NamedTuple):
    """
    A price declaration directive. This establishes the price of a currency in
    terms of another currency as of the directive's date. A history of the prices
    for each currency pairs is built and can be queried within the bookkeeping
    system. Note that because Beancount does not store any data at time-of-day
    resolution, it makes no sense to have multiple price directives at the same
    date. (Beancount will not attempt to solve this problem; this is beyond the
    general scope of double-entry bookkeeping and if you need to build a day
    trading system, you should probably use something else).

    Attributes:
      meta: See above.
      date: See above.
     currency: A string, the currency that is being priced, e.g. HOOL.
     amount: An instance of Amount, the number of units and currency that
       'currency' is worth, for instance 1200.12 USD.
    """

    meta: Meta
    date: datetime.date
    currency: Currency
    amount: Amount


class Document(NamedTuple):
    """
    A document file declaration directive. This directive is used to attach a
    statement to an account, at a particular date. A typical usage would be to
    render PDF files or scans of your bank statements into the account's journal.
    While you can explicitly create those directives in the input syntax, it is
    much more convenient to provide Beancount with a root directory to search for
    filenames in a hierarchy mirroring the chart of accounts, filenames which
    should match the following dated format: "YYYY-MM-DD.*". See options for
    detail. Beancount will automatically create these documents directives based
    on the file hierarchy, and you can get them by parsing the list of entries.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the account which the statement or document is associated
        with.
      filename: The absolute filename of the document file.
      tags: A set of tag strings (without the '#'), or None, if an empty set.
      links: A set of link strings (without the '^'), or None, if an empty set.
    """

    meta: Meta
    date: datetime.date
    account: Account
    filename: str
    tags: Optional[frozenset[str]]
    links: Optional[frozenset[str]]


class Custom(NamedTuple):
    """
    A custom directive. This directive can be used to implement new experimental
    dated features in the Beancount file. This is meant as an intermediate measure
    to be used when you would need to implement a new directive in a plugin. These
    directives will be parsed liberally... any list of tokens are supported. All
    that is required is some unique name for them that acts as a "type". These
    directives are included in the stream and a plugin should be able to gather
    them.

    Attributes:
      meta: See above.
      type: A string that represents the type of the directive.
      values: A list of values of various simple types supported by the grammar.
        (Note that this list is not enforced to be consistent for all directives
        of the same type by the parser.)
    """

    meta: Meta
    date: datetime.date
    type: str
    values: list[Any]


# A list of all the valid directive types.
ALL_DIRECTIVES = (
    Open,
    Close,
    Commodity,
    Pad,
    Balance,
    Transaction,
    Note,
    Event,
    Query,
    Price,
    Document,
    Custom,
)

# Type for any of the directives.
Directive = Union[
    Open,
    Close,
    Commodity,
    Pad,
    Balance,
    Transaction,
    Note,
    Event,
    Query,
    Price,
    Document,
    Custom,
]


class dtypes:
    "Types of directives."

    Open = Open
    Close = Close
    Commodity = Commodity
    Pad = Pad
    Balance = Balance
    Transaction = Transaction
    Note = Note
    Event = Event
    Query = Query
    Price = Price
    Document = Document
    Custom = Custom


# Type for the list of entries and options map.
# Note: In v3, make the terminology 'Entries' obsolete.
Entries = list[Directive]
Directives = list[Directive]
Options = dict[str, Any]


def new_metadata(filename: str, lineno: int, kvlist: Meta | None = None) -> Meta:
    """Create a new metadata container from the filename and line number.

    Args:
      filename: A string, the filename for the creator of this directive.
      lineno: An integer, the line number where the directive has been created.
      kvlist: An optional container of key-values.
    Returns:
      A metadata dict.
    """
    meta = {"filename": filename, "lineno": lineno}
    if kvlist:
        meta.update(kvlist)
    return meta


# Either both number and currency should be provided or both should be None
@overload
def create_simple_posting(
    entry: Transaction, account: Account, number: Decimal | str, currency: str
) -> Posting: ...


@overload
def create_simple_posting(
    entry: Transaction, account: Account, number: None, currency: None
) -> Posting: ...


def create_simple_posting(
    entry: Transaction, account: Account, number: Decimal | str | None, currency: str | None
) -> Posting:
    """Create a simple posting on the entry, with just a number and currency (no cost).

    Args:
      entry: The entry instance to add the posting to.
      account: A string, the account to use on the posting.
      number: A Decimal number or string to use in the posting's Amount.
      currency: A string, the currency for the Amount.
    Returns:
      An instance of Posting, and as a side-effect the entry has had its list of
      postings modified with the new Posting instance.
    """
    if isinstance(account, str):
        pass
    if number is None or currency is None:
        units = None
    else:
        if not isinstance(number, Decimal):
            number = D(number)
        units = Amount(number, currency)
    posting = Posting(account, units, None, None, None, None)
    if entry is not None:
        entry.postings.append(posting)
    return posting


def create_simple_posting_with_cost(
    entry: Transaction,
    account: Account,
    number: Decimal | str,
    currency: str,
    cost_number: Decimal | str,
    cost_currency: str,
) -> Posting:
    """Create a simple posting on the entry, with a number and currency and cost.

    Args:
      entry: The entry instance to add the posting to.
      account: A string, the account to use on the posting.
      number: A Decimal number or string to use in the posting's Amount.
      currency: A string, the currency for the Amount.
      cost_number: A Decimal number or string to use for the posting's cost Amount.
      cost_currency: a string, the currency for the cost Amount.
    Returns:
      An instance of Posting, and as a side-effect the entry has had its list of
      postings modified with the new Posting instance.
    """
    if isinstance(account, str):
        pass
    if not isinstance(number, Decimal):
        number = D(number)
    if cost_number is not None and not isinstance(cost_number, Decimal):
        cost_number = D(cost_number)
    units = Amount(number, currency)
    cost = Cost(cost_number, cost_currency, entry.date, None)
    posting = Posting(account, units, cost, None, None, None)
    if entry is not None:
        entry.postings.append(posting)
    return posting


NoneType: type = type(None)


def sanity_check_types(
    entry: Directive, allow_none_for_tags_and_links: bool = False
) -> None:
    """Check that the entry and its postings has all correct data types.

    Args:
      entry: An instance of one of the entries to be checked.
      allow_none_for_tags_and_links: A boolean, whether to allow plugins to
        generate Transaction objects with None as value for the 'tags' or 'links'
        attributes.
    Raises:
      AssertionError: If there is anything that is unexpected, raises an exception.
    """
    assert isinstance(entry, ALL_DIRECTIVES), "Invalid directive type"
    assert isinstance(entry.meta, dict), "Invalid type for meta"
    assert "filename" in entry.meta, "Missing filename in metadata"
    assert "lineno" in entry.meta, "Missing line number in metadata"
    assert isinstance(entry.date, datetime.date), "Invalid date type"
    if isinstance(entry, Transaction):
        assert isinstance(entry.flag, str), "Invalid flag type"
        assert isinstance(entry.payee, (NoneType, str)), "Invalid payee type"
        assert isinstance(entry.narration, (NoneType, str)), "Invalid narration type"
        set_types = (
            (NoneType, set, frozenset)
            if allow_none_for_tags_and_links
            else (set, frozenset)
        )
        assert isinstance(entry.tags, set_types), "Invalid tags type: {}".format(
            type(entry.tags)
        )
        assert isinstance(entry.links, set_types), "Invalid links type: {}".format(
            type(entry.links)
        )
        assert isinstance(entry.postings, list), "Invalid postings list type"
        for posting in entry.postings:
            assert isinstance(posting, Posting), "Invalid posting type"
            assert isinstance(posting.account, str), "Invalid account type"
            assert isinstance(posting.units, (Amount, NoneType)), "Invalid units type"
            assert isinstance(posting.cost, (Cost, CostSpec, NoneType)), "Invalid cost type"
            assert isinstance(posting.price, (Amount, NoneType)), "Invalid price type"
            assert isinstance(posting.flag, (str, NoneType)), "Invalid flag type"


def posting_has_conversion(posting: Posting) -> bool:
    """Return true if this position involves a conversion.

    A conversion is when there is a price attached to the amount but no cost.
    This is used on transactions to convert between units.

    Args:
      posting: an instance of Posting
    Return:
      A boolean, true if this posting has a price conversion.
    """
    return posting.cost is None and posting.price is not None


def transaction_has_conversion(transaction: Transaction) -> bool:
    """Given a Transaction entry, return true if at least one of
    the postings has a price conversion (without an associated
    cost). These are the source of non-zero conversion balances.

    Args:
      transaction: an instance of a Transaction entry.
    Returns:
      A boolean, true if this transaction contains at least one posting with a
      price conversion.
    """
    assert isinstance(
        transaction, Transaction
    ), "Invalid type of entry for transaction: {}".format(transaction)
    for posting in transaction.postings:
        if posting_has_conversion(posting):
            return True
    return False


def get_entry(posting_or_entry: Directive | TxnPosting) -> Directive:
    """Return the entry associated with the posting or entry.

    Args:
      entry: A TxnPosting or entry instance
    Returns:
      A datetime instance.
    """
    return (
        posting_or_entry.txn
        if isinstance(posting_or_entry, TxnPosting)
        else posting_or_entry
    )


# Sorting order of directives on the same day, by type:
# - Open entries should always be first.
# - Balance entries should appear before Transactions, because
#   they are defined to apply at the beginning of the day.
# - All other directives come next (including Transactions).
# - Document directives should appear after Transactions because
#   they can be inserted on the statement date, which may include
#   transactions on that date.
# - Close directives should always appear last.
# This is the rationale for this sorting order.
SORT_ORDER = {Open: -2, Balance: -1, Document: 1, Close: 2}


def entry_sortkey(entry: Directive) -> tuple[datetime.date, int, int]:
    """Sort-key for entries. We sort by date, except that checks
    should be placed in front of every list of entries of that same day,
    in order to balance linearly.

    Args:
      entry: An entry instance.
    Returns:
      A tuple of (date, integer, integer), that forms the sort key for the
      entry.
    """
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.meta["lineno"])


def sorted(entries: Directives) -> Directives:
    """A convenience to sort a list of entries, using entry_sortkey().

    Args:
      entries: A list of directives.
    Returns:
      A sorted list of directives.
    """
    return builtins.sorted(entries, key=entry_sortkey)


def posting_sortkey(entry: Directive | TxnPosting) -> tuple[datetime.date, int, int]:
    """Sort-key for entries or TxnPosting instances. We sort by date, except
    that checks should be placed in front of every list of entries of that same
    day, in order to balance linearly.

    Args:
      entry: A TxnPosting or entry instance.
    Returns:
      A tuple of (date, integer, integer), that forms the sort key for the
      TxnPosting or entry.
    """
    assert isinstance(entry, (TxnPosting,) + ALL_DIRECTIVES)
    if isinstance(entry, TxnPosting):
        entry = entry.txn
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.meta["lineno"])


# TODO(blais): Rename 'txns' to 'transactions' for clarity.
def filter_txns(entries: Directives) -> Iterator[Transaction]:
    """A generator that yields only the Transaction instances.

    This is such an incredibly common operation that it deserves a terse
    filtering mechanism.

    Args:
      entries: A list of directives.
    Yields:
      A sorted list of only the Transaction directives.
    """
    for entry in entries:
        if isinstance(entry, Transaction):
            yield entry


def has_entry_account_component(entry: Directive, component: str) -> bool:
    """Return true if one of the entry's postings has an account component.

    Args:
      entry: A Transaction entry.
      component: A string, a component of an account name. For instance,
        ``Food`` in ``Expenses:Food:Restaurant``. All components are considered.
    Returns:
      Boolean: true if the component is in the account. Note that a component
      name must be whole, that is ``NY`` is not in ``Expenses:Taxes:StateNY``.
    """
    return isinstance(entry, Transaction) and any(
        has_component(posting.account, component) for posting in entry.postings
    )


def find_closest(entries: Directives, filename: str, lineno: int) -> Directive | None:
    """Find the closest entry from entries to (filename, lineno).

    Args:
      entries: A list of directives.
      filename: A string, the name of the ledger file to look for. Be careful
        to provide the very same filename, and note that the parser stores the
        absolute path of the filename here.
      lineno: An integer, the line number closest after the directive we're
        looking for. This may be the exact/first line of the directive.
    Returns:
      The closest entry found in the given file for the given filename, or
      None, if none could be found.
    """
    min_diffline = sys.maxsize
    closest_entry = None
    for entry in entries:
        emeta = entry.meta
        if emeta["filename"] == filename and emeta["lineno"] > 0:
            diffline = lineno - emeta["lineno"]
            if 0 <= diffline < min_diffline:
                min_diffline = diffline
                closest_entry = entry
    return closest_entry


def remove_account_postings(account: Account, entries: Directives) -> Directives:
    """Remove all postings with the given account.

    Args:
      account: A string, the account name whose postings we want to remove.
    Returns:
      A list of entries without the rounding postings.
    """
    new_entries = []
    for entry in entries:
        if isinstance(entry, Transaction) and (
            any(posting.account == account for posting in entry.postings)
        ):
            entry = entry._replace(
                postings=[
                    posting for posting in entry.postings if posting.account != account
                ]
            )
        new_entries.append(entry)
    return new_entries


def iter_entry_dates(
    entries: Directives, date_begin: datetime.date, date_end: datetime.date
) -> Iterator[Directive]:
    """Iterate over the entries in a date window.

    Args:
      entries: A date-sorted list of dated directives.
      date_begin: A datetime.date instance, the first date to include.
      date_end: A datetime.date instance, one day beyond the last date.
    Yields:
      Instances of the dated directives, between the dates, and in the order in
      which they appear.
    """
    getdate = lambda entry: entry.date
    index_begin = bisect_left_with_key(entries, date_begin, key=getdate)
    index_end = bisect_left_with_key(entries, date_end, key=getdate)
    for index in range(index_begin, index_end):
        yield entries[index]
