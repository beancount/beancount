"""Getter functions that operate on lists of entries to return various lists of
things that they reference, accounts, tags, links, currencies, etc.
"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from collections import OrderedDict
from collections import defaultdict
from typing import TYPE_CHECKING

from beancount.core import account
from beancount.core.data import Close
from beancount.core.data import Commodity
from beancount.core.data import Open
from beancount.core.data import Transaction

if TYPE_CHECKING:
    import datetime
    from typing import Any
    from typing import Iterator

    from beancount.core.data import Account
    from beancount.core.data import Balance
    from beancount.core.data import Currency
    from beancount.core.data import Directive
    from beancount.core.data import Directives
    from beancount.core.data import Document
    from beancount.core.data import Note
    from beancount.core.data import Pad

    AccountsUseMap = tuple[dict[Account, datetime.date], dict[Account, datetime.date]]


class GetAccounts:
    """Accounts gatherer."""

    def get_accounts_use_map(self, entries: Directives) -> AccountsUseMap:
        """Gather the list of accounts from the list of entries.

        Args:
          entries: A list of directive instances.
        Returns:
          A pair of dictionaries of account name to date, one for first date
          used and one for last date used. The keys should be identical.
        """
        accounts_first = {}
        accounts_last = {}
        for entry in entries:
            method = getattr(self, entry.__class__.__name__)
            for account_ in method(entry):
                if account_ not in accounts_first:
                    accounts_first[account_] = entry.date
                accounts_last[account_] = entry.date
        return accounts_first, accounts_last

    def get_entry_accounts(self, entry: Directive) -> set[Account]:
        """Gather all the accounts references by a single directive.

        Note: This should get replaced by a method on each directive eventually,
        that would be the clean way to do this.

        Args:
          entry: A directive instance.
        Returns:
          A set of account name strings.
        """
        method = getattr(self, entry.__class__.__name__)
        return set(method(entry))

    def Transaction(_, entry: Transaction) -> Iterator[Account]:
        """Process a Transaction directive.

        Args:
          entry: An instance of Transaction.
        Yields:
          The accounts of the legs of the transaction.
        """
        for posting in entry.postings:
            yield posting.account

    def Pad(_, entry: Pad) -> tuple[Account, Account]:
        """Process a Pad directive.

        Args:
          entry: An instance of Pad.
        Returns:
          The two accounts of the Pad directive.
        """
        return (entry.account, entry.source_account)

    def _one(_, entry: Open | Close | Balance | Note | Document) -> tuple[Account]:
        """Process directives with a single account attribute.

        Args:
          entry: An instance of a directive.
        Returns:
          The single account of this directive.
        """
        return (entry.account,)

    def _zero(_, entry: Any) -> tuple:
        """Process directives with no accounts.

        Args:
          entry: An instance of a directive.
        Returns:
          An empty list
        """
        return ()

    # Associate all the possible directives with their respective handlers.
    Open = Close = Balance = Note = Document = _one
    Commodity = Event = Query = Price = Custom = _zero


# Global instance to share.
_GetAccounts = GetAccounts()


def get_accounts_use_map(entries: Directives) -> AccountsUseMap:
    """Gather all the accounts references by a list of directives.

    Args:
      entries: A list of directive instances.
    Returns:
      A pair of dictionaries of account name to date, one for first date
      used and one for last date used. The keys should be identical.
    """
    return _GetAccounts.get_accounts_use_map(entries)


def get_accounts(entries: Directives) -> set[str]:
    """Gather all the accounts references by a list of directives.

    Args:
      entries: A list of directive instances.
    Returns:
      A set of account strings.
    """
    _, accounts_last = _GetAccounts.get_accounts_use_map(entries)
    return set(accounts_last.keys())


def get_entry_accounts(entry: Directive) -> set[str]:
    """Gather all the accounts references by a single directive.

    Note: This should get replaced by a method on each directive eventually,
    that would be the clean way to do this.

    Args:
      entries: A directive instance.
    Returns:
      A set of account strings.
    """
    return _GetAccounts.get_entry_accounts(entry)


def get_account_components(entries: Directives) -> list[str]:
    """Gather all the account components available in the given directives.

    Args:
      entries: A list of directive instances.
    Returns:
      A list of strings, the unique account components, including the root
      account names.
    """
    accounts = get_accounts(entries)
    components = set()
    for account_name in accounts:
        components.update(account.split(account_name))
    return sorted(components)


def get_all_tags(entries: Directives) -> list[str]:
    """Return a list of all the tags seen in the given entries.

    Args:
      entries: A list of directive instances.
    Returns:
      A set of tag strings.
    """
    all_tags: set[str] = set()
    for entry in entries:
        if not isinstance(entry, Transaction):
            continue
        if entry.tags:
            all_tags.update(entry.tags)
    return sorted(all_tags)


def get_all_payees(entries: Directives) -> list[str]:
    """Return a list of all the unique payees seen in the given entries.

    Args:
      entries: A list of directive instances.
    Returns:
      A set of payee strings.
    """
    all_payees = set()
    for entry in entries:
        if not isinstance(entry, Transaction):
            continue
        all_payees.add(entry.payee)
    all_payees.discard(None)
    return sorted(all_payees)  # type: ignore[arg-type]


def get_all_links(entries: Directives) -> list[str]:
    """Return a list of all the links seen in the given entries.

    Args:
      entries: A list of directive instances.
    Returns:
      A set of links strings.
    """
    all_links: set[str] = set()
    for entry in entries:
        if not isinstance(entry, Transaction):
            continue
        if entry.links:
            all_links.update(entry.links)
    return sorted(all_links)


def get_leveln_parent_accounts(account_names: list[Account], level: int, nrepeats: int = 0):
    """Return a list of all the unique leaf names at level N in an account hierarchy.

    Args:
      account_names: A list of account names (strings)
      level: The level to cross-cut. 0 is for root accounts.
      nrepeats: A minimum number of times a leaf is required to be present in the
        the list of unique account names in order to be returned by this function.
    Returns:
      A list of leaf node names.
    """
    leveldict: dict[str, int] = defaultdict(int)
    for account_name in set(account_names):
        components = account.split(account_name)
        if level < len(components):
            leveldict[components[level]] += 1
    levels = {level_ for level_, count in leveldict.items() if count > nrepeats}
    return sorted(levels)


def get_dict_accounts(account_names):
    """Return a nested dict of all the unique leaf names.
    account names are labelled with LABEL=True

    Args:
      account_names: An iterable of account names (strings)
    Returns:
      A nested OrderedDict of account leafs
    """
    leveldict = OrderedDict()
    for account_name in account_names:
        nested_dict = leveldict
        for component in account.split(account_name):
            nested_dict = nested_dict.setdefault(component, OrderedDict())
        nested_dict[get_dict_accounts.ACCOUNT_LABEL] = True
    return leveldict


get_dict_accounts.ACCOUNT_LABEL = "__root__"  # type: ignore[attr-defined]


def get_min_max_dates(
    entries: Directives, types: tuple[type[Any], ...] | None = None
) -> tuple[datetime.date | None, datetime.date | None]:
    """Return the minimum and maximum dates in the list of entries.

    Args:
      entries: A list of directive instances.
      types: An optional tuple of types to restrict the entries to.
    Returns:
      A pair of datetime.date dates, the minimum and maximum dates seen in the
      directives.
    """
    date_first = date_last = None

    for entry in entries:
        if types and not isinstance(entry, types):
            continue
        date_first = entry.date
        break

    for entry in reversed(entries):
        if types and not isinstance(entry, types):
            continue
        date_last = entry.date
        break

    return (date_first, date_last)


def get_active_years(entries: Directives) -> Iterator[int]:
    """Yield all the years that have at least one entry in them.

    Args:
      entries: A list of directive instances.
    Yields:
      Unique dates see in the list of directives.
    """
    seen = set()
    prev_year = None
    for entry in entries:
        year = entry.date.year
        if year != prev_year:
            prev_year = year
            assert year not in seen
            seen.add(year)
            yield year


def get_account_open_close(entries):
    """Fetch the open/close entries for each of the accounts.

    If an open or close entry happens to be duplicated, accept the earliest
    entry (chronologically).

    Args:
      entries: A list of directive instances.
    Returns:
      A map of account name strings to pairs of (open-directive, close-directive)
      tuples.
    """
    # A dict of account name to (open-entry, close-entry).
    open_close_map = defaultdict(lambda: [None, None])
    for entry in entries:
        if not isinstance(entry, (Open, Close)):
            continue
        open_close = open_close_map[entry.account]
        index = 0 if isinstance(entry, Open) else 1
        previous_entry = open_close[index]
        if previous_entry is not None:
            if previous_entry.date <= entry.date:
                entry = previous_entry
        open_close[index] = entry

    return dict(open_close_map)


def get_commodity_directives(entries: Directives) -> dict[Currency, Commodity]:
    """Create map of commodity names to Commodity entries.

    Args:
      entries: A list of directive instances.
    Returns:
      A map of commodity name strings to Commodity directives.
    """
    return {entry.currency: entry for entry in entries if isinstance(entry, Commodity)}


def get_values_meta(name_to_entries_map, *meta_keys, default=None):
    """Get a map of the metadata from a map of entries values.

    Given a dict of some key to a directive instance (or None), return a mapping
    of the key to the metadata extracted from each directive, or a default
    value. This can be used to gather a particular piece of metadata from an
    accounts map or a commodities map.

    Args:
      name_to_entries_map: A dict of something to an entry or None.
      meta_keys: A list of strings, the keys to fetch from the metadata.
      default: The default value to use if the metadata is not available or if
        the value/entry is None.
    Returns:
      A mapping of the keys of name_to_entries_map to the values of the 'meta_keys'
      metadata. If there are multiple 'meta_keys', each value is a tuple of them.
      On the other hand, if there is only a single one, the value itself is returned.
    """
    value_map = {}
    for key, entry in name_to_entries_map.items():
        value_list = []
        for meta_key in meta_keys:
            value_list.append(
                entry.meta.get(meta_key, default) if entry is not None else default
            )
        value_map[key] = value_list[0] if len(meta_keys) == 1 else tuple(value_list)
    return value_map
