"""Merge the metadata from a second file into the current set of entries.

This is useful if you like to keep more sensitive private data, such as account
numbers or passwords, in a second, possibly encrypted file. This can be used to
generate a will, for instance, for your loved ones to be able to figure where
all your assets are in case you pass away. You can store all the super secret
stuff in a more closely guarded, hidden away separate file.

The metadata from

- Open directives: Account name must match.
- Close directives: Account name must match.
- Commodity directives: Currency must match.

are copied over. Metadata from the external file conflicting with that present
in the main file overwrites it (external data wins).

  WARNING! If you include an encrypted file and the main file is not encrypted,
  the contents extraction from the encrypted file may appear in the cache.

"""
__author__ = "Martin Blais <blais@furius.ca>"

from beancount import loader
from beancount.core import getters

__plugins__ = ('merge_meta',)


def merge_meta(entries, unused_options_map, config):
    """Load a secondary file and merge its metadata in our given set of entries.

    Args:
      entries: A list of directives. We're interested only in the Transaction instances.
      unused_options_map: A parser options dict.
      config: The plugin configuration string.
    Returns:
      A list of entries, with more metadata attached to them.
    """
    ext_entries, ext_error, ext_options_map = loader.load_file(config)

    # Map Open and Close directives.
    oc_map = getters.get_account_open_close(entries)
    ext_oc_map = getters.get_account_open_close(ext_entries)

    new_entries = list(entries)

    for account in set(oc_map.keys()) & set(ext_oc_map.keys()):
        print(account)
        open_entry, close_entry = oc_map[account]
        ext_open_entry, ext_close_entry = ext_oc_map[account]

        if open_entry and ext_open_entry:
            print(open_entry)
            print(ext_open_entry)

        if close_entry and ext_close_entry:
            print(close_entry)
            print(ext_close_entry)

    # Note: Include the included file in the list of inputs so that .

    return new_entries, []
