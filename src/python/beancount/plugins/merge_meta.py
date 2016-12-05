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
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount import loader
from beancount.core import getters

__plugins__ = ('merge_meta',)


def merge_meta(entries, options_map, config):
    """Load a secondary file and merge its metadata in our given set of entries.

    Args:
      entries: A list of directives. We're interested only in the Transaction instances.
      unused_options_map: A parser options dict.
      config: The plugin configuration string.
    Returns:
      A list of entries, with more metadata attached to them.
    """
    external_filename = config
    new_entries = list(entries)

    ext_entries, ext_errors, ext_options_map = loader.load_file(external_filename)

    # Map Open and Close directives.
    oc_map = getters.get_account_open_close(entries)
    ext_oc_map = getters.get_account_open_close(ext_entries)
    for account in set(oc_map.keys()) & set(ext_oc_map.keys()):
        open_entry, close_entry = oc_map[account]
        ext_open_entry, ext_close_entry = ext_oc_map[account]
        if open_entry and ext_open_entry:
            open_entry.meta.update(ext_open_entry.meta)
        if close_entry and ext_close_entry:
            close_entry.meta.update(ext_close_entry.meta)

    # Map Commodity directives.
    comm_map = getters.get_commodity_map(entries, False)
    ext_comm_map = getters.get_commodity_map(ext_entries, False)
    for currency in set(comm_map) & set(ext_comm_map):
        comm_entry = comm_map[currency]
        ext_comm_entry = ext_comm_map[currency]
        if comm_entry and ext_comm_entry:
            comm_entry.meta.update(ext_comm_entry.meta)

    # Note: We cannot include the external file in the list of inputs so that a
    # change of it triggers a cache rebuild because side-effects on options_map
    # aren't cascaded through. This is something that should be defined better
    # in the plugin interface and perhaps improved upon.

    return new_entries, ext_errors
