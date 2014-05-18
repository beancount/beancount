"""Account object.

These account objects are rather simple and dumb; they do not contain the list
of their associated postings. This is achieved by building a realization; see
realization.py for details.
"""


# Component separator for account names.
sep = ':'


def join(*components):
    """Join the names with the account separator.

    Args:
      *components: Strings, the components of an account name.
    Returns:
      A string, joined in a single account name.
    """
    return sep.join(components)


# FIXME: convert to just parent().
def account_name_parent(account_name):
    """Return the name of the parent account of the given account.

    Args:
      account_name: A string, the name of the account whose parent to return.
    Returns:
      A string, the name of the parent account of this account.
    """
    assert isinstance(account_name, str), account_name
    if not account_name:
        return None
    components = account_name.split(sep)
    components.pop(-1)
    return sep.join(components)


# FIXME: convert to just leaf().
def account_name_leaf(account_name):
    """Get the name of the leaf of this account.

    Args:
      account_name: A string, the name of the account whose leaf name to return.
    Returns:
      A string, the name of the leaf of the account.
    """
    assert isinstance(account_name, str)
    return account_name.split(sep)[-1] if account_name else None
