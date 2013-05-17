"""
Realization of specific lists of account postings into reports.
"""


class AccountTree:
    """A container for a hierarchy of accounts, that can conveniently
    create and maintain a hierarchy of accounts."""

    def __init__(self):
        # The root note of all accounts.
        self.root = Account('', None, [])

        # A mapping of (name, Account).
        self.accounts_map = {'': self.root}

    def dump(self, out_file):
        string = render_tree.render(self.root,
                                    lambda x: account_leaf_name(x.name),
                                    lambda x: x.children)
        print(string, file=out_file)

    def get(self, name):
        return self.accounts_map[name]

    def get_names(self):
        return sorted(self.accounts_map)

    def get_or_create(self, name):
        """Get or create the account with the given name."""
        try:
            account = self.accounts_map[name]
        except KeyError:
            parent = self.get_or_create(account_parent_name(name))
            account = Account(name)
            parent.children.append(account)
            self.accounts_map[name] = account
        return account


class Ledger:
    """A class that contains a particular list of entries and an
    associated account tree. Note: the account tree is redundant and
    could be recalculated from the list of entries."""

    def __init__(self, entries):

        # A list of sorted entries in this ledger.
        assert isinstance(entries, list)
        entries.sort(key=lambda x: x.date)
        self.entries = entries

        # A tree of accounts.
        assert isinstance(accounts, AccountTree)
        self.accounts = accounts
