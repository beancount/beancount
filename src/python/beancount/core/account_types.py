"""Definition for global account types.

This is where we keep the global account types value and definition.

Note that it's unfortunate that we're using glboals and side-effect here, but
this is the best solution in the short-term, the account types are used
in too many places to pass around that state everywhere. Maybe we change
this later on.
"""
from collections import namedtuple


# A tuple that contains the names of the root accounts. This is a subset of options.
# Attributes:
#   assets: a str, the name of the prefix for the Asset subaccounts.
#   liabilities: a str, the name of the prefix for the Liabilities subaccounts.
#   equity: a str, the name of the prefix for the Equity subaccounts.
#   income: a str, the name of the prefix for the Income subaccounts.
#   expenses: a str, the name of the prefix for the Expenses subaccounts.
AccountTypes = namedtuple('AccountTypes', "assets liabilities equity income expenses")

# The global value of account types.
ACCOUNT_TYPES = None
TYPES_ORDER = None

# Default values for root accounts.
DEFAULT_ACCOUNT_TYPES = AccountTypes("Assets",
                                     "Liabilities",
                                     "Equity",
                                     "Income",
                                     "Expenses")


def update_valid_account_names(account_types=DEFAULT_ACCOUNT_TYPES):
    """Set the globals for the root account names. Calling this function without a
    parameter initializes the default names for the five root accounts for
    assets, liabilities, equity, income and expenses. This is used betwee
    parsing runs.

    Args:
      account_types: None to reset to default, or an instance of AccountTypes to
        set to specific values.
    """
    assert isinstance(account_types, (AccountTypes, type(None)))
    global ACCOUNT_TYPES, TYPES_ORDER
    ACCOUNT_TYPES = account_types
    TYPES_ORDER = dict((x,i) for (i,x) in enumerate(account_types))


update_valid_account_names(DEFAULT_ACCOUNT_TYPES)
