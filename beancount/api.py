"""V3 public API.

The future of Beancount (v3) will bind the majority of public symbols on the
root package, so that one can seamlessly use it like e.g., numpy, with

   import beancount as bn

   bn.load_file(...)

Note: This API may change over time, though we're not expecting to remove any
symbols on the v3 branch.
"""

__copyright__ = "Copyright (C) 2013-2014, 2016-2025  Martin Blais"
__license__ = "GNU GPLv2"


# For split, join, parent, leaf, root
from .core import account
from .core import amount
from .core.account_types import get_account_sign
from .core.account_types import get_account_sort_key
from .core.account_types import get_account_type
from .core.account_types import is_account_type
from .core.account_types import is_balance_sheet_account
from .core.account_types import is_equity_account
from .core.account_types import is_income_statement_account
from .core.account_types import is_inverted_account
from .core.amount import Amount
from .core.convert import convert_amount
from .core.convert import convert_position
from .core.convert import get_cost
from .core.convert import get_units
from .core.convert import get_value
from .core.convert import get_weight
from .core.data import Account
from .core.data import Booking
from .core.data import Currency
from .core.data import Directive
from .core.data import Directives
from .core.data import Flag
from .core.data import Meta
from .core.data import Options
from .core.data import Posting
from .core.data import TxnPosting
from .core.data import dtypes  # Directive types on its own object.  # Not on the top-level.
from .core.data import (
    # TODO(blais): Replace this with
    # bn.dfilter(..., bn.dtypes.Transaction).
    filter_txns,
)
from .core.data import new_metadata
from .core.flags import FLAG_CONVERSIONS
from .core.flags import FLAG_MERGING
from .core.flags import FLAG_OKAY
from .core.flags import FLAG_PADDING
from .core.flags import FLAG_SUMMARIZE
from .core.flags import FLAG_TRANSFER
from .core.flags import FLAG_WARNING
from .core.getters import get_account_open_close
from .core.getters import get_accounts
from .core.inventory import Inventory
from .core.number import ZERO
from .core.number import D
from .core.position import Cost
from .core.position import CostSpec
from .core.position import Position
from .core.prices import PriceMap
from .core.prices import build_price_map
from .core.prices import get_latest_price
from .core.prices import get_price
from .core.realization import RealAccount
from .core.realization import realize

# TODO(blais): We should return a namedtuple of all the contents, not the three
# (entries, options, errors) lists.
from .loader import load_doc
from .loader import load_encrypted_file
from .loader import load_file
from .parser.options import get_account_types
from .parser.printer import format_entry
from .parser.printer import print_entries
from .parser.printer import print_entry

__all__ = [
    "FLAG_CONVERSIONS",
    "FLAG_MERGING",
    "FLAG_OKAY",
    "FLAG_PADDING",
    "FLAG_SUMMARIZE",
    "FLAG_TRANSFER",
    "FLAG_WARNING",
    "ZERO",
    "Account",
    "Amount",
    "Booking",
    "Cost",
    "CostSpec",
    "Currency",
    "D",
    "Directive",
    "Directives",
    "Flag",
    "Inventory",
    "Meta",
    "Options",
    "Position",
    "Posting",
    "PriceMap",
    "RealAccount",
    "TxnPosting",
    "account",
    "amount",
    "build_price_map",
    "convert_amount",
    "convert_position",
    "dtypes",
    "filter_txns",
    "format_entry",
    "get_account_open_close",
    "get_account_sign",
    "get_account_sort_key",
    "get_account_type",
    "get_account_types",
    "get_accounts",
    "get_cost",
    "get_latest_price",
    "get_price",
    "get_units",
    "get_value",
    "get_weight",
    "is_account_type",
    "is_balance_sheet_account",
    "is_equity_account",
    "is_income_statement_account",
    "is_inverted_account",
    "load_doc",
    "load_encrypted_file",
    "load_file",
    "new_metadata",
    "print_entries",
    "print_entry",
    "realize",
]
