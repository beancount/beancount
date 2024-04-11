"""V3 public API.

!WARNING! THIS IS EXPERIMENTAL.

The future of Beancount (v3) will bind the majority of public symbols on the
root package, so that one can seamlessly use it like e.g., numpy, with

   import beancount as bn

   bn.load_file(...)

WARNING: This is a prototype API. This is subject to change until the first v3
release. Principle: Not all symbols are intended to be present here, only the
most commonly used ones.
"""

# pylint: disable=unused-import,unused-wildcard-import

from .core.number import D

from .core.flags import *  # pylint: disable=wildcard-import

# TODO(blais): We should return a namedtuple of all the contents, not the lists.
from .loader import load_file, load_encrypted_file, load_doc

from .core.data import (
    Account,
    Currency,
    Flag,
    Meta,
    Booking,
    Posting,
    Directive,
    Directives,
    Options,
    new_metadata,
    # TODO(blais): Replace this with
    # bn.dfilter(..., bn.dtypes.Transaction).
    filter_txns,
    # Directive types on its own object.
    # Not on the top-level.
    dtypes,
)

# For split, join, parent, leaf, root
from .core import account

from .core.getters import get_accounts, get_account_open_close

from .core.account_types import (
    get_account_type,
    is_account_type,
    is_balance_sheet_account,
    is_income_statement_account,
    is_equity_account,
    is_inverted_account,
    get_account_sign,
)

from .core.amount import (
    Amount,
)

from .core.position import Position, Cost, CostSpec

from .core.inventory import (
    Inventory,
)

from .core.convert import (
    get_units,
    get_cost,
    get_weight,
    get_value,
    convert_position,
    convert_amount,
)

from .core.realization import RealAccount, realize

from .core.prices import PriceMap, build_price_map, get_price, get_latest_price

from .parser.options import get_account_types

from .parser.printer import print_entry
from .parser.printer import print_entries
from .parser.printer import format_entry
