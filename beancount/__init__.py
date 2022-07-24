"""Beancount, a double-entry bookkeeping software.
"""
__copyright__ = "Copyright (C) 2013-2014, 2016-2018,2021  Martin Blais"
__license__ = "GNU GPLv2"


# Check the version requirements.
import sys
if (sys.version_info.major, sys.version_info.minor) < (3, 3):
    raise ImportError("Python 3.3 or above is required")


# Read in the VERSION number from package data.
from os import path
with open(path.join(path.dirname(__file__), "VERSION")) as version_file:
    __version__ = version_file.read().strip()


# V3 public API.
#
# !WARNING! THIS IS EXPERIMENTAL.
#
# The future of Beancount (v3) will bind the majority of public symbols on the
# root package, so that one can seamlessly use it like e.g., numpy, with
#
#    import beancount as bn
#
#    bn.load_file(...)
#
# WARNING: This is a prototype API. This is subject to change until the first v3
# release. Principle: Not all symbols are intended to be present here, only the
# most commonly used ones.

# TODO(blais): Move this to an api.py module and import that.

from .core.number import D

from .core.flags import *

from .loader import (load_file,
                     load_encrypted_file,
                     load_doc)

from .core.data import (Account,
                        Currency,
                        Flag,
                        Meta,
                        Booking,
                        Directives,
                        Options,
                        new_metadata,
                        # TODO(blais): Replace this with
                        # bn.dfilter(..., bn.dtypes.Transaction).
                        filter_txns,

                        # Directive types on its own object.
                        # Not on the top-level.
                        dtypes)

# For split, join, parent, leaf, root
from .core import account

from .core.getters import (get_accounts)

from .core.account_types import (get_account_type,
                                 is_account_type,
                                 is_balance_sheet_account,
                                 is_income_statement_account,
                                 is_equity_account,
                                 is_inverted_account,
                                 get_account_sign)

from .core.amount import (Amount,)

from .core.position import (Position,
                            Cost,
                            CostSpec)

from .core.inventory import (Inventory,)

from .core.convert import (get_units,
                           get_cost,
                           get_weight,
                           get_value,
                           convert_position,
                           convert_amount)

from .core.realization import (RealAccount,
                               realize)

from .core.prices import (PriceMap,
                          build_price_map,
                          get_price,
                          get_latest_price)


# Import v3 extension module.
try:
    from beancount.cparser import extmodule
    parse = extmodule.parse
except ImportError:
    warnings.warn("Unable to import v3 extension module.")
