"""A plugin of plugins which triggers are all the pedantic plugins.

In a sense, this is the inverse of "pedantic." This is useful when doing some
types of quick and dirty tests.
"""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.plugins import check_commodity
from beancount.plugins import coherent_cost
from beancount.plugins import leafonly
from beancount.plugins import noduplicates
from beancount.plugins import nounused
from beancount.plugins import onecommodity
from beancount.plugins import sellgains
from beancount.plugins import unique_prices
from beancount import loader

__plugins__ = loader.combine_plugins(
    check_commodity,
    coherent_cost,
    leafonly,
    noduplicates,
    nounused,
    onecommodity,
    sellgains,
    unique_prices)
