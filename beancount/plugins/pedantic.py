"""A plugin of plugins which triggers all the pedantic plugins."""

__copyright__ = "Copyright (C) 2017, 2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from beancount import loader
from beancount.plugins import check_commodity
from beancount.plugins import check_drained
from beancount.plugins import coherent_cost
from beancount.plugins import leafonly
from beancount.plugins import noduplicates
from beancount.plugins import nounused
from beancount.plugins import onecommodity
from beancount.plugins import sellgains
from beancount.plugins import unique_prices

__plugins__ = loader.combine_plugins(
    check_commodity,
    coherent_cost,
    leafonly,
    noduplicates,
    nounused,
    onecommodity,
    sellgains,
    unique_prices,
    check_drained,
)
