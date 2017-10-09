"""A plugin of plugins which triggers are all the automatic and lax plugins.

In a sense, this is the inverse of "pedantic." This is useful when doing some
types of quick and dirty tests. You can just import the "auto" plugin. Put that
in a macro.

Also see: the 'pedantic' plugin.
"""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.plugins import auto_accounts
from beancount.plugins import implicit_prices
from beancount import loader

__plugins__ = loader.combine_plugins(
    auto_accounts,
    implicit_prices)
