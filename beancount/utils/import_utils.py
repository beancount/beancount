"""Utilities for importing symbols programmatically.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import importlib


def import_symbol(dotted_name):
    """Import a symbol in an arbitrary module.

    Args:
      dotted_name: A dotted path to a symbol.
    Returns:
      The object referenced by the given name.
    Raises:
      ImportError: If the module not not be imported.
      AttributeError: If the symbol could not be found in the module.
    """
    comps = dotted_name.split('.')
    module_name = '.'.join(comps[:-1])
    symbol_name = comps[-1]
    module = importlib.import_module(module_name)
    return getattr(module, symbol_name)
