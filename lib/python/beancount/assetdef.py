"""
Asset definition code.

This code can be used to load properties of specific assets from Python modules
with a given set of attributes.
"""

# stdlib imports
import os, re, dircache
from os.path import *


# A dictionary of commodity names to filenames or dict-of-properties.
_commfiles = {}

def add_asset_path(dn):
    for root, dirs, files in os.walk(dn):
        for fn in files:
            if not re.match('[A-Z0-9-.]+$', fn):
                continue
            if fn not in _commfiles:
                _commfiles[fn] = join(root, fn)
                    
def load_asset(fn):
    """ Loan a single asset definition from a file, in Python language."""
    if not exists(fn):
        return None
    d = {}
    execfile(fn, d)
## FIXME: we could insure that some set of attributes is defined.
    return d

def get_asset(comm):
    """
    Find and load the properties of commodity 'comm'. Loading is done lazily,
    since there may be many more definitions than are necessary in typical
    usage.
    """
    try:
        adef = _commfiles[comm]
    except KeyError:
        return None # No properties could be found.

    if isinstance(adef, (str, unicode)):
        adef = _commfiles[comm] = load_asset(fn)

    return adef



