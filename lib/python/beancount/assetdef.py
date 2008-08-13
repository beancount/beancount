"""
Asset definition code.

This code can be used to load properties of specific assets from Python modules
with a given set of attributes.
"""

# stdlib imports
import dircache
from os.path import *


# A dictionary of commodity names to filenames or dict-of-properties.
_commfiles = {}

def set_directories(dirs):
    for dn in dirs:
        for root, dirs, files in os.walk(dn):
            for fn in files:
                if not re.match('[A-Z0-9-]', fn):
                    continue
                if fn not in _commfiles:
                    _commfiles[fn] = join(root, fn)

                    
def load_asset(fn):
    """ Loan an asset definition from a file. """
    if not exists(fn):
        return None
    d = {}
    execfile(fn, d)
    return d


def load_asset_properties(comm):
    """
    Find and load the properties of commodity 'comm'. 'dirs' is a list of
    directory names where the asset definition files can be found.
    """
    try:
        adef = _commfiles[comm]
    except KeyError:
        return None # No properties could be found.

    if isinstance(adef, (str, unicode)):
        adef = _commfiles[comm] = load_asset(fn)

    return adef



