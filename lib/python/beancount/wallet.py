"""
Wallet arithmetic.
"""

# stdlib imports
from decimal import Decimal

__all__ = ('Wallet',)



class Wallet(dict):
    """
    A mapping of currency to amount. The basic operators are suppored.
    """

    # A dict of commodity -> Decimal to determine the default precision for
    # rounding. The 'None' key is used as a default precision value.
    roundmap = {None: Decimal('0.01')}

    def __init__(self, *args, **kwds):
        if len(args) == 2:
            # Initialize from a tuple ('COM', num).
            dict.__init__(self, (args,), **kwds)
        elif len(args) == 1 and isinstance(args[0], str):
            # Initialize from a string "num COM".
            num, com = args[0].split()
            dict.__init__(self, ((com, num),), **kwds)
        else:
            # Initialize like a normal dictionary.
            dict.__init__(self, *args, **kwds)

        # Convert the initialized contents to Decimal objects.
        for k, value in self.iteritems():
            assert k is not None
            if not isinstance(value, Decimal):
                self[k] = Decimal(value)

        _clean(self)

    def copy(self):
        return Wallet(self)

    def only(self, comm):
        "Return a wallet like this one but with only the given commodity."
        try:
            return Wallet(comm, self[comm])
        except KeyError:
            return Wallet()

    def mask(self, other):
        "Return this wallet with only the commodities in the other wallet."
        return Wallet(
            (com, amt) for com, amt in self.iteritems() if com in other)

    def __str__(self):
        sitems = sorted(self.iteritems(), key=self.commodity_key)
        return ', '.join('%s %s' % (v, k) for k, v in sitems)

    def __repr__(self):
        return 'Wallet(%s)' % dict.__repr__(self)

    def tostrlist(self):
        """Return a list of pairs of strings (amount, commodity) to be
        rendered)."""
        return sorted(self.iteritems(), key=self.commodity_key)

    def __setitem__(self, key, value):
        if not isinstance(value, Decimal):
            value = Decimal(value)
        dict.__setitem__(self, key, value)

    def isempty(self):
        return not any(self.itervalues())
        
    def __nonzero__(self):
        return any(self.itervalues())

    def __neg__(self):
        return Wallet((k, -v) for k, v in self.iteritems())

    def __add__(self, other):
        if other is None:
            return Wallet(self)
        w = Wallet()
        for k, v1 in self.iteritems():
            if k in other:
                w[k] = v1 + other[k]
            else:
                w[k] = v1
        for k, v2 in other.iteritems():
            if k not in self:
                w[k] = v2
        _clean(w)
        return w

    def __iadd__(self, other):
        if other is None:
            return self
        w = self
        for k, v1 in self.iteritems():
            if k in other:
                w[k] = v1 + other[k]
            else:
                w[k] = v1
        for k, v2 in other.iteritems():
            if k not in self:
                w[k] = v2
        _clean(w)
        return w

    def __sub__(self, other):
        if other is None:
            return Wallet(self)
        w = Wallet()
        for k, v1 in self.iteritems():
            if k in other:
                w[k] = v1 - other[k]
            else:
                w[k] = v1
        for k, v2 in other.iteritems():
            if k not in self:
                w[k] = -v2
        _clean(w)
        return w

    def __isub__(self, other):
        if other is None:
            return self
        w = self
        for k, v1 in self.iteritems():
            if k in other:
                w[k] = v1 - other[k]
            else:
                w[k] = v1
        for k, v2 in other.iteritems():
            if k not in self:
                w[k] = -v2
        _clean(w)
        return w

    def __mul__(self, other):
        assert isinstance(other, int)
        w = Wallet(self)
        for k, v in self.iteritems():
            w[k] *= other
        _clean(w)
        return w

    def __div__(self, other):
        assert isinstance(other, int)
        w = Wallet(self)
        for k, v in self.iteritems():
            w[k] /= other
        _clean(w)
        return w

    def round(self, mprecision=None):
        """
        Given a map of commodity to Decimal objects with a specific precision,
        return a rounded version of this wallet. (The default precision is
        provided by a key of None in the mprecision dict.)
        """
        if mprecision is None:
            mprecision = self.roundmap
        assert isinstance(mprecision, dict)
        w = Wallet()
        for com, amt in self.iteritems():
            try:
                prec = mprecision[com]
            except KeyError:
                prec = mprecision[None]
            w[com] = amt.quantize(prec)
        _clean(w)
        return w

    @staticmethod
    def commodity_key(kv):
        """ A sort key for the commodities."""
        k = kv[0]
        return (len(k), k)



def _clean(w):
    "Remove zero'ed components of the wallet."
    rlist = [k for k, v in w.iteritems() if not v]
    for k in rlist:
        del w[k]

