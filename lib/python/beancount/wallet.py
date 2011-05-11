"""
Wallet arithmetic.
"""

# stdlib imports
import logging
from decimal import Decimal

# beancount imports
from beancount.utils import TimerUtil

__all__ = ('Wallet',)


class _Wallet(dict):
    """
    A mapping of currency to amount. The basic operators are supported.
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
            init = []
            for am in args[0].split(','):
                try:
                    num, com = am.strip().split()
                    init.append((com, num))
                except ValueError:
                    raise ValueError(
                        "Invalid string for initializing a Wallet: %s" % args[0])
            dict.__init__(self, init, **kwds)
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

    def mask_wallet(self, other):
        "Return this wallet with only the commodities in the other wallet."
        return Wallet(
            (com, amt) for com, amt in self.iteritems() if com in other)

    def mask_commodity(self, com):
        "Return this wallet with only the given commodity."
        w = Wallet()
        num = self.get(com, None)
        if num is not None:
            w[com] = num
        return w

    def __str__(self):
        sitems = sorted(self.iteritems(), key=self.commodity_key)
        return ', '.join('%s %s' % (v, k) for k, v in sitems)

    def __repr__(self):
        return 'Wallet(%s)' % dict.__repr__(self)

    def tostrlist(self):
        """Return a list of pairs of strings (commodity, amount) to be
        rendered)."""
        return sorted(self.iteritems(), key=self.commodity_key)

    def tonum(self):
        """Assuming that the wallet contains a single commodity, return the
        amount for that commodity. If the Wallet is empty, return 0."""
        if len(self) == 0:
            d = Decimal()
        elif len(self) == 1:
            d = self.itervalues().next()
        else:
            raise ValueError("Cannot convert wallet %s to a single number." % self)
        return d

    def tocomm(self):
        """Assuming that the wallet contains a single commodity, return the
        amount for that commodity. Fail if the Wallet is empty."""
        if len(self) == 1:
            return self.iterkeys().next()
        else:
            raise ValueError("Cannot convert wallet %s to a single number." % self)

    def single(self):
        """Return a tuple of (amount, commodity) if this wallet contains a
        single thing. If empty or if it contains multiple things, blow up."""
        assert len(self) == 1, "Wallet contains more than one thing."
        c, a = self.iteritems().next()
        return (a, c)

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
        assert isinstance(other, (int, Decimal)), repr(other)
        w = Wallet(self)
        for k, v in self.iteritems():
            w[k] *= other
        _clean(w)
        return w

    def __div__(self, other):
        if isinstance(other, (int, Decimal)):
            w = Wallet(self)
            for k, v in self.iteritems():
                w[k] /= other
            _clean(w)
        elif isinstance(other, Wallet):
            assert False # FIXME: Implement computing ratios.
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
        return (comm_importance.get(k, len(k)), k)

    def price(self, comm, ucomm, price):
        """ Replace all the units of 'comm' by units of 'ucomm' at the given
        price. """
        try:
            units = self[comm]
        except KeyError:
            return
        wdiff = Wallet()
        wdiff[comm] = -units
        wdiff[ucomm] = units * price
        self += wdiff

    def split(self):
        """ Split this wallet into two, one with all the positive unit values
        and one with all the negative unit values. This function returns two
        wallets which, summed together, should equal this wallet."""
        wpos, wneg = Wallet(), Wallet()
        zero = Decimal('0')
        for k, value in self.iteritems():
            w = wpos if value > zero else wneg
            w[k] = value
        return wpos, wneg

    def convert(self, conversions):
        """Given a list of (from-asset, to-asset, rate), convert the from-assets
        to to-assets using the specified rate and return a new Wallet with the
        new amounts."""
        w = self.copy()
        if conversions is None:
            return w
        assert isinstance(conversions, list)
        for from_asset, to_asset, rate in conversions:
            if from_asset in w:
                if to_asset not in w:
                    w[to_asset] = Decimal()
                w[to_asset] += w[from_asset] * rate
                del w[from_asset]
        return w

    def nbthings(self):
        """ Return a single number, the total number of things that are stored
        in this wallet. (This is used for fiddling, as a really gross and
        inaccurate approximation of total amount.)"""
        return sum(self.itervalues())





# Order of important for commodities.
comm_importance = {
    'USD': 0,
    'CAD': 1,
    'EUR': 2,
    }



def _clean(w):
    "Remove zero'ed components of the wallet."
    rlist = [k for k, v in w.iteritems() if not v]
    for k in rlist:
        del w[k]



try:
    from beancount.cwallet import Wallet
except ImportError:
    logging.warning("Fast Wallet object not available; falling back on Python Wallet object.")
    Wallet = _Wallet
    
    
