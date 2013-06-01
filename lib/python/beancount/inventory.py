"""
Trade inventory.

This module provides a very flexible inventory object, used to maintain position
and to calculate P+L for a positions, for a single product. In order to compute
P+L, we need to book current trades with past trades, and our inventory object
supports various booking methods to select which positions get booked (FIFO,
LIFO, custom), and a few ways to price the booked positions (real, average
cost). It can also support manual booking of specific trades, in case an
external booking algorithm already takes place.

Note that to avoid precision errors, all prices are expressed as integers scaled
to 10^8.

(We have intended to make this object as flexible as possible, because this is a
generic problem that tends to be badly solved in the financial industry with
error-prone and confusing calculations, and can easily be solved once and for
all.)
"""

# stdlib imports
from collections import deque
from decimal import Decimal

__all__ = ('Inventory', 'FIFOInventory', 'LIFOInventory', 'AvgInventory',
           'BOOKING_FIFO', 'BOOKING_LIFO', 'BOOKING_NONE',
           'PRICING_REAL', 'PRICING_AVERAGE')



class Position(object):
    """ A position that we're holding. Its size represents the remaining size,
    and not the original trade size."""

    def __init__(self, inv, price, size, obj=None):

        # The inventory that this position is attached to. (Note that this is
        # only necessary to access the integer representation converters.)
        self.inv = inv

        # The original price paid for the position.
        self.price = price

        # The current/remaining size of that position.
        self.size = size

        # An trade object that represents a psition (it can be any type of your
        # choosing). These objects are returned when matched and/or removed.
        self.obj = obj

    def __str__(self):
        S = self.inv.S
        if self.obj is not None:
            return '%s @ %s : %s' % (self.size, S(self.price), self.obj)
        else:
            return '%s @ %s' % (self.size, S(self.price))

    def cost(self):
        return self.price * self.size


# Booking methods.
def BOOKING_FIFO(inv):
    "Return the next trade for FIFO booking."
    return inv.positions[0]

def BOOKING_LIFO(inv):
    "Return the next trade for LIFO booking."
    return inv.positions[-1]

def BOOKING_NONE(inv):
    "Prevent from using automatic booking."
    raise IndexError("Automatic booking is disabled. "
                     "You need to close your trades manually.")


# Pricing methods.
PRICING_REAL = object()
PRICING_AVERAGE = object()


class Inventory(object):
    "An interface for inventory objects."

    def __init__(self, booking=BOOKING_NONE, pricing=PRICING_REAL):
        # The booking method, a function object that is supposed to return the
        # next position to be matched.
        self.booking_findnext = booking

        # The pricing method.
        self.pricing_method = pricing

        # Functions that convert into integer, float and string representation.
        if not hasattr(self, 'L'):
            self.L = lambda x: Decimal(str(x))
        if not hasattr(self, 'F'):
            self.F = float
        if not hasattr(self, 'S'):
            self.S = str

        self.reset()

    def reset(self):
        "Reset the realized pnl and position (initial state)."

        # The last marked price for the underlying product.
        self.mark = None

        # The realized P+L for this inventory.
        self._realized_pnl = self.L(0)

        # The price of the last trade.
        self.last_trade_price = None

        # For precision reasons, we track the cost of our positions separately
        # (we could otherwise adjust each of them to the average every time it
        # changes, introducing numerical error, but simplifying the code). This
        # is only used for average cost pricing.
        self.cost4avg = self.L(0)

        # A deque of Position objects for our active position.
        self.positions = deque()

    def reset_position(self):
        """
        Reset the current position to 0. After calling this method, the P+L is
        only the unrealized P+L. We return the list of trade objects for the
        eliminated positions.
        """
        eliminated = [pos.obj for pos in self.positions]
        self.positions = deque()
        return eliminated

    def consolidate(self, price):
        """
        Book all of the current inventory's position at the given price
        (typically, some sort of settlement price) and set the position cost at
        that price. This method does not affect the position, but it transfers
        unrealized P+L into realized P+L. This means that if the mark price is
        equal to the consolidation price, the unrealized P+L is 0 after this
        method is called if the consolidation price is equal to the mark price.
        """
        # We extract the PnL from each position by changing its trade price.
        realized_pnl = 0
        for pos in self.positions:
            realized_pnl += (price - pos.price) * pos.size
            pos.price = price
        self._realized_pnl += realized_pnl

        if self.pricing_method is PRICING_AVERAGE:
            self.cost4avg += realized_pnl
            assert self.cost4avg == self.realcost()

        # Another way to accomplish this would be to perform two trades, but
        # that would delete the information about the trade objects. We used to
        # do it like this:
        ## pos = self.position()
        ## self.trade(price, -pos)
        ## assert self.position() == 0
        ## self.trade(price, pos)

    def reset_pnl(self):
        """
        Reset the realized P+L to 0 and returns it. This method effectively
        transfers out the realized P+L from this inventory.
        """
        rpnl, self._realized_pnl = self._realized_pnl, 0
        return rpnl

    def setmark(self, price):
        "Set the mark price for the current product."
        self.mark = price

    def setmarkexit(self, bid, ask):
        "Set the mark price at the price to exit."
        self.mark = bid if self.position() > 0 else ask

    def _sanity_check(self):
        "Perform some internal sanity checks."

        # Check that the signs of our inventory are all the same.
        if self.positions:
            size = self.positions[0].size
            for pos in self.positions:
                assert pos.size * size > 0, pos

    def position(self):
        "Return the current position this inventory is holding."
        self._sanity_check()
        return sum(pos.size for pos in self.positions) if self.positions else self.L(0)

    def realcost(self):
        "Return the real cost of our current positions."
        return sum(pos.cost() for pos in self.positions) if self.positions else self.L(0)

    def cost(self):
        "Return the original cost of our active position."
        if self.pricing_method is PRICING_REAL:
            return self.realcost()
        else:
            return self.cost4avg

    def value(self):
        "Return the marked value of the entire position."
        pos = self.position()
        if pos != 0:
            if self.mark is None:
                raise ValueError("You need to set the mark to obtain the pnl")
            return pos * self.mark
        else:
            return self.L(0)

    def avgcost(self):
        "Return the average price paid for each unit of the current position."
        pos = self.position()
        return self.L(self.F(self.cost()) / pos) if pos != 0 else self.L(0)

    def realized_pnl(self):
        return self._realized_pnl

    def unrealized_pnl(self):
        "Return the P+L for our current position (not including past realized P+L."
        pos = self.position()
        if pos == 0:
            return self.L(0)
        if self.mark is None:
            raise ValueError("You need to set the mark to obtain the pnl")
        return self.position() * self.mark - self.cost()

    def pnl(self):
        "Return the P+L for our current position (not including past realized P+L."
        return self._realized_pnl + self.unrealized_pnl()

    def dump(self):
        print ',---------------', self
        print '| position       ', self.position()
        print '| mark           ', self.S(self.mark) if self.mark else None
        print '| avgcost        ', self.S(self.avgcost() or 0)
        print '| value          ', self.S(self.value()) if self.mark else None
        print '| cost           ', self.S(self.cost())
        print '| cost4avg       ', self.S(self.cost4avg)
        print '| unrealized_pnl ', self.S(self.unrealized_pnl()) if self.mark else None
        print '| realized_pnl   ', self.S(self.realized_pnl())
        print '| pnl            ', self.S(self.pnl()) if self.mark else None
        print '| inventory:     '
        for pos in self.positions:
            print '|   %s' % pos
        print '`---------------', self

    def close_all(self, price):
        "Close all the positions at the mark price."
        self.trade(price, -self.position())
        assert self.position() == 0

    def _findpos(self, obj):
        "Return the position that corresponds to a specific trade object."
        for pos in self.positions:
            if pos.obj is obj:
                return pos
        else:
            return None

    def close(self, obj, price, quant=None):
        """ Close the position for the trade 'obj' at the given 'price'. If
        'quant' is specified, close the position only partially (otherwise close
        the entire position). Note that the outcome of using this method does
        not depend on the selected booking method."""
        pos = self._findpos(obj)
        if pos is None:
            raise KeyError("Invalid trade object, could not be found: %s" % obj)
        if quant is not None:
            if quant * pos.size <= 0:
                raise KeyError("Invalid close size %s of %s." % (quant, pos.size))
            if abs(quant) > abs(pos.size):
                raise KeyError("Trying to close %s of %s." % (quant, pos.size))
        else:
            quant = -pos.size
        return self._trade(price, quant, None, lambda inv: pos)

    def trade(self, price, quant, obj=None):
        """ Book a trade for size 'quant' at the given 'price', using the
        default booking method. Return list of trade objects booked and
        the PnL realized by this trade (if any).
        Note: if you want to book positions manually, use the close() method."""

        return self._trade(price, quant, obj, self.booking_findnext)

    def _trade(self, price, quant, obj, nextpos):
        """ Trade booking implementation. We book trades at price 'price' for
        the given size 'quant' only. 'obj' is the trade object to this trade,
        and is inserted in the new Position object if there is remaining size.
        'nextpos' is a function that will return the next Position object to be
        booked against (this is the booking method)."""

        ## trace('__________________ _trade', price, quant, obj, booking)

        # A list of (trade-object, quantity) booked.
        booked = []

        # Total size booked during this trade.
        total_booked = 0

        # "Real" PnL for the booked trades.
        real_pnl = 0

        # Book the new trade against existing positions if the trade is not on
        # the same side as our current position.
        position = self.position()
        if quant * position < 0:

            # Process all the positions.
            done = 0
            while self.positions:
                pos = nextpos(self)
                if abs(quant) >= abs(pos.size):
                    # This position is entirely consumed by the trade.
                    booked_quant = pos.size
                    self.positions.remove(pos) # This may become slow.
                else:
                    # This position is only partially consumed by the trade.
                    booked_quant = -quant
                    pos.size += quant
                    done = 1

                quant += booked_quant
                total_booked += booked_quant
                booked.append( (pos.obj, booked_quant) )

                real_pnl += booked_quant * (price - pos.price)
                if done or quant == 0:
                    break

            assert quant * self.position() >= 0

        # Price the booked trades into the realized PnL, depending on method.
        if self.pricing_method is PRICING_REAL:
            realized_pnl = real_pnl
        else:
            if position == 0:
                assert total_booked == 0, total_booked
                realized_pnl = 0
            else:
                realized_cost = self.L((total_booked*self.F(self.cost4avg))/position)
                realized_pnl = total_booked * price - realized_cost
                self.cost4avg -= realized_cost
        self._realized_pnl += realized_pnl
        if total_booked == 0:
            assert realized_pnl == 0, realized_pnl
        else:
            booked.append( (obj, -total_booked) )

        # Append the remainder of our trade to the inventory if not all was
        # booked.
        if quant != 0:
            newpos = Position(self, price, quant, obj)
            self.positions.append(newpos)
            if self.pricing_method is PRICING_AVERAGE:
                self.cost4avg += newpos.cost()

        self.last_trade_price = price
        return booked, realized_pnl



class FIFOInventory(Inventory):
    def __init__(self):
        Inventory.__init__(self, booking=BOOKING_FIFO, pricing=PRICING_REAL)

class LIFOInventory(Inventory):
    def __init__(self):
        Inventory.__init__(self, booking=BOOKING_LIFO, pricing=PRICING_REAL)

class AvgInventory(Inventory):
    def __init__(self):
        Inventory.__init__(self, booking=BOOKING_FIFO, pricing=PRICING_AVERAGE)
        # Note: the booking method matters little here, other than for the
        # ordering of the trades that get closed.
