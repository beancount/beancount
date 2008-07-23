"""
Inventory algorithms.
"""

# stdlib imports
from collections import deque
from decimal import Decimal

__all__ = ('FIFOInventory', 'LIFOInventory', 'AvgInventory')


class Inventory(object):
    "An interface for inventory objects."

    def __init__(self):
        self.reset()

    def reset(self):
        "Reset the realized pnl and position (initial state)."

        # The last marked price for the underlying product.
        self.mark = None

        # The realized P+L for this inventory.
        self._realized_pnl = Decimal()

        # Extra objects and history of realized P+L.
        self.realized_extras = []

        # The price of the last trade.
        self.last_trade_price = None

        self.reset_position()

    def reset_position(self):
        """
        Reset the current position to 0. After calling this method, the P+L is
        only the unrealized P+L.
        """
        raise NotImplementedError
    
    def consolidate(self, price):
        """
        Book all of the current inventory's position at the given price
        (typically, some sort of settlement price) and set the position cost at
        that price. This method does not affect the position, but it transfers
        unrealized P+L into realized P+L. This means that if the mark price is
        equal to the consolidation price, the unrealized P+L is 0 after this
        method is called.
        """
        pos = self.position()
        self.trade(price, -pos)
        assert self.position() == 0
        self.trade(price, pos)

    def reset_pnl(self):
        """
        Reset the realized P+L to 0 and returns it. This method effectively
        transfers out the realized P+L from this inventory.
        """
        rpnl, self._realized_pnl = self._realized_pnl, 0
        self.realized_extras = []
        return rpnl

    def setmark(self, price):
        "Set the mark price for the current product."
        self.mark = price

    def position(self):
        "Return the current position this inventory is holding."
        raise NotImplementedError

    def cost(self):
        "Return the original cost of our active position."
        raise NotImplementedError

    def value(self):
        "Return the marked value of the entire position."
        pos = self.position()
        if pos != 0:
            if self.mark is None:
                raise ValueError("You need to set the mark to obtain the pnl")
            return pos * self.mark
        else:
            return Decimal()

    def avgprice(self):
        "Return the average price paid for each unit of the current position."
        pos = self.position()
        return float(self.cost()) / float(pos) if pos != 0 else None

    def realized_pnl(self):
        return self._realized_pnl

    def unrealized_pnl(self):
        "Return the P+L for our current position (not including past realized P+L."
        pos = self.position()
        if pos == 0:
            return Decimal()
        if self.mark is None:
            raise ValueError("You need to set the mark to obtain the pnl")
        return self.position() * self.mark - self.cost()

    def pnl(self):
        "Return the P+L for our current position (not including past realized P+L."
        return self._realized_pnl + self.unrealized_pnl()

    def close(self):
        "Close all the current positions at the mark price."
        self.trade(self.mark, -self.position())
        assert self.position() == 0

    def trade(self, price, quant, extra=None):
        """
        Book a trade for size 'quant' at the given 'price'.
        The 'extra' parameter is used for inventories that book trades, to
        report on the history of which trades are summarized in the realized
        pnl.
        """

        raise NotImplementedError

    def dump(self):
        print '---------------', self
        print 'position       ', self.position()
        print 'mark           ', self.mark if self.mark else None
        print 'avgprice       ', self.avgprice() or 0
        print 'value          ', self.value() if self.mark else None
        print 'cost           ', self.cost()
        print 'unrealized_pnl ', self.unrealized_pnl() if self.mark else None
        print 'realized_pnl   ', self.realized_pnl()
        print 'pnl            ', self.pnl() if self.mark else None

    def serialize(self):
        """
        Return a dict of values that can be used to store and restore the
        inventory later. This is used to persist the inventory state for its
        P+L.
        """
        return {
            'position': self.position(),
            'cost': self.cost(),
            'realized_pnl': self.realized_pnl(),
            'mark': self.mark if self.mark else None,
            }
                    
    def unserialize(self, m):
        pass



#
# Concrete implementations.
#

class BookingInventory(Inventory):
    "A base class for classes that book trades."

    def __init__(self):
        Inventory.__init__(self)
        self.reset_position()

    def reset_position(self):
        # A list of (price, size) tuples for our active position.
        self.inventory = deque()

    def position(self):
        return sum(s for p, s, e in self.inventory) if self.inventory else Decimal()

    def cost(self):
        return sum(p*s for p, s, e in self.inventory) if self.inventory else Decimal()

    def trade(self, price, quant, extra=None):
        self.last_trade_price = price

        extras = []

        # Book the new trade against existing positions if the trade is not on
        # the same side as our current position.
        pos = self.position()
        if quant * pos < 0:

            # Process all the position records.
            done = 0
            while self.inventory:
                rec_price, rec_quant, rec_extra = self.record_popnext()
                if abs(quant) >= abs(rec_quant):
                    # This record is entirely consumed by the trade.
                    booked_quant = rec_quant
                else:
                    # This record is only partially consumed by the trade.
                    # We reinsert the updated record and exit the loop.
                    booked_quant = -quant
                    self.record_reinsert((rec_price, rec_quant + quant, rec_extra))
                    done = 1

                quant += booked_quant
                self._realized_pnl += booked_quant * (price - rec_price)
                extras.append( (rec_extra, booked_quant, rec_price) )
                if done:
                    break

            assert quant * self.position() >= 0

        # Append the remainder of our trade to the inventory if not all was
        # booked. Note: we don't bother joining the last record if it is at the
        # same price as the trade.
        if quant != 0:
            self.record_append((price, quant, extra))

        # Don't forget to add this trade to the list as well.
        extras.append( (extra, quant, price) )

        self.realized_extras.extend(extras)
        return extras

    def dump(self):
        super(BookingInventory, self).dump()
        print 'inventory      ', ', '.join('%s @ %s' % (s, p) for p, s, e in self.inventory)

    def record_popnext(self):
        "Pop and return the next trade record."

    def record_reinsert(self, rec):
        "Reinsert a removed trade record."

    def record_append(self, rec):
        "Append a new trade record."


class AvgInventory(Inventory):
    "A concrete inventory class that books trades at their average price."

    def reset_position(self):
        # Because we want to average the price of our trades, we lump together
        # all the position as a single trade and update the price.
        self._position = 0

        # Note: we keep the internal cost value as a float to minimize numerical
        # error.
        self._cost = Decimal()

    def position(self):
        return self._position

    def cost(self):
        return int(self._cost)

    def trade(self, price, quant, extra=None):
        # Note: 'extra' does not make sense for the average inventory.

        self.last_trade_price = price

        pos = self._position
        if quant * pos < 0:

            if abs(quant) > abs(pos):
                # Our position is entirely consumed by the trade.
                booked_quant = pos
                delta_cost = self._cost
            else:
                booked_quant = -quant
                # Our position is partially consumed by the trade: book the
                # trades at their current average price.
                delta_cost = (booked_quant * self._cost) / pos

            self._cost -= delta_cost
            self._position -= booked_quant
            self._realized_pnl += booked_quant * price - delta_cost

            quant += booked_quant
            assert quant * self.position() >= 0

        # Append the remainder of our trade to our position.
        if quant != 0:
            self._position += quant
            self._cost += quant * price


class FIFOInventory(BookingInventory):
    "An inventory that books trades in a first-in/first-out fashion."

    def record_popnext(self):
        return self.inventory.popleft()

    def record_reinsert(self, rec):
        self.inventory.appendleft(rec)

    def record_append(self, rec):
        self.inventory.append(rec)


class LIFOInventory(BookingInventory):
    "An inventory that books trades in a lsat-in/first-out fashion."

    def record_popnext(self):
        return self.inventory.pop()

    def record_reinsert(self, rec):
        self.inventory.append(rec)

    def record_append(self, rec):
        self.inventory.append(rec)


