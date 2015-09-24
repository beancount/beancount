"""A kludge to hack FIFO bookings until Beancount actually has them supported.
"""

__author__ = 'Martin Blais <blais@furius.ca>'
__plugins__ = ('rebook_as_fifo',)

from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core import data
from beancount.core import amount
from beancount.core import position
from beancount.parser import printer


CCY_FIFO = 'BTC'
CCY_CASH = 'USD'


def is_special(posting):
    """"A special posting is one in the FIFO currency that doesn't have a cost and that
    has a price.
    """
    pos = posting.position
    return (pos.lot.currency == CCY_FIFO and
            pos.lot.cost is None and
            posting.price is not None)


def rebook_as_fifo(entries, options_map):
    new_entries = []
    errors = []
    for entry in entries:

        # Figure out if this transaction has postings in Bitcoins without a cost.
        # The purpose of this plugin is to fixup those.
        if (isinstance(entry, data.Transaction) and
            any(is_special(posting) for posting in entry.postings)):

            # Segregate the reducing lots, augmenting lots and other lots.
            reducing = []
            augmenting = []
            other = []
            for index, posting in enumerate(entry.postings):
                if is_special(posting):
                    out = reducing if posting.position.number < ZERO else augmenting
                else:
                    out = other
                out.append(posting)

            # We will create a replacement list of postings with costs filled
            # in, possibly more than the original list, to account for the
            # different lots.
            new_postings = []

            # First, book the reducing postings as FIFO on the list of all
            # accumulated Bitcoins.
            for posting in reducing:
                # FIXME: get cost amount... need to loop here.
                new_lot = posting.position.lot._replace(
                    cost=amount.Amount(cost, CCY_CASH))
                new_postings.append(posting._replace(
                    position=position.Position(new_lot, pos.number)))

            # Second, compute the total difference between the prices and costs
            # allocated, and add a new Income leg to account for it.
            # FIXME: TODO

            # Third, add back all the othe legs in.
            # FIXME: TODO

            entry = entry._replace(postings=new_postings)

        new_entries.append(entry)

    return new_entries, errors
