"""
Test price history interpolation.
"""

# stdlib imports
from datetime import date
from decimal import Decimal

# beancount imports
from beancount.ledger import PriceHistory



class TestPriceHistory(object):

    def test_phist(self):
        phist = PriceHistory()
        phist.append( (date(2008, 2, 3), Decimal('1.1567')) )
        phist.append( (date(2008, 2, 18), Decimal('1.2123')) )
        phist.append( (date(2008, 2, 26), Decimal('1.4023')) )

        assert phist.interpolate(date(2008, 2, 2)) == Decimal('1.1567')
        assert phist.interpolate(date(2008, 2, 3)) == Decimal('1.1567')
        assert phist.interpolate(date(2008, 2, 4)) > Decimal('1.1567')
        assert phist.interpolate(date(2008, 2, 17)) < Decimal('1.2123')
        assert phist.interpolate(date(2008, 2, 18)) == Decimal('1.2123')
        assert phist.interpolate(date(2008, 2, 19)) > Decimal('1.2123')
        assert phist.interpolate(date(2008, 2, 25)) < Decimal('1.4023')
        assert phist.interpolate(date(2008, 2, 26)) == Decimal('1.4023')
        assert phist.interpolate(date(2008, 2, 27)) == Decimal('1.4023')


