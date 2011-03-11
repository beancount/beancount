"""
Wallet arithmetic tests.
"""

# stdlib imports
import unittest
from decimal import Decimal

# beancount imports
from beancount.wallet import Wallet



class TestWallet(unittest.TestCase):

    def test_init(self):
        print Wallet
        w = Wallet()
        print w
        print len(w)
        w['USD'] = 42
        o = w['USD']
        print o, repr(o)


    def test_simple(self):
        "Simple wallet tests."

        w = Wallet()
        assert len(w) == 0

        # Test constructing with keywords.
        w = Wallet(CAD='5.688')
        assert len(w) == 1
        assert w['CAD'] == Decimal('5.688')

        # Test special shortcut constructor.
        w = Wallet('CAD', '5.688')
        assert len(w) == 1
        assert w['CAD'] == Decimal('5.688')

        # Test special shortcut constructor.
        w = Wallet('1 CAD, 2 USD')
        assert len(w) == 2
        assert w['CAD'] == Decimal('1')
        assert w['USD'] == Decimal('2')

        # Test copy constructor.
        w2 = Wallet(w)
        assert w is not w2
        assert w == w2

        # Test copy method.
        ww = w.copy()
        assert isinstance(ww, Wallet), ww

    def test_operators(self):
        "Test some of the basic operations on wallets."

        w1 = Wallet(CAD='17.1')
        w2 = Wallet(CAD='0.9')
        assert w1 + w2 == Wallet(CAD='18.0')
        assert w1 - w2 == Wallet(CAD='16.2')
        assert w1 * 2 == Wallet(CAD='34.2')
        assert w1 / 2 == Wallet(CAD='8.55')

    def test_round(self):
        "Test rounding."

        w = Wallet(CAD='17.1343843', USD='83.42434', EUR='6.237237232')
        mprecision = dict((('USD', Decimal('.01')),
                           ('EUR', Decimal('.0001')),
                           ('JPY', Decimal('.00001')),
                           (None, Decimal('.1')),
                           ))
        wr = w.round(mprecision)
        assert wr['EUR'] == Decimal('6.2372')
        assert wr['USD'] == Decimal('83.42')
        assert wr['CAD'] == Decimal('17.10')

    def test_equality(self):
        "Test equality predicates."

        w1 = Wallet(CAD='17.1343843')
        w1p = Wallet(CAD='17.1343843')
        w2 = Wallet(USD='83.42434')
        w3 = Wallet(CAD='17.8888')
        assert w1 == w1p
        assert w2 != w1
        assert w2 != w1p
        assert w1 != w3

    def test_neg(self):
        "Test negative value."

        w = Wallet(CAD='17.1343843',
                   USD='83.42434')
        assert -w == Wallet(CAD='-17.1343843',
                            USD='-83.42434')

    def test_only(self):
        "Test the only filter."

        w = Wallet(CAD='17.1343843',
                   USD='83.42434')
        assert w.only('CAD') == Wallet(CAD='17.1343843')


    def test_price(self):
        
        w = Wallet(AAPL='20')
        w.price('AAPL', 'USD', Decimal('10'))
        assert w == Wallet(USD='200')
        
        w = Wallet(AAPL='20', MSFT='10.1')
        w.price('AAPL', 'USD', Decimal('10'))
        assert w == Wallet(USD='200', MSFT='10.1')

    def test_split(self):

        w = Wallet()
        wp, wn = w.split()
        assert w == (wp + wn)

        w = Wallet('10 USD')
        wp, wn = w.split()
        assert w == (wp + wn)
        
        w = Wallet('-10 CAD')
        wp, wn = w.split()
        assert w == (wp + wn)

        w = Wallet(USD='10', CAD='-10')
        wp, wn = w.split()
        assert w == (wp + wn)

    def test_convert(self):
        conv = [('INR', 'CAD', 1/Decimal('40'))]

        w = Wallet()
        assert w.convert(conv) == w
        
        w = Wallet('10 USD')
        assert w.convert(conv) == w
        
        w = Wallet('10 INR')
        assert w.convert(conv) == Wallet('0.25 CAD')

        w = Wallet('1600 INR')
        assert w.convert(conv) == Wallet('40 CAD')

    def test_nbthings(self):
        w = Wallet()
        w['USD'] = Decimal('4.1')
        w['JPY'] = Decimal('17.0')
        assert w.nbthings() == Decimal('21.1')



if __name__ == '__main__':
    unittest.main()
