"""
Code utilised to obtain market values.
"""

# stdlib imports
from __future__ import with_statement
import re, urllib, threading, logging
from decimal import Decimal
from datetime import datetime

# other imports
from beancount.fallback.BeautifulSoup import BeautifulSoup


__all__ = ('get_market_price', 'currencies', 'get_xrate', 'get_xrates')


currencies = ['USD', 'CAD', 'JPY', 'EUR', 'AUD', 'CHF', 'BRL']

market_currency = {
    'NYSE': 'USD',
    'TSE': 'CAD',
    }

url_google = 'http://finance.google.com/finance?q=%s'

def getquote_google(sym):
    ssym = sym.strip().lower()
    f = urllib.urlopen(url_google % ssym)
    soup = BeautifulSoup(f)
    el = soup.find('span', 'pr')
    if el is not None:
        # Find the quote currency.
        h1 = soup.find('h1')
        mstr = h1.next.next
        mstr = mstr.replace('&nbsp;', '').replace('\n', '')
        mstring = '\\(([A-Za-z]+),\\s+([A-Z]+):%s\\)' % ssym.upper()
        mo = re.match(mstring, mstr)
        if mo is not None:
            market = mo.group(2)
            comm = market_currency[market]
        else:
            raise ValueError("Unknown market: %s for %s" % (mstr, ssym))
        price = Decimal(el.contents[0])

        chg = soup.find('span', 'bld')
    else:
        comm, price, chg = None, None
        
        
    url = '' % (symbol, stat)
    return urllib.urlopen(url).read().strip().strip('"')



url_yahoo = 'http://download.finance.yahoo.com/d/quotes.csv?s=%s&f=l1c1'

def specDecimal(s):
    if s == 'N/A':
        return Decimal()
    else:
        return Decimal(s)

def getquote_yahoo(sym, pcomm):
    ssym = sym.strip().lower()
    if pcomm == 'CAD':
        ssym += '.TO'
    f = urllib.urlopen(url_yahoo % ssym)
    contents = f.read().strip()
    price, change = [specDecimal(x) for x in contents.split(',')]
    return (price, change)

getquote = getquote_yahoo



# A dict of commodity id to (market price, underlying commodity).
_market_prices = {}
_market_prices_lock = threading.Lock()

def get_market_price(comm, pcomm):
    try:
        return _market_prices[(comm, pcomm)]
    except KeyError:
        t = GetQuoteThread(comm, pcomm)
        t.start()
        return (None, None)
    
class GetQuoteThread(threading.Thread):
    
    def __init__(self, comm, pcomm):
        threading.Thread.__init__(self)
        self.key = (comm, pcomm)

    def run(self):
        logging.info("Fetching price for %s" % str(self.key))
        r = getquote(*self.key)
        with _market_prices_lock:
            _market_prices[self.key] = r
        logging.info("Price for %s = %s" % (self.key, r))
        




def find_currency_rows(soup):
    """
    Find the table rows that have an exchange rate and yield them.
    """
    for row in soup.findAll('tr'):
        for td in row.findAll('td'):
            bolds = td.findAll('b')
            if not bolds:
                break
            else:
                b = bolds[0].contents[0]
                mo = re.match('([A-Z]{3})/([A-Z]{3})', b)
                if mo is None:
                    break
                yield row, mo.group(1, 2)

months = dict((x,i+1)
              for i,x in enumerate(('Jan Feb Mar Apr May Jun Jul'
                                    ' Aug Sep Oct Nov Dec').split()))

def get_rate(tr):
    """
    Given a table row, get the bid and ask as Decimal objects and parse the
    date/time.
    """
    nodes = [x.find('font').contents[0] for x in tr.findAll('td')[1:]]
    bid = Decimal(nodes[0].strip())
    ask = Decimal(nodes[1].strip())
    mo = re.match('\s*[A-Z][a-z]+\s+([A-Z][a-z]+)\s+(\d+)\s+(\d+):(\d+):(\d+)\s+(\d{4})',
                  nodes[2])
    assert mo, nodes[2]
    year, day, hour, min, sec = map(int, mo.group(6, 2, 3, 4, 5))
    mth = months[mo.group(1)]
    dt = datetime(year, mth, day, hour, min, sec)
    return bid, ask, dt


_xrates_url = 'http://www.oanda.com/rtrates'

# Cache of (quote, base) to (bid, ask, time).
_xrates = {}

def refresh_xrates():
    _xrates.clear()
    fn, info = urllib.urlretrieve(_xrates_url)
    soup = BeautifulSoup(open(fn))
    for tr, (quote, base) in find_currency_rows(soup):
        bid, ask, time = get_rate(tr)
        _xrates[(quote, base)] = (bid, ask, time)

def get_xrate(quote, base):
    "Get the given exchange rate."
    try:
        r = _xrates[(quote, base)]
    except KeyError:
        t = threading.Thread(target=refresh_xrates)
        t.start()
        refresh_xrates()
        r = None
    return r
    
def get_xrates():
    """ Return all the exchange rates we have. """
    if _xrates:
        return _xrates.copy()
    else:
        refresh_xrates()



