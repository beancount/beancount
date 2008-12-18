"""
All the actual web pages.
This is isolated in a module so we can reload it on every request while we're
developing.
"""

# stdlib imports
import sys, logging, re
from wsgiref.util import request_uri, application_uri
from os.path import *
from operator import attrgetter
from datetime import date
from urlparse import urlparse
from itertools import izip, count
from pprint import pformat
from decimal import Decimal, getcontext
from collections import defaultdict

# fallback imports
from beancount.fallback import xmlout
from beancount.fallback.xmlout import *

# beancount imports
from beancount.ledger import Account
from beancount.ledger import VIRT_NORMAL, VIRT_BALANCED, VIRT_UNBALANCED
from beancount.utils import render_tree, itertree
from beancount.wallet import Wallet
from beancount.web.serve import *
from beancount.utils import iter_pairs
from beancount import cmdline
from beancount.web.market import *



class Template(object):
    "Base template for all our pages."

    output_encoding = 'utf-8'

    def __init__(self, ctx):
        self.initialize(ctx)

    def initialize(self, ctx):
        self.header = DIV(SPAN(ctx.opts.title or ' ', id='title'), id='header')

        self.body = BODY()
        self.head = HEAD(
            META(http_equiv="Content-Type",
                 content="text/html; charset=%s" % self.output_encoding),
            LINK(rel='stylesheet', href=umap('@@Style'), type='text/css'),
            SCRIPT(' ', type="text/javascript", src=umap('@@Treetable')),
            )
        self.html = HTML(self.head, self.body)

        # Add a common header for all pages.
        self.document = DIV(id='document')

        self.navigation = DIV(
            UL(LI(A('Chart of Accounts', href=umap('@@ChartOfAccounts'))),
               LI(A('Journals', href=umap('@@LedgerIndex'))),
               LI(A('Trial Balance', href=umap('@@TrialBalance'))),
               LI(A('Balance Sheet', href=umap('@@BalanceSheet'))),
               LI(A('Income', href=umap('@@IncomeStatement'))),
               LI(A('CashFlow', href=umap('@@CashFlow'))),
               LI(A('Capital', href=umap('@@CapitalStatement'))),
               LI(A('Payees', href=umap('@@Payees'))),
               LI(A('Positions', href=umap('@@Positions'))),
               LI(A('Trades', href=umap('@@Trades'))),
               LI(A('Activity', href=umap('@@Activity'))),
               LI(A('Locations', href=umap('@@Locations'))),
               LI(A('Stats/Logs', href=umap('@@Statistics'))),
               ),
            id='top-navigation')

        self.reload = DIV(A("Reload", href=umap('@@Reload')), id='reload')

        self.style = DIV(
            UL(LI(A('Com', href=umap('@@SetStyle', style='compact'))),
               LI(A('Oth', href=umap('@@SetStyle', style='other'))),
               ##LI(A('O', href=umap('@@SetStyle', style='only'))),
               LI(A('Ful', href=umap('@@SetStyle', style='full'))),
               ),
            id='style-selector')

        self.body.add(self.header)
        self.body.add(self.reload)
        self.body.add(self.style)
        self.body.add(self.navigation)
        self.body.add(self.document)

        self.add = self.document.add

    def render(self, app):
        app.setHeader('Content-Type','text/html')
        ##app.write(doctype)
        contents = tostring(self.html, app,
                            encoding=self.output_encoding,
                            pretty=True)

doctype = '''\
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
'''



def ljoin(l, sep):
    "Intersperse the given list with the object 'seq'."
    if l:
        nl = [(x, sep) for x in l[:-1]] + [l[-1]]
    else:
        nl = l
    return nl

def hwallet(w, round=True):
    "Return some HTML suitable for rendering a wallet."
    if round:
        w = w.round()
    return ljoin([SPAN('%s %s' % (a,c), CLASS='amount') for (c,a) in w.tostrlist()], ', ')

def hwallet_paren(w, round=True):
    "A version of hwallet that renders negative numbers in parentheses."
    if round:
        w = w.round()
    l = []
    for (c,a) in w.tostrlist():    
        if a >= 0:
            a = SPAN('%s %s' % (a,c), CLASS='amount')
        else:
            a = SPAN('(%s) %s' % (-a,c), CLASS='amount')
        l.append(a)
    return ljoin(l, ', ')



# Scavenged from Mercurial.
def cachefunc(func):
    '''cache the result of function calls'''
    # XXX doesn't handle keywords args
    cache = {}
    if func.func_code.co_argcount == 1:
        # we gain a small amount of time because
        # we don't need to pack/unpack the list
        def f(arg):
            if arg not in cache:
                cache[arg] = func(arg)
            return cache[arg]
    else:
        def f(*args):
            if args not in cache:
                cache[args] = func(*args)
            return cache[args]

    return f

websep = '~'

def webaccname(accname):
    return accname.replace(':', websep)

@cachefunc
def haccount_split(accname):
    """Return some HTML for a full account name. This version makes each
    subaccount clickable."""
    l = []
    append = l.append
    comps = []
    cappend = comps.append
    for comp in accname.split(Account.sep):
        cappend(comp)
        wname = websep.join(comps)
        append(A(comp, href=umap('@@AccountLedger', wname), CLASS='accomp'))
    accspan = SPAN(ljoin(l, SPAN(Account.sep, CLASS='accsep')), CLASS='account')
    accspan.cache = 1
    return accspan

@cachefunc
def haccount(accname):
    "Return some HTML for a full account name. There is a single link."
    accspan = SPAN(
        A(accname, href=umap('@@AccountLedger', webaccname(accname)),
          CLASS='accomp'),
        CLASS='account')
    accspan.cache = 1
    return accspan


def page__chartofaccounts(app, ctx):
    page = Template(ctx)

    table = TABLE(id='chart-of-accounts', CLASS='accounts treetable')
    table.add(THEAD(TR(TH("Account"), TH("Dr/Cr"), TH("Valid Commodities"))))
    it = iter(itertree(ctx.ledger.get_root_account()))
    for acc, td1, tr, skip in treetable_builder(table, it):
        td1.add(
            A(acc.name, href=umap('@@AccountLedger', webaccname(acc.fullname)),
              CLASS='accomp'))
        tr.add(
            TD(acc.getatype()),
            TD(", ".join(acc.commodities) if acc.commodities else ""),
            ## TD("%d" % len(acc.postings)),
            )

    page.add(H1("Chart of Accounts"), table)
    return page.render(app)

def page__stats(app, ctx):
    page = Template(ctx)
    page.add(H1("Statistics, Logs and Other Info"))

    page.add(H2("Command-line Options"), PRE(' '.join(sys.argv)))

    ledger = ctx.ledger
    page.add(H2("Statistics"),
             TABLE(
                 TR(TD("Nb Transactions:"), TD("%d" % len(ledger.transactions))),
                 TR(TD("Nb Postings:"), TD("%d" % len(ledger.postings)))
                 ))

    ul = UL( LI(A('Message Log (and Errors)', href=umap('@@Messages'))),
             LI(A('Pricing Commodities', href=umap('@@Pricing'))),
             LI(A('Source', href=umap('@@Source'))),
             LI(A('Resources Required for CSS (for websuck)', href=umap('@@ScrapeResources'))))
    page.add(H2("Links"), ul)

    ul = UL()
    prices = ledger.directives['price'].prices
    for (base, quote), phist in prices.iteritems():
        sym = '%s/%s' % (base, quote)
        ul.add( LI(A("Price History for %s" % sym,
                      href=umap('@@PriceHistory', base, quote))) )
    page.add(H2("Price Histories"), ul)

    return page.render(app)

def page__scraperes(app, ctx):
    page = Template(ctx)
    page.add(H1("Scrape Resources"))
    for id_ in ('@@FolderOpen', '@@FolderClosed', '@@HeaderBackground'):
        page.add(IMG(src=umap(id_)))
    page.add(A("Home", href=umap('@@Home')))

    return page.render(app)



def page__trialbalance(app, ctx):
    page = Template(ctx)

    table = TABLE(id='balance', CLASS='accounts treetable')
    table.add(THEAD(TR(TH("Account"), TH("Debit"), TH("Credit"), TH(), TH("Cum. Sum"))))
    it = iter(itertree(ctx.ledger.get_root_account()))
    sumdr, sumcr = Wallet(), Wallet()
    for acc, td1, tr, skip in treetable_builder(table, it):
        if len(acc) == 0:
            skip()
            continue
        td1.add(
            A(acc.name, href=umap('@@AccountLedger', webaccname(acc.fullname)),
              CLASS='accomp'))
        
        lbal = acc.local_balance.convert(app.opts.conversions).round()
        dr, cr = lbal.split()
        sumdr += dr
        sumcr += cr
        tr.add(
            TD(hwallet_paren(dr), CLASS='wallet'),
            TD(hwallet_paren(-cr), CLASS='wallet'),
            )

        bal = acc.balance.convert(app.opts.conversions).round()
        if not lbal and bal:
            tr.add(
                TD('...'),
                TD(hwallet_paren(bal),
                   CLASS='wallet'),
                )

    table.add(TR(TD(), TD(hwallet_paren(sumdr)), TD(hwallet_paren(-sumcr)),
                 TD(), TD()))

    page.add(H1('Trial Balance'), table)
    return page.render(app)


def semi_table(acc, tid, remove_empty=True, conversions=None):
    """ Given an account, create a table for the transactions contained therein
    (including its subaccounts)."""

    table = TABLE(id=tid, CLASS='semi accounts treetable')
    table.add(THEAD(TR(TH("Account"), TH("Debit"), TH("Credit"))))
    it = iter(itertree(acc))
    sumdr, sumcr = Wallet(), Wallet()
    for acc, td1, tr, skip in treetable_builder(table, it):
        if remove_empty and len(acc) == 0:
            skip()
            continue

        td1.add(
            A(acc.name, href=umap('@@AccountLedger', webaccname(acc.fullname)),
              CLASS='accomp'))

        local_balance = acc.local_balance
        if conversions:
            local_balance = local_balance.convert(conversions)

        dr, cr = local_balance.split()
        sumdr += dr
        sumcr += cr
        tr.add(
            TD(hwallet_paren(dr.round()) if dr else ''),
            TD(hwallet_paren(-cr.round()) if cr else ''),
            )

    table.add(TR(TD(B("Totals")),
                 TD(hwallet_paren(sumdr)),
                 TD(hwallet_paren(-sumcr))))

    net = sumdr + sumcr
    ## table.add(TR(TD(B("Net")),
    ##              TD(hwallet_paren(net))
    ##              ))

    return table, net


def page__balancesheet(app, ctx):
    page = Template(ctx)
    page.add(H1("Balance Sheet"))

    ledger = ctx.ledger

    a_acc = ledger.find_account(('Assets', 'Asset'))
    l_acc = ledger.find_account(('Liabilities', 'Liability'))
    e_acc = ledger.find_account(('Equity', 'Capital'))
    if None in (a_acc, l_acc, e_acc):
        page.add(P("Could not get all A, L and E accounts.", CLASS="error"))
        return page.render(app)

    a_table, a_total = semi_table(a_acc, 'assets', conversions=app.opts.conversions)
    l_table, l_total = semi_table(l_acc, 'liabilities', conversions=app.opts.conversions)
    e_table, e_total = semi_table(e_acc, 'equity', conversions=app.opts.conversions)
    page.add(DIV(H2("Liabilities", CLASS="duotables"), l_table,
                 H2("Equity", CLASS="duotables"), e_table,
                 CLASS='right'),
             DIV(H2("Assets", CLASS="duotables"), a_table,
                 CLASS='left'),
             )

    total = a_total + l_total + e_total
    page.add(BR(style="clear: both"),
             TABLE( TR(TD(B("Net Income:")), TD(hwallet(total))),
                    id='net', CLASS='treetable') )

    return page.render(app)



def page__income(app, ctx):
    page = Template(ctx)
    page.add(H1("Income Statement / P&L Report"))

    ledger = ctx.ledger

    i_acc = ledger.find_account(('Income', 'Revenue', 'Revenues'))
    e_acc = ledger.find_account(('Expenses', 'Expense'))
    if None in (i_acc, e_acc):
        page.add(P("Could not get all unique income and expenses accounts.", CLASS="error"))
        return page.render(app)

    i_table, i_total = semi_table(i_acc, 'income', conversions=app.opts.conversions)
    e_table, e_total = semi_table(e_acc, 'expenses', conversions=app.opts.conversions)
    page.add(DIV(H2("Expenses", CLASS="duotables"), e_table,
                 CLASS='right'),
             DIV(H2("Income", CLASS="duotables"), i_table,
                 CLASS='left'),
             )

    total = i_total + e_total
    net = TABLE(id='net', CLASS='treetable')
    net.add(
        TR(TD("Net Difference"),
           TD(hwallet(total))))
    page.add(BR(style="clear: both"),
             H2("Net Difference"), net)

    return page.render(app)


def page__capital(app, ctx):
    page = Template(ctx)
    page.add(H1("Capital Statement"))
    page.add(P("FIXME TODO"))
    return page.render(app)


def page__cashflow(app, ctx):
    page = Template(ctx)
    page.add(H1("Cash-Flow Statement"))
    page.add(P("FIXME TODO"))
    return page.render(app)


def page__payees(app, ctx):
    page = Template(ctx)
    page.add(H1("Payees"))
    ul = page.add(UL())
    for payee in sorted(ctx.ledger.payees.iterkeys()):
        ul.add(LI(A(payee, href=umap('@@PayeeLedger', payee))))

    return page.render(app)



refcomm = 'USD'

market_url = 'http://finance.google.com/finance?q=%s'

def page__positions(app, ctx):
    page = Template(ctx)
    page.add(H1("Positions / Assets"))
## FIXME: remove
    return page.render(app)


    # First compute the trial balance.
    ledger = ctx.ledger

    a_acc = ledger.find_account(('Assets', 'Asset'))
    if a_acc is None:
        page.add(P("Could not find assets account.", CLASS="error"))
        return page.render(app)

    # Add a table of currencies that we're interested in.
    icurrencies = set(c for c in a_acc.balance.iterkeys() if c in currencies)

    try:
        xrates = get_xrates()
    except IOError:
        xrates = None
    if xrates:
        tbl = TABLE(id="xrates")
        tbl.add(THEAD(TR(TD("Quote"), TD("Base"), TD("Bid"), TD("Ask"))))
        for (quote, base), (bid, ask, dtime) in xrates.iteritems():
            if quote in icurrencies and base in icurrencies:
                tds = [TD(str(x)) for x in (quote, base, bid, ask)]
                tbl.add(TR(tds))
        page.add(H2("Exchange Rates"), tbl)

    # Add a table of positions.
    tbl = TABLE(id="positions")
    tbl.add(THEAD(TR(TD("Description"),
                     TD("Position"), TD("Currency"), TD("Price"), TD("Change"),
                     TD("Total Value"), TD("Total Change"),
                     TD("Total Value (USD)"), TD("Total Change (USD)"))))
    commnames = ledger.directives['defcomm'].commnames
    for comm, amount in a_acc.balance.tostrlist():
        # Strip numbers from the end, to remove splits.
        mo = re.match('(.*)[\'~][0-9]', comm)
        if mo:
            comm = mo.group(1)
        try:
            if comm in currencies:
                pcomms = []
            else:
                pcomms = [x for x in ledger.pricedmap[comm] if x in currencies]
        except KeyError:
            logging.error(comm, ledger.pricedmap)
            continue
        assert len(pcomms) in (0, 1), "Ambiguous commodities: %s" % pcomms
        if pcomms:
            pcomm = pcomms[0]
            try:
                price, change = get_market_price(comm, pcomm)
            except IOError:
                price, change = None, None
            if price is None:
                fprice = fchange = \
                    totvalue = totchange = \
                    totvalue_usd = totchange_usd = "..."
            else:
                fprice = "%s %s" % (price, pcomm)
                fchange = "%s %s" % (change, pcomm)
                totvalue = "%s %s" % (amount * price, pcomm)
                totchange = "%s %s" % (amount * change, pcomm)
                if pcomm == refcomm:
                    rate = Decimal("1")
                else:
                    urate = xrates.get((pcomm, refcomm), None)
                    if urate is not None:
                        bid, ask, _ = urate
                        rate = (bid + ask) / 2
                    else:
                        irate = xrates.get((refcomm, pcomm), None)
                        if irate is not None:
                            bid, ask, _ = irate
                            rate = 1 / ((bid + ask) / 2)
                        else:
                            rate = None

                if rate is not None:
                    totvalue_usd = "%.2f %s" % (amount * price * rate, refcomm)
                    totchange_usd = "%.2f %s" % (amount * change * rate, refcomm)
                else:
                    totvalue_usd = ""
                    totchange_usd = ""
        else:
            pcomm = ''
            fprice = fchange = totvalue = totchange = totvalue_usd = totchange_usd = ''
            if comm in currencies:
                totvalue = "%s %s" % (amount, comm)

        market, name = commnames.get(comm, u'')
        if market is not None:
            name = A(name, href=market_url % market)
        tds = [TD(name, CLASS='l'),
               TD("%s %s" % (amount, comm)), TD(pcomm),
               TD(fprice), TD(fchange),
               TD(totvalue), TD(totchange),
               TD(totvalue_usd), TD(totchange_usd)]

        tbl.add(TR(tds))

    page.add(H2("Total Assets"), tbl)

    return page.render(app)



def page__pricing(app, ctx):
    "A list of the commodities used for pricing each type of financial asset."
    page = Template(ctx)
    page.add(H1("Pricing Commodities"))

    # First compute the trial balance.
    ledger = ctx.ledger

    # Add a table of positions.
    tbl = TABLE(id="pricedmap")
    tbl.add(THEAD(TR(TD("Commodity"), TD("Price Currency"))))
    for comm, pclist in sorted(ledger.pricedmap.iteritems()):
        tbl.add(TR( TD(comm), TD(', '.join(pclist)) ))

    page.add(tbl)

    return page.render(app)


def page__pricehistory(app, ctx):
    """A page that describes the samples of a price history. It expects
    base/quote commodities as input."""
    page = Template(ctx)
    page.add(H1("Price History for %s/%s" % (ctx.base, ctx.quote)))
    
    tbl, = page.add(TABLE(THEAD(TR(TH("Date"), TH("Rate"))),
                          CLASS='price_history'))

    prices = app.ledger.directives['price'].prices
    phist = prices[(ctx.base, ctx.quote)]
    for date_, rate in phist:
        tbl.append( TR(TD(date_.isoformat()), TD(str(rate))) )

    return page.render(app)




def treetable_builder(tbl, iterator, skiproot=False):
    """
    Given a TABLE object in 'tbl' and a root 'node', create and iterate over the
    rows for creating a JavaScript tree with the treetable JS source. Note that
    the table needs to have a unique 'id' attribute. This is an iterator that yields

      (node, td-of-column-1, tr-for-row, skip-function)

    You need to add the relevant columns on the row object, using data in the
    node.
    """
    iid = tbl.attrib['id']

    ## page.add(
    ##     A(u'<+>', onclick="treetable_expandAll('balance');"),
    ##     A(u'<->', onclick="treetable_collapseAll('balance');"))

    if skiproot:
        iterator.next()

    skipflag = []
    def skipfun():
        skipflag.append(1)

    spc = SPAN(CLASS='foldspc')
    for ordering, isterminal, node in iterator:
        rowid = '%s_%s' % (iid, '_'.join(str(x) for x in ordering))

        pretitle = [spc] * (len(ordering))
        if not isterminal:
            folder = A(IMG(src=umap("@@FolderOpen"), CLASS='folder'),
                       # href='#',
                       onclick="treetable_toggleRow('%s');" % rowid)
            pretitle[-1] = folder

        td = TD(pretitle, CLASS='tree')
        tr = TR(td, id=rowid)
        yield node, td, tr, skipfun
        if skipflag:
            skipflag[:] = []
        else:
            tbl.add(tr)



def page__activity(app, ctx):
    "Output the updated time ranges of each account."

    page = Template(ctx)

    today = date.today()
    table = TABLE(id='activity', CLASS='accounts treetable')
    table.add(THEAD(TR(TH("Account"),
                       TH("Oldest Chk"),
                       TH("Newest Chk"),
                       TH("Last Posting"),
                       TH("Days since"),
                       )))
    it = iter(itertree(ctx.ledger.get_root_account(), pred=attrgetter('checked')))
    for acc, td1, tr, _ in treetable_builder(table, it):
        td1.add(
            A(acc.name, href=umap('@@AccountLedger', webaccname(acc.fullname)),
              CLASS='accomp'))

        append = False
        row = [TD() for _ in xrange(4)]
        elapsed_check, elapsed_post = None, None
        if acc.checked:
            row[0].add(str(acc.check_min))
            row[1].add(str(acc.check_max))
            elapsed_check = (today - acc.check_max).days
            append = True

        if acc.postings:
            post_last = acc.postings[-1]
            row[2].add(str(post_last.actual_date))
            elapsed_post = (today - post_last.actual_date).days
            append = True

        if append:
            row[3].add('%s days' % min(filter(lambda x: x is not None,
                                              [elapsed_check, elapsed_post])))
            tr.extend(row)

    page.add(H1('Activity'), table)
    return page.render(app)



def iter_months(oldest, newest):
    """Yield dates for the first day of every month between oldest and newest."""
    cdate = date(oldest.year, oldest.month, 1)
    while 1:
        yield cdate
        mth = cdate.month % 12 + 1
        year = cdate.year + (1 if mth == 1 else 0)
        cdate = date(year, mth, 1)
        if cdate > newest:
            break


def page__ledgerindex(app, ctx):
    ledger = app.ledger

    page = Template(ctx)
    ul = UL(
        LI(A("General Journal", href=umap('@@GeneralLedger')),
           "  (all transactions by-date)"),
        )
    page.add(H1("Journals"),
             ul,
             ## P(I("""Note: These journals display transactions for accounts, ordered by-date; for
             ##        ledgers (transactions by-account), click on any account name
             ##        in any other view.""")),
             )

    if ledger.transactions:
        date_oldest = min(x.actual_date for x in ledger.transactions)
        date_youngest = max(x.actual_date for x in ledger.transactions)
        mths = list(iter_months(date_oldest, date_youngest))
        for d in reversed(mths):
            mthstr = d.strftime('%Y-%m')
            ul.add(LI(A("Journal for %s" % mthstr,
                        href=umap("@@MonthLedger", d.year, d.month)
                      )))

    return page.render(app)



## FIXME I want to split this and support a page for single payee transactions
## and flow statement.

def page__ledger(app, ctx):
    """
    List the transactions that pertain to a list of filtered postings.
    """
    page = Template(ctx)
    style = ctx.session.get('style', 'full')
    assert style in ('compact', 'other', 'only', 'full')

    accname = getattr(ctx, 'accname', '')
    accname = accname.replace(websep, Account.sep)
    try:
        acc = ctx.ledger.get_account(accname)
    except KeyError:
        raise HttpNotFound(accname)
    postings = set(acc.subpostings())

    year = getattr(ctx, 'year', '')
    mth = getattr(ctx, 'month', '')
    if year and mth:
        year = int(year)
        mth = int(mth)
        dbegin = date(year, mth, 1)
        mth = mth % 12 + 1
        if mth == 1:
            year += 1
        dend = date(year, mth, 1)
    else:
        dbegin = None

    # Get the list of checks for this account and include them in the listing.
    checks = ctx.ledger.directives['check']
    acc_checks = sorted(checks.account_checks(acc))

    if dbegin is not None:
        def dfilter(txn):
            if not (dbegin <= txn.actual_date < dend):
                return True
    else:
        dfilter = None

    table = render_postings_table(postings, style, dfilter, acc_checks)

    if acc.isroot():
        if dbegin is None:
            page.add(H1('General Journal'), table)
        else:
            page.add(H1('Journal for %s' % dbegin), table)
    else:
        page.add(H1('Ledger for ', haccount_split(acc.fullname)), table)

    return page.render(app)


def page__ledger_payee(app, ctx):
    """
    List the transactions that pertain to a list of filtered postings.
    """
    page = Template(ctx)
    style = ctx.session.get('style', 'full')
    assert style in ('compact', 'other', 'only', 'full')

    payee = getattr(ctx, 'payee', '')
    txns = ctx.ledger.payees[payee]

    postings = []
    for txn in txns:
        postings.extend(txn.postings)

    table = render_postings_table(postings, style)

    page.add(H1('Payee transactions for %s' % payee), table)
    return page.render(app)



def render_postings_table(postings, style,
                          filterfun=None,
                          acc_checks=None,
                          amount_overrides=None):

    table = TABLE(CLASS='txntable')

    # Get the list of transactions that relate to the postings.
    txns = set(post.txn for post in postings)

    balance = Wallet()
    for txn in sorted(txns):
        if filterfun is not None and filterfun(txn) is True:
            continue

        if acc_checks is not None:
            register_insert_checks(acc_checks, table, txn.actual_date)

        try:
            sty = 'background-color: %s' % flag_colors[txn.flag]
        except KeyError:
            sty = ''

        # Sum the balance of the selected postings from this transaction.
        txn_amount = Wallet()
        for post in txn.postings:
            if post in postings:
                if amount_overrides and post in amount_overrides:
                    amt = amount_overrides[post]
                else:
                    amt = post.amount
                txn_amount += amt

        # Add this amount to the balance.
        balance += txn_amount

        # Display the transaction line.
        tr = TR(TD(txn.rdate()),
                TD(txn.flag, CLASS='flag', style=sty),
                TD(txn.description(), CLASS='description'),
                TD(CLASS='wallet'),
                TD(hwallet(txn_amount), CLASS='wallet'),
                TD(hwallet(balance), CLASS='wallet cumulative'),
                CLASS='txn')
        table.add(tr)

        # Display the postings.
        if style != 'compact':
            for post in txn.postings:
                inlist = post in postings
                if inlist:
                    if style == 'other':
                        continue
                elif style == 'only':
                        continue

                postacc = haccount(post.account.fullname)
                if post.virtual == VIRT_UNBALANCED:
                    postacc = ['(', SPAN(postacc), ')']
                elif post.virtual == VIRT_BALANCED:
                    postacc = ['[', SPAN(postacc), ']']
                if post.note:
                    postacc = [postacc, SPAN(';', post.note, CLASS='postnote')]
                td_account =TD(postacc)
                if inlist:
                    td_account.attrib['class'] = 'highpost'
                tr = TR(TD(post.rdate(), colspan='2', CLASS='postdate'),
                        td_account,
                        TD(hwallet(post.amount), CLASS='wallet'),
                        TD(['@ ', hwallet(post.price, round=False)] if post.price else '', CLASS='price'),
                        TD(),
                        CLASS='posting')

                table.add(tr)

    # Add the remaining checks.
    if acc_checks is not None:
        register_insert_checks(acc_checks, table)

    return table




# Colors for the flag cell.
flag_colors = {'!': '#F66',
               '*': '#AFA',
               'A': '#AAF'}

# Colors for the body of the check row in the register.
check_colors = {'!': '#F66',
               '*': '#AFA',
               'A': '#AFA'}


def register_insert_checks(checklist, table, date=None):
    """
    Insert checks in the register.
    Note: this modified 'checklist' to remove the added checks.
    """
    while 1:
        if not checklist:
            break
        chk = checklist[0]
        if date is None or chk.cdate <= date:
            sty = 'background-color: %s' % flag_colors[chk.flag]
            trsty = 'background-color: %s' % check_colors[chk.flag]
            tr = TR(TD(str(chk.cdate)),
                    TD(chk.flag, CLASS='flag', style=sty),
                    TD(u'Check at %s:%s' % (chk.filename, chk.lineno),
                       CLASS='description check'),
                    TD(hwallet(chk.expected), CLASS='wallet'),
                    TD(hwallet(chk.diff)),
                    TD(hwallet(chk.balance), CLASS='wallet'),
                    CLASS='assert', style=trsty)
            table.add(tr)
            del checklist[0]
        else:
            break


def page__source(app, ctx):
    """
    Serve the source of the ledger.
    """
    page = Template(ctx)
    div = DIV(id='source')
    if app.opts.unsafe:
        div.add(P("(Sorry, source not available.)"))
    else:
        for i, line in izip(count(1), ctx.ledger.source):
            div.add(PRE("%4d  |%s" % (i, line.strip())), A(name='line%d' % i))

    page.add(H1('Source'), div)
    return page.render(app)



msgname = {
    logging.ERROR: 'error',
    logging.WARNING: 'warning',
    logging.INFO: 'info',
    logging.CRITICAL: 'critical',
    }

def page__messages(app, ctx):
    """
    Report all ledger errors.
    """
    page = Template(ctx)
    page.add(H1('Parsing Messages'))

    ledger = ctx.ledger
    div = page.add(DIV(CLASS='message'))
    tbl = div.add(TABLE())
    for msg in ledger.messages:
        name = msgname[msg.level]
        tbl.add(TR(TD(name.capitalize(), CLASS=name),
                      TD(A(msg.message, href=umap('@@Source') + '#line%d' % (msg.lineno or 0)))))

    return page.render(app)


ramq_reqdays = 183

def page__locations(app, ctx):
    page = Template(ctx)
    page.add(H1("Locations"))

    location = ctx.ledger.directives['location']

    # Group lists per year.
    peryear = defaultdict(list)
    for x in sorted(location.locations):
        ldate = x[0]
        peryear[ldate.year].append(x)

    today = date.today()

    # Cap lists beginnings and ends.
    yitems = sorted(peryear.iteritems())
    city, country = "", ""
    for year, ylist in yitems:
        ldate, _, _ = ylist[0]
        if (ldate.month, ldate.day) != (1, 1):
            ylist.insert(0, (date(year, 1, 1), city, country))

        ldate, city, country = ylist[-1]
        if (ldate.month, ldate.day) != (1, 1):
            d = date(year+1, 1, 1)
            if d > today:
                d = today
            ylist.append((d, city, country))

    for year, ylist in yitems:
        ul = page.add(H2(str(year)), UL())
        comap = defaultdict(int)
        ramq_days = 0
        for x1, x2 in iter_pairs(ylist, False):
            ldate1, city, country = x1
            ldate2, _, _ = x2
            days = (ldate2 - ldate1).days
            ul.append(LI("%s -> %s (%d days) : %s (%s)" % (ldate1, ldate2, days, city, country)))
            comap[country] += days
            if country == 'Canada' or days < 21:
                ramq_days += days
                # FIXME: I think that technically I would have to be in Quebec,
                # not just in Canada.

        ulc = page.add(H2("Summary %s" % year), UL())
        for country, days in sorted(comap.iteritems()):
            ulc.append(LI("%s : %d days" % (country, days)))

        missing_days = ramq_reqdays - ramq_days
        ulc.add(LI("... for RAMQ eligibility: %d days / %d : %s" %
                   (ramq_days, ramq_reqdays,
                    ('Missing %d days' % missing_days if missing_days > 0 else 'Okay'))))

    return page.render(app)


def page__trades(app, ctx):
    """
    Render a list of trades.
    """
    page = Template(ctx)
    page.add(H1("Trades"))

    style = ctx.session.get('style', 'full')
    ledger = ctx.ledger

    for bt in ledger.booked_trades:

        postings = [x.post for x in bt.legs]
        overrides = dict((x.post, Wallet(bt.comm_book, x.amount_book))
                         for x in bt.legs)

        legs_table = TABLE(
            THEAD(
                TR(TH("Date"), TH("Units"), TH("Native Price"),
                   TH("Native Amount"), TH("Target Rate"), TH("Target Amount"))),
            CLASS="trades")

        for leg in bt.legs:
            legs_table.add(
                TR(TD(str(leg.post.actual_date)),
                   TD(hwallet(Wallet(bt.comm_book, leg.amount_book))),
                   TD(hwallet(Wallet(leg.comm_price, leg.price))),
                   TD(hwallet(Wallet(leg.comm_price, leg.amount_price))),
                   TD(hwallet(Wallet('%s/%s' % (leg.comm_price, bt.comm_target or '-'), leg.xrate))),
                   TD(hwallet(Wallet(bt.comm_target or leg.comm_price, leg.amount_target))),
                   ))

        post_book = bt.post_book

        legs_table.add(
            TR(TD('Gain/-Loss'),
               TD(),
               TD(),
               TD(hwallet(-bt.post_book.amount_orig)),
               TD(),
               TD(hwallet(-bt.post_book.amount)),
               ))

        table = render_postings_table(postings, style,
                                      amount_overrides=overrides)
        title = '%s - %s %s ; %s' % (
            bt.close_date(),
            bt.comm_book,
            'in %s' % bt.comm_target if bt.comm_target else '',
            bt.account.fullname)
        page.add(DIV(H2(title), legs_table, P("Corresponding transactions:"), table,
                     CLASS='btrade'))

    return page.render(app)



def page__reload(app, ctx):
    """
    Reload the ledger file and return to the given URL.
    """
    app.ledger = cmdline.reload(ctx.ledger, app.opts)
    raise HttpRedirect(ctx.environ['HTTP_REFERER'])



def page__setstyle(app, ctx):
    "Set the session's style and redirect where we were."
    ctx.session['style'] = ctx.style[0]
    raise HttpRedirect(ctx.environ['HTTP_REFERER'])



def redirect(*args):
    "Return a resource to redirect to the given resource id."
    def redirect_res(app, ctx):
        raise HttpRedirect(umap(*args))
    return redirect_res

def static(fn, ctype):
    """Return a handler for a static file to be served, with caching.
    Caching is disabled when we're in debug/development mode. """
    cache = []
    def f(app, ctx):
        if ctx.debug or not cache:
            result = open(join(dirname(__file__), fn)).read()
            cache.append(result)
        else:
            result = cache[0]
        app.setHeader('Content-Type', ctype)
        app.write(result)
    return f

def page__servererror(app, ctx):
    app.setHeader('Content-Type','text/html')
    app.write('TODO')
    ## FIXME return the error page here.





# page-id, callable-handler, render-format, regexp-for-matching
# If the regexp is left to a value of None, it is assumed it matches the render string exactly.
page_directory = (

    ('@@Style', static('style.css', 'text/css'), '/style.css', None),
    ('@@Treetable', static('treetable.js', 'text/javascript'), '/treetable.js', None),
    ('@@FolderOpen', static('folder_open.png', 'image/png'), '/folder_open.png', None),
    ('@@FolderClosed', static('folder_closed.png', 'image/png'), '/folder_closed.png', None),
    ('@@HeaderBackground', static("header-universal-dollar.jpg", 'image/jpeg'), '/header.jpg', None),
    ('@@ScrapeResources', page__scraperes, '/scraperes', None),

    ('@@Home', redirect('@@ChartOfAccounts'), '/', None),
    ('@@HomeIndex', redirect('@@ChartOfAccounts'), '/index', None),

    ('@@ChartOfAccounts', page__chartofaccounts, '/accounts', None),
    ('@@Statistics', page__stats, '/stats', None),
    ('@@Activity', page__activity, '/activity', None),
    ('@@TrialBalance', page__trialbalance, '/trial', None),
    ('@@BalanceSheet', page__balancesheet, '/balance', None),
    ('@@IncomeStatement', page__income, '/income', None),
    ('@@CapitalStatement', page__capital, '/capital', None),
    ('@@CashFlow', page__cashflow, '/cashflow', None),
    ('@@Payees', page__payees, '/payees', None),
    ('@@Positions', page__positions, '/positions', None),
    ('@@Locations', page__locations, '/locations', None),
    ('@@Trades', page__trades, '/trades', None),
    ('@@Pricing', page__pricing, '/pricing', None),
    ('@@PriceHistory', page__pricehistory, '/price/history/%s/%s',
     '^/price/history/(?P<base>[^/]+)/(?P<quote>[^/]+)$'),

    ('@@LedgerIndex', page__ledgerindex, '/ledger/index', None),
    ('@@GeneralLedger', page__ledger, '/ledger/general', None),
    ('@@MonthLedger', page__ledger, '/ledger/bymonth/%04d/%02d',
     '^/ledger/bymonth/(?P<year>\d\d\d\d)/(?P<month>\d\d)$'),

    ('@@AccountLedger', page__ledger, '/ledger/byaccount/%s',
     '^/ledger/byaccount/(?P<accname>.*)$'),
    ('@@PayeeLedger', page__ledger_payee, '/ledger/bypayee/%s',
     '^/ledger/bypayee/(?P<payee>.*)$'),

    ('@@SetStyle', page__setstyle, '/setstyle', '^/setstyle$'),
    ('@@Messages', page__messages, '/messages', None),
    ('@@Reload', page__reload, '/reload', None),
    ('@@Source', page__source, '/source', None),
    ('@@Error', page__servererror, '/error', None),

    )

mapper = Mapper(page_directory)
umap = urlmap = mapper.map

