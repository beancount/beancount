"""
All the actual web pages.
This is isolated in a module so we can reload it on every request while we're
developing.
"""

# stdlib imports
import logging
from wsgiref.util import request_uri, application_uri
from os.path import *
from operator import attrgetter
from datetime import date
from urlparse import urlparse
from itertools import izip, count

# fallback imports
from beancount.fallback import xmlout
from beancount.fallback.xmlout import *

# beancount imports
from beancount.ledger import compute_balsheet, Account
from beancount.utils import render_tree, itertree
from beancount.wallet import Wallet
from beancount.web.serve import *
from beancount import cmdline



class Template(object):
    "Base template for all our pages."

    output_encoding = 'utf8'

    def __init__(self):
        self.initialize()

    def initialize(self):
        self.header = DIV(A(IMG(src=umap('@@Logo'), id='logo')),
                          id='header')

        self.body = BODY()
        self.head = HEAD(
            META(http_equiv="Content-Type",
                 content="text/html; charset=%s" % self.output_encoding),
            LINK(rel='stylesheet', href=umap('@@Style'), type='text/css'),
            SCRIPT(type="text/javascript", src=umap('@@Treetable')),
            )
        self.html = HTML(self.head, self.body)

        # Add a common header for all pages.
        self.document = DIV(id='document')

        self.navigation = DIV(
            UL(LI(A('Chart of Accounts', href=umap('@@ChartOfAccounts'))),
               LI(A('Trial Balance', href=umap('@@TrialBalance'))),
               LI(A('Ledgers', href=umap('@@LedgerIndex'))),
               LI(A('Balance Sheet', href=umap('@@BalanceSheet'))),
               LI(A('P&L', href=umap('@@IncomeStatement'))),
               LI(A('Capital', href=umap('@@CapitalStatement'))),
               LI(A('Activity', href=umap('@@Activity'))),
               LI(A('Stats', href=umap('@@Statistics'))),
               LI(A('Source', href=umap('@@Source'))),
               LI(A('Log', href=umap('@@Messages'))),
               ),
            id='top-navigation')

        self.reload = DIV(A("Reload", href=umap('@@Reload')), id='reload')

        self.style = DIV(
            UL(LI(A('Compact', href=umap('@@SetStyle', style='compact'))),
               LI(A('Other', href=umap('@@SetStyle', style='other'))),
               LI(A('Only', href=umap('@@SetStyle', style='only'))),
               LI(A('Full', href=umap('@@SetStyle', style='full'))),
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
        contents = tostring(self.html, app,
                            encoding=self.output_encoding)

def ljoin(l, sep):
    "Intersperse the given list with the object 'seq'."
    if l:
        nl = [(x, sep) for x in l[:-1]] + [l[-1]]
    else:
        nl = l
    return nl

def hwallet(w):
    "Return some HTML suitable for rendering a wallet."
    return ljoin([SPAN('%s %s' % (a,c), CLASS='amount') for (c,a) in w.round().tostrlist()], ', ')



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

@cachefunc
def haccount(accname):
    "Return some HTML for a full account name."
    l = []
    append = l.append
    comps = []
    cappend = comps.append
    for comp in accname.split(Account.sep):
        cappend(comp)
        name = Account.sep.join(comps)
        append(A(comp, href=umap('@@AccountLedger', name), CLASS='accomp'))
    accspan = SPAN(ljoin(l, SPAN(Account.sep, CLASS='accsep')), CLASS='account')
    accspan.cache = 1
    return accspan

def chartofaccounts(app, ctx):
    page = Template()

    table = TABLE(id='chart-of-accounts', CLASS='accounts treetable')
    table.add(THEAD(TR(TH("Account"), TH("Dr/Cr"), TH("Valid Commodities"))))
    it = iter(itertree(ctx.ledger.get_root_account()))
    for acc, td1, tr, skip in treetable_builder(table, it):
        td1.add(
            A(acc.name, href=umap('@@AccountLedger', acc.fullname), CLASS='accomp'))
        tr.add(
            TD(acc.getatype()),
            TD(", ".join(acc.commodities) if acc.commodities else ""),
            ## TD("%d" % len(acc.postings)),
            )

    page.add(H1("Chart of Accounts"), table)
    return page.render(app)

def stats(app, ctx):
    page = Template()
    page.add(H1("Statistics"))
    page.add(P("FIXME TODO"))
## FIXME: add application options
## FIXME: add global ledger values, filenames, nb. transactions, etc.
## FIXME: maybe Source should just become a link from this page
## FIXME: maybe Log should just become a link from this page
    return page.render(app)

def trial(app, ctx):
    page = Template()

    local = False
    at_cost = False

    # First compute the trial balance.
    compute_balsheet(ctx.ledger, 'local_balance', 'balance', at_cost)

    table = TABLE(id='balance', CLASS='accounts treetable')
    table.add(THEAD(TR(TH("Account"), TH("Cumulative"), TH("Individual"))))
    it = iter(itertree(ctx.ledger.get_root_account()))
    for acc, td1, tr, skip in treetable_builder(table, it):
        if len(acc) == 0:
            skip()
            continue
        td1.add(
            A(acc.name, href=umap('@@AccountLedger', acc.fullname), CLASS='accomp'))
        tr.add(
            TD(hwallet(getattr(acc, 'balance').round()), CLASS='wallet'),
            TD(hwallet(getattr(acc, 'local_balance').round()), CLASS='wallet'))

    page.add(H1('Trial Balance'), table)
    return page.render(app)

def balance(app, ctx):
    page = Template()
    page.add(H1("Balance Sheet"))
    page.add(P("FIXME TODO"))
    return page.render(app)

def pnl(app, ctx):
    page = Template()
    page.add(H1("Income Statement / P&L Report"))
    page.add(P("FIXME TODO"))
    return page.render(app)

def capital(app, ctx):
    page = Template()
    page.add(H1("Capital Statement"))
    page.add(P("FIXME TODO"))
    return page.render(app)

def positions(app, ctx):
    page = Template()
    page.add(H1("Positions / Assets"))
    page.add(P("FIXME TODO - add a summary of market values of current positions for assets, with changes"))
    return page.render(app)



def treetable_builder(tbl, iterator, skiproot=False):
    """
    Given a TABLE object in 'tbl' and a root 'node', create and iterate over the
    rows for creating a JavaScript tree with the treetable JS source. Note that
    the table needs to have a unique 'id' attribute. This is an iterator that yields

      (node, td-of-column-1, tr-for-row)

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



def activity(app, ctx):
    "Output the updated time ranges of each account."

    page = Template()

    today = date.today()
    table = TABLE(id='activity', CLASS='accounts treetable')
    table.add(THEAD(TR(TH("Account"),
                       TH("Oldest Chk"),
                       TH("Newest Chk"),
                       TH("Days since"),
                       TH("Last Posting"),
                       TH("Days since"),
                       )))
    it = iter(itertree(ctx.ledger.get_root_account(), pred=attrgetter('checked')))
    for acc, td1, tr, _ in treetable_builder(table, it):
        td1.add(
            A(acc.name, href=umap('@@AccountLedger', acc.fullname), CLASS='accomp'))

        if acc.checked:
            elapsed = today - acc.check_max
            tr.extend(TD(str(x)) for x in (acc.check_min, acc.check_max, '%s days' % elapsed.days))

        if acc.postings:
            post_last = acc.postings[-1]
            elapsed = today - post_last.actual_date
            tr.extend(TD(str(x)) for x in (post_last.actual_date, '%s days' % elapsed.days))

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


def ledgeridx(app, ctx):
    ledger = app.ledger
    
    page = Template()
    ul = UL(
        LI(A("General Ledger (all transactions)", href=umap('@@GeneralLedger'))),
        )
    page.add(H1("Ledgers"),
             ul,
             P(I("Note: These ledgers display transactions for all accounts; for by-account ledgers, click on any account name in any other view.")),
             )

    if ledger.transactions:
        date_oldest = ledger.transactions[0].actual_date
        mths = list(iter_months(date_oldest, date.today()))
        for d in reversed(mths):
            mthstr = d.strftime('%Y-%m')
            ul.add(LI(A("Ledger for %s" % mthstr,
                        href=umap("@@MonthLedger", d.year, d.month)
                      )))

    return page.render(app)



def ledger(app, ctx):
    """
    List the transactions that pertain to a list of filtered postings.
    """
    page = Template()
    table = TABLE(id='ledger')

    style = ctx.session.get('style', 'other')
    assert style in ('compact', 'other', 'only', 'full')

    accname = getattr(ctx, 'accname', '')
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

    # Get the list of transactions that related to the postings.
    txns = set(post.txn for post in postings)

    # Get the list of checks for this account and include them in the listing.
    checks = ctx.ledger.directives['check']
    acc_checks = sorted(checks.account_checks(acc))

    balance = Wallet()
    for txn in sorted(txns):
        if dbegin is not None:
            if not (dbegin <= txn.actual_date < dend):
                continue

        register_insert_checks(acc_checks, table, txn.actual_date)

        try:
            sty = 'background-color: %s' % flag_colors[txn.flag]
        except KeyError:
            sty = ''

        # Sum the balance of the selected postings from this transaction.
        txn_amount = Wallet()
        for post in txn.postings:
            if post in postings:
                txn_amount += post.amount

        # Add this amount to the balance.
        balance += txn_amount

        # Display the transaction line.
        tr = TR(TD(txn.rdate()),
                TD(txn.flag, CLASS='flag', style=sty),
                TD(txn.description(), CLASS='description'),
                TD(CLASS='wallet'),
                TD(hwallet(txn_amount), CLASS='wallet'),
                TD(hwallet(balance), CLASS='wallet'),
                CLASS='txn')
        table.add(tr)

        # Display the postings.
        if style != 'compact':
            for post in txn.postings:
                if post in postings:
                    if style == 'other':
                        continue
                else:
                    if style == 'only':
                        continue

                postacc = haccount(post.account.fullname)
                if post.note:
                    postacc = SPAN(SPAN(postacc), SPAN(';', post.note, CLASS='postnote'))
                tr = TR(TD(post.rdate(), colspan='2', CLASS='postdate'),
                        TD(postacc),
                        TD(hwallet(post.amount), CLASS='wallet'),
                        TD(),
                        TD(),
                        CLASS='posting')
                table.add(tr)

    # Add the remaining checks.
    register_insert_checks(acc_checks, table)

    if acc.isroot():
        page.add(H1('General Ledger'), table)
    else:
        page.add(H1('Ledger for ', haccount(acc.fullname)), table)

    return page.render(app)

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


def source(app, ctx):
    """
    Serve the source of the ledger.
    """
    page = Template()
    div = DIV(id='source')
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

def messages(app, ctx):
    """
    Report all ledger errors.
    """
    page = Template()
    page.add(H1('Parsing Messages'))

    ledger = ctx.ledger
    div = page.add(DIV(CLASS='message'))
    tbl = div.add(TABLE())
    for msg in ledger.messages:
        name = msgname[msg.level]
        tbl.add(TR(TD(name.capitalize(), CLASS=name),
                      TD(A(msg.message, href=umap('@@Source') + '#line%d' % (msg.lineno or 0)))))

    return page.render(app)


def reload(app, ctx):
    """
    Reload the ledger file and return to the given URL.
    """
    app.ledger = cmdline.reload(ctx.ledger, app.opts)
    raise HttpRedirect(ctx.environ['HTTP_REFERER'])



def setstyle(app, ctx):
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

def server_error(app, ctx):
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
    ('@@Logo', static("header-universal-dollar.jpg", 'image/jpeg'), '/header.jpg', None),
    ('@@Home', redirect('@@ChartOfAccounts'), '/', None),

    ('@@ChartOfAccounts', chartofaccounts, '/accounts', None),
    ('@@Statistics', stats, '/stats', None),
    ('@@Activity', activity, '/activity', None),
    ('@@TrialBalance', trial, '/trial', None),
    ('@@BalanceSheet', balance, '/balance', None),
    ('@@IncomeStatement', pnl, '/pnl', None),
    ('@@CapitalStatement', capital, '/capital', None),
    ('@@Positions', positions, '/positions', None),

    ('@@LedgerIndex', ledgeridx, '/ledger/index', None),
    ('@@GeneralLedger', ledger, '/ledger/general', None),
    ('@@MonthLedger', ledger, '/ledger/bymonth/%04d/%02d', '^/ledger/bymonth/(?P<year>\d\d\d\d)/(?P<month>\d\d)$'),
    ('@@AccountLedger', ledger, '/ledger/byaccount/%s', '^/ledger/byaccount/(?P<accname>.*)$'),

    ('@@SetStyle', setstyle, '/setstyle', '^/setstyle$'),
    ('@@Messages', messages, '/messages', None),
    ('@@Reload', reload, '/reload', None),
    ('@@Source', source, '/source', None),
    ('@@Error', server_error, '/error', None),

    )

mapper = Mapper(page_directory)
umap = urlmap = mapper.map

