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

# other imports
import xmlout
from xmlout import *

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
            UL(LI(A('Home', href=umap('@@Home'))),
               LI(A('Ranges', href=umap('@@Ranges'))),
               LI(A('Balance Sheet', href=umap('@@Balance'))),
               LI(A('General Ledger', href=umap('@@GeneralLedger'))),
               LI(A('Source', href=umap('@@Source'))),
               LI(A('Errors', href=umap('@@Messages'))),
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
        append(A(comp, href=umap('@@Register', name), CLASS='accomp'))
    accspan = SPAN(ljoin(l, SPAN(Account.sep, CLASS='accsep')), CLASS='account')
    accspan.cache = 1
    return accspan

def info(app, ctx):
    page = Template()
    page.add(H1("Ledger Home"))
    return page.render(app)

def balance(app, ctx):
    page = Template()

    local = False
    at_cost = False

    # First compute the balance sheet.
    compute_balsheet(ctx.ledger, 'local_balance', 'balance', at_cost)

    table = TABLE(id='balance', CLASS='treetable')
    it = iter(itertree(ctx.ledger.get_root_account()))
    for acc, td1, tr, skip in treetable_builder(table, it):
        if len(acc) == 0:
            skip()
            continue
        td1.add(
            A(acc.name, href=umap('@@Register', acc.fullname), CLASS='accomp'))
        tr.add(
            TD(hwallet(getattr(acc, 'balance').round()), CLASS='wallet'),
            TD(hwallet(getattr(acc, 'local_balance').round()), CLASS='wallet'))

    page.add(H1('Balance Sheet'), table)
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





def ranges(app, ctx):
    "Output the updated ranges of each account."

    page = Template()

    today = date.today()
    table = TABLE(id='ranges', CLASS='treetable')
    table.add(THEAD(TR(TH(), TH("Oldest Chk"), TH("Newest Chk"), TH("Days since"))))
    it = iter(itertree(ctx.ledger.get_root_account(), pred=attrgetter('checked')))
    for acc, td1, tr, _ in treetable_builder(table, it):
        td1.add(
            A(acc.name, href='/register/%s' % acc.fullname, CLASS='accomp'))

        if acc.checked:
            elapsed = today - acc.check_max
            tr.extend(TD(str(x)) for x in (acc.check_min, acc.check_max, '(%s days)' % elapsed.days))
        else:
            tr.extend(TD() for _ in xrange(3))

    page.add(H1('Updated Ranges'), table)
    return page.render(app)



def register(app, ctx):
    """
    List the transactions that pertain to a list of filtered postings.
    """
    page = Template()
    table = TABLE(id='register')

    style = ctx.session.get('style', 'other')
    assert style in ('compact', 'other', 'only', 'full')

    accname = getattr(ctx, 'accname', '')
    try:
        acc = ctx.ledger.get_account(accname)
    except KeyError:
        raise HttpNotFound(accname)
    postings = set(acc.subpostings())

    # Get the list of transactions that related to the postings.
    txns = set(post.txn for post in postings)

    # Get the list of checks for this account and include them in the listing.
    checks = ctx.ledger.directives['check']
    acc_checks = sorted(checks.account_checks(acc))

## FIXME: remove
    ## for c in acc_checks:
    ##     print c

    balance = Wallet()
    for txn in sorted(txns):
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
        page.add(H1('Register for', haccount(acc.fullname)), table)

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
                      TD(A(msg.message, href=umap('@@Source') + '#line%d' % msg.lineno))))

    return page.render(app)


def reload(app, ctx):
    """
    Reload the ledger file and return to the given URL.
    """
    app.ledger = cmdline.reload(ctx.ledger)

    # Filter out the selected postings.
    pred = cmdline.create_filter_pred(app.opts)
    app.ledger.filter_postings(pred)

    raise HttpRedirect(ctx.environ['HTTP_REFERER'])



def setstyle(app, ctx):
    "Set the session's style and redirect where we were."
    ctx.session['style'] = ctx.style[0]
    raise HttpRedirect(ctx.environ['HTTP_REFERER'])



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
    ('@@FolderOpen', static('folder_open.png', 'text/javascript'), '/folder_open.png', None),
    ('@@FolderClosed', static('folder_closed.png', 'text/javascript'), '/folder_closed.png', None),
    ('@@Logo', static("header-universal-dollar.jpg", 'image/jpeg'), '/header.jpg', None),
    ('@@Home', info, '/', None),
    ('@@Info', info, '/info', None),
    ('@@Ranges', ranges, '/ranges', None),
    ('@@Balance', balance, '/balance', None),
    ('@@GeneralLedger', register, '/register', None),
    ('@@Register', register, '/register/%s', '^/register/(?P<accname>.*)$'),
    ('@@SetStyle', setstyle, '/setstyle', '^/setstyle$'),
    ('@@Messages', messages, '/messages', None),
    ('@@Reload', reload, '/reload', None),
    ('@@Source', source, '/source', None),
    ('@@Error', server_error, '/error', None),

    )

mapper = Mapper(page_directory)
umap = urlmap = mapper.map

