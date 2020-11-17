"""Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import io
import logging
import re
import sys
import time
import threading
import datetime
import calendar

import bottle
from bottle import response
from bottle import request

from beancount.core import data
from beancount.core import getters
from beancount.core import account
from beancount.core import account_types
from beancount.core import compare
from beancount.core import convert
from beancount.ops import basicops
from beancount.core import prices
from beancount.utils import misc_utils
from beancount.utils import text_utils
from beancount.parser import version
from beancount.web import bottle_utils
from beancount.parser import options
from beancount.parser import printer
from beancount import loader
from beancount.web import views
from beancount.utils import scrape
from beancount.reports import html_formatter
from beancount.reports import balance_reports
from beancount.reports import journal_html
from beancount.reports import journal_reports
from beancount.reports import holdings_reports
from beancount.reports import price_reports
from beancount.reports import misc_reports
from beancount.parser import context


# The default view page to redirect to.
DEFAULT_VIEW_REDIRECT = 'balsheet'


# The protocol to use for file links.
FILELINK_PROTOCOL = 'beancount://{filename}?lineno={lineno}'


class HTMLFormatter(html_formatter.HTMLFormatter):
    """A formatter object that can be used to render accounts links.

    Attributes:
      build_url: A function used to render links to a Bottle application.
      leafonly: a boolean, if true, render only the name of the leaf nodes.
    """
    def __init__(self, dcontext, build_url, leaf_only, account_xform, view_links=True):
        super().__init__(dcontext)
        self.build_url = build_url
        self.leaf_only = leaf_only
        self.view_links = view_links
        self.account_xform = account_xform

    def build_global(self, *args, **kwds):
        "Render to global application."
        return app.router.build(*args, **kwds)

    EMS_PER_COMPONENT = 1.5

    def render_account(self, account_name):
        """See base class."""
        if self.view_links:
            if self.leaf_only:
                # Calculate the number of components to figure out the indent to
                # render at.
                components = account.split(account_name)
                indent = '{:.1f}'.format(len(components) * self.EMS_PER_COMPONENT)
                anchor = '<a href="{}" class="account">{}</a>'.format(
                    self.build_url('journal',
                                   account_name=self.account_xform.render(account_name)),
                    account.leaf(account_name))
                return '<span "account" style="padding-left: {}em">{}</span>'.format(
                    indent, anchor)
            else:
                anchor = '<a href="{}" class="account">{}</a>'.format(
                    self.build_url('journal',
                                   account_name=self.account_xform.render(account_name)),
                    account_name)
                return '<span "account">{}</span>'.format(anchor)
        else:
            if self.leaf_only:
                # Calculate the number of components to figure out the indent to
                # render at.
                components = account.split(account_name)
                indent = '{:.1f}'.format(len(components) * self.EMS_PER_COMPONENT)
                account_name = account.leaf(account_name)
                return '<span "account" style="padding-left: {}em">{}</span>'.format(
                    indent, account_name)
            else:
                return '<span "account">{}</span>'.format(account_name)

    def render_inventory(self, inv):
        """Override this formatter to convert the inventory to units only."""
        return super().render_inventory(inv.reduce(convert.get_units))

    def render_context(self, entry):
        """See base class."""
        # Note: rendering to global application.
        # Note(2): we could avoid rendering links to summarizing and transfer
        # entries which are not going to be found.
        return self.build_global('context', ehash=compare.hash_entry(entry))

    def render_link(self, link):
        """See base class."""
        # Note: rendering to global application.
        return self.build_global('link', link=link)

    def render_doc(self, filename):
        """See base class."""
        return '<a href="{}" class="filename">{}</a>'.format(
            self.build_url('doc', filename=filename.lstrip('/')),
            path.basename(filename))

    def render_event_type(self, event):
        """See base class."""
        return '<a href="{}">{}</a>'.format(self.build_url('event', event=event),
                                            event)

    def render_commodity(self, base_quote):
        """See base class."""
        base, quote = base_quote
        return '<a href="{}">{} / {}</a>'.format(
            self.build_url('prices', base=base, quote=quote), base, quote)

    def render_source(self, source):
        """See base class."""
        return '{}#{}'.format(app.get_url('source'), source['lineno'])


def render_report(report_class, entries, args=None,
                  css_id=None, css_class=None, center=False, leaf_only=True):
    """Instantiate a report and rendering it to a string.

    Args:
      report_class: A class, the type of the report to render.
      real_root: An instance of RealAccount to render.
      args: A list of strings, the arguments to initialize the report with.
      css_id: An optional string, the CSS id for the div to render.
      css_class: An optional string, the CSS class for the div to render.
      center: A boolean flag, if true, wrap the results in a <center> tag.
      leaf_only: A boolean, whether to render the leaf names only.
    Returns:
      A string, the rendered report.
    """
    formatter = HTMLFormatter(app.options['dcontext'],
                              request.app.get_url, leaf_only, app.account_xform)
    oss = io.StringIO()
    if center:
        oss.write('<center>\n')
    report_ = report_class.from_args(args,
                                     formatter=formatter,
                                     css_id=css_id,
                                     css_class=css_class)
    report_.render_htmldiv(entries, app.errors, app.options, oss)
    if center:
        oss.write('</center>\n')
    return oss.getvalue()


def render_real_report(report_class, real_root, price_map, price_date,
                       args=None, leaf_only=False):
    # pylint: disable=too-many-arguments
    """Instantiate a report and rendering it to a string.

    This is intended to be called in the context of a Bottle view app request
    (it uses 'request').

    Args:
      report_class: A class, the type of the report to render.
      real_root: An instance of RealAccount to render.
      price_map: A price map as built by build_price_map().
      price_date: The date at which to evaluate the prices.
      args: A list of strings, the arguments to initialize the report with.
      leaf_only: A boolean, whether to render the leaf names only.
    Returns:
      A string, the rendered report.
    """
    formatter = HTMLFormatter(app.options['dcontext'],
                              request.app.get_url, leaf_only, app.account_xform)
    oss = io.StringIO()
    report_ = report_class.from_args(args, formatter=formatter)
    report_.render_real_htmldiv(real_root, price_map, price_date, app.options, oss)
    return oss.getvalue()


#--------------------------------------------------------------------------------
# Global application pages.


NOT_IMPLEMENTED = '''
<p style="color:red; font-weight: bold; font-style: italic;">
  (Feature not implemented yet.)
</p>
'''

# pylint: disable=invalid-name
app = bottle.Bottle()
A = bottle_utils.AttrMapper(app.router.build)


def render_overlay(contents):
    """Render an overlay of the navigation with the current errors.

    This is used to bring up new errors on any page when they occur.

    Returns:
      A string of HTML for the contents of the errors overlay.
    """
    return '''
      <div class="navigation" id="nav-right">
        <ul>
          {}
        </ul>
      </div>'''.format(contents)

    # It would be nice to have a fancy overlay here, that automatically appears
    # after parsing if there are errors and that automatically smoothly fades
    # out.


def render_global(*args, **kw):
    """Render the title and contents in our standard template for a global page.

    Args:
      *args: A tuple of values for the HTML template.
      *kw: A dict of optional values for the HTML template.
    Returns:
      An HTML string of the rendered template.
    """
    response.content_type = 'text/html'
    kw['A'] = A # Application mapper
    kw['V'] = V # View mapper
    kw['title'] = app.options['title']
    kw['view_title'] = ''
    kw['navigation'] = GLOBAL_NAVIGATION
    kw['scripts'] = kw.get('scripts', '')

    kw['overlay'] = (
        render_overlay('<li><a href="{}">Errors</a></li>'.format(
            app.router.build('errors')))
        if request.params.pop('render_overlay', True)
        else '')
    return template.render(*args, **kw)


@app.route('/', name='root')
def root():
    "Redirect the root page to the home page."
    bottle.redirect(app.get_url('toc'))


@app.route('/index', name='toc')
def toc():
    entries_no_open_close = [entry for entry in app.entries
                             if not isinstance(entry, (data.Open, data.Close))]
    if entries_no_open_close:
        mindate, maxdate = None, None
    else:
        mindate, maxdate = getters.get_min_max_dates(entries_no_open_close)

    def view_url(name, **kw):
        return app.router.build(name, path=DEFAULT_VIEW_REDIRECT, **kw)

    viewboxes = []
    if app.args.view:
        # Render a single link to a view, the view selected from --view.
        viewboxes.append(('selected', None,
                          [('/', 'Selected View')]))
    else:
        # Render a menu of various views.

        # Global views.
        viewboxes.append(('global', None,
                          [(view_url('all'), 'All Transactions')]))

        # By year views.
        viewboxes.append(
            ('year', 'By Year',
             [(view_url('year', year=year), 'Year {}'.format(year))
              for year in reversed(app.active_years)]))

        # By tag views.
        viewboxes.append(('tag', 'Tags',
                          [(view_url('tag', tag=tag), '#{}'.format(tag))
                           for tag in getters.get_all_tags(app.entries)]))

        # By component.
        components = getters.get_account_components(app.entries)
        viewboxes.append(
            ('component', 'Component',
             [(view_url('component', component=component), '{}'.format(component))
              for component in components]))

        # Note: With the filtering language, payees will be added and much many more
        # options. Don't worry.

    oss = io.StringIO()
    oss.write('<div id="viewboxes">\n')
    for cssid, title, viewbox in viewboxes:
        view_items = ['<li><a href="{}">{}</a></li>'.format(url, title)
                      for url, title in viewbox]
        oss.write("""
          <div id="{cssid}" class="viewbox">
            {title}
            <ul>
              {view_items}
            </ul>
          </div>
          <hr/>
        """.format(cssid=cssid,
                   title='<h2>{}</h2>'.format(title) if title else '',
                   view_items='\n'.join(view_items)))
    oss.write('</div> <!-- viewboxes -->\n')

    return render_global(
        pagetitle="Table of Contents",
        contents=oss.getvalue())


@app.route('/errors', name='errors')
def errors():
    "Report error encountered during parsing, checking and realization."
    return render_global(
        pagetitle="Errors",
        contents=render_report(misc_reports.ErrorReport, [], leaf_only=False))


@app.route('/source', name='source')
def source():
    "Render the source file, allowing scrolling at a specific line."

    contents = io.StringIO()
    if app.args.no_source:
        contents.write("Source hidden.")
    else:
        contents.write('<div id="source">')
        for i, line in enumerate(app.source.splitlines()):
            lineno = i+1
            contents.write(
                '<pre id="{lineno}">{lineno}  {line}</pre>\n'.format(
                    lineno=lineno, line=line.rstrip()))
        contents.write('</div>')

    return render_global(
        pagetitle="Source",
        contents=contents.getvalue()
        )


@app.route('/link/<link:re:.*>', name='link')
def link(link=None):
    "Serve journals for links."

    linked_entries = basicops.filter_link(link, app.entries)

    oss = io.StringIO()
    formatter = HTMLFormatter(app.options['dcontext'],
                              request.app.get_url, False, app.account_xform,
                              view_links=False)
    journal_html.html_entries_table_with_balance(oss, linked_entries, formatter)
    return render_global(
        pagetitle="Link: {}".format(link),
        contents=oss.getvalue())


@app.route('/context/<ehash:re:[a-fA-F0-9]*>', name='context')
def context_(ehash=None):
    "Render the before & after context around a transaction entry."

    matching_entries = [entry
                        for entry in app.entries
                        if ehash == compare.hash_entry(entry)]

    oss = io.StringIO()
    if len(matching_entries) == 0:
        print("ERROR: Could not find matching entry for '{}'".format(ehash),
              file=oss)

    elif len(matching_entries) > 1:
        print("ERROR: Ambiguous entries for '{}'".format(ehash),
              file=oss)
        print(file=oss)
        dcontext = app.options['dcontext']
        printer.print_entries(matching_entries, dcontext, file=oss)

    else:
        entry = matching_entries[0]

        # Render the context.
        oss.write("<pre>\n")
        oss.write(context.render_entry_context(app.entries, app.options, entry))
        oss.write("</pre>\n")

        # Render the filelinks.
        if FILELINK_PROTOCOL:
            meta = entry.meta
            uri = FILELINK_PROTOCOL.format(filename=meta.get('filename'),
                                           lineno=meta.get('lineno'))
            oss.write('<div class="filelink"><a href="{}">{}</a></div>'.format(uri, 'Open'))

    return render_global(
        pagetitle="Context: {}".format(ehash),
        contents=oss.getvalue())









GLOBAL_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{A.toc}}">Table of Contents</a></li>
  <li><a href="{{A.errors}}">Errors</a></li>
  <li><a href="{{A.source}}">Source</a></li>
</ul>
""").render(A=A)


@app.route('/resources/web.css', name='style')
def style():
    "Stylesheet for the entire document."
    response.content_type = 'text/css'
    if app.args.debug:
        with open(path.join(path.dirname(__file__), 'web.css')) as f:
            global STYLE; STYLE = f.read()
    return STYLE


@app.get('/favicon.ico')
def favicon():
    return bottle.static_file('favicon.ico', path.dirname(__file__))


@app.get('/third_party/<filename:re:.*>')
def third_party(filename=None):
    return bottle.static_file(request.path[1:], path.dirname(__file__))


doc_name = 'doc'
@app.route('/doc/<filename:re:.*>', name=doc_name)
def doc(filename=None):
    "Serve static filenames for documents directives."

    filename = '/' + filename

    # Check that there is a document directive that has this filename.
    # This is for security; we don't want to be able to serve just any file.
    for entry in misc_utils.filter_type(app.entries, data.Document):
        if entry.filename == filename:
            break
    else:
        raise bottle.HTTPError(404, "Not found")

    # Just serve the file ourselves.
    return bottle.static_file(path.basename(filename),
                              path.dirname(filename))


#--------------------------------------------------------------------------------
# View application pages.

viewapp = bottle.Bottle()

# (We disable this error because access to request.app needs to be delayed.)
# pylint: disable=unnecessary-lambda
V = bottle_utils.AttrMapper(lambda *args, **kw: request.app.get_url(*args, **kw))
M = bottle_utils.AttrMapper(lambda month: month_request(request.view.year, month))
Mp = bottle_utils.AttrMapper(lambda month: month_request(request.view.year-1, month))
Mn = bottle_utils.AttrMapper(lambda month: month_request(request.view.year+1, month))


def month_request(year, month):
    """Render a URL to a particular month of the context's request.
    """
    if year < app.active_years[0] or year > app.active_years[-1]:
        return ''
    month = list(calendar.month_abbr).index(month)
    month = "{:0>2d}".format(month)
    return app.router.build('month', year=year, month=month,
            path=request.path[1:])


def render_view(*args, **kw):
    """Render the title and contents in our standard template for a view page.

    Args:
      *args: A tuple of values for the HTML template.
      *kw: A dict of optional values for the HTML template.
    Returns:
      An HTML string of the rendered template.
    """
    response.content_type = 'text/html'
    kw['A'] = A # Application mapper
    kw['V'] = V # View mapper
    kw['title'] = app.options['title']
    kw['view_title'] = ' - ' + request.view.title

    overlays = []
    if request.params.pop('render_overlay', False):
        overlays.append(
            '<li><a href="{}">Errors</a></li>'.format(app.router.build('errors')))

    # Render navigation, with monthly navigation option.
    oss = io.StringIO()
    oss.write(APP_NAVIGATION.render(A=A, V=V, view_title=request.view.title))
    if request.view.monthly is views.MonthNavigation.COMPACT:
        overlays.append(
            '<li><a href="{}">Monthly</a></li>'.format(M.Jan))
    elif request.view.monthly is views.MonthNavigation.FULL:
        annual = app.router.build('year',
                                  path=DEFAULT_VIEW_REDIRECT,
                                  year=request.view.year)
        oss.write(APP_NAVIGATION_MONTHLY_FULL.render(M=M, Mp=Mp, Mn=Mn, V=V, annual=annual))
    kw['navigation'] = oss.getvalue()
    kw['overlay'] = render_overlay(' '.join(overlays))

    kw['scripts'] = kw.get('scripts', '')

    return template.render(*args, **kw)


APP_NAVIGATION = bottle.SimpleTemplate("""
<ul id="nav-main">
  <li><a href="{{A.toc}}">Table of Contents</a></li>
  <li><span class="ledger-name">{{view_title}}:</span></li>
  <li><a href="{{V.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{V.income}}">Income Statement</a></li>
  <li><a href="{{V.holdings}}">Equity/Holdings</a></li>
  <li><a href="{{V.trial}}">Trial Balance</a></li>
  <li><a href="{{V.journal_all}}">General Journal</a></li>
  <li><a href="{{V.index}}">Index</a></li>
</ul>
""")

APP_NAVIGATION_MONTHLY_FULL = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{annual}}">Annual</a></li>
  [
  <li><a href="{{Mp.Dec}}">Prev December</a></li> &laquo;
  <li><a href="{{M.Jan}}">January</a></li>
  <li><a href="{{M.Feb}}">February</a></li>
  <li><a href="{{M.Mar}}">March</a></li>
  <li><a href="{{M.Apr}}">April</a></li>
  <li><a href="{{M.May}}">May</a></li>
  <li><a href="{{M.Jun}}">June</a></li>
  <li><a href="{{M.Jul}}">July</a></li>
  <li><a href="{{M.Aug}}">August</a></li>
  <li><a href="{{M.Sep}}">September</a></li>
  <li><a href="{{M.Oct}}">October</a></li>
  <li><a href="{{M.Nov}}">November</a></li>
  <li><a href="{{M.Dec}}">December</a></li> &raquo;
  <li><a href="{{Mn.Jan}}">Next January</a></li>
  ]
</ul>
""")

@viewapp.route('/', name='approot')
def approot():
    bottle.redirect(request.app.get_url(DEFAULT_VIEW_REDIRECT))


@viewapp.route('/index', name='index')
def index():
    "Index of all pages, so that navigation need not have all links."

    oss = io.StringIO()
    oss.write('<ul>\n')
    for title, page in [
            ("Balances", "trial"),
            ("Balance Sheet", "balsheet"),
            ("Opening Balances", "openbal"),
            ("Income Statement", "income"),
            ("General Journal", "journal_all"),
            ("Conversions", "conversions"),
            ("Documents", "documents"),
            ("Holdings (Full Detail)", "holdings"),
            ("Holdings by Account", "holdings_byaccount"),
            ("Holdings by Root Account", "holdings_byrootaccount"),
            ("Holdings by Commodity", "holdings_bycommodity"),
            ("Holdings by Currency", "holdings_bycurrency"),
            ("Net Worth", "networth"),
            ("Commodities", "commodities"),
            ("Events", "event_index"),
            ("Activity/Update", "activity"),
            ("Statistics (Types)", "stats_types"),
            ("Statistics (Postings)", "stats_postings"),
    ]:
        oss.write('<li><a href={}>{}</a></li>\n'.format(
            request.app.get_url(page), title))
    oss.write('</ul>\n')

    return render_view(
        pagetitle="Index",
        contents=oss.getvalue())


@viewapp.route('/trial', name='trial')
def trial():
    "Trial balance / Chart of Accounts."
    return render_view(
        pagetitle="Trial Balance",
        contents=render_real_report(balance_reports.BalancesReport,
                                    request.view.real_accounts,
                                    app.price_map,
                                    request.view.price_date,
                                    leaf_only=True))


@viewapp.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."
    return render_view(pagetitle="Balance Sheet",
                       contents=render_real_report(balance_reports.BalanceSheetReport,
                                                   request.view.closing_real_accounts,
                                                   app.price_map,
                                                   request.view.price_date,
                                                   leaf_only=True))


@viewapp.route('/openbal', name='openbal')
def openbal():
    "Opening balances."
    return render_view(pagetitle="Opening Balances",
                       contents=render_real_report(balance_reports.BalanceSheetReport,
                                                   request.view.opening_real_accounts,
                                                   app.price_map,
                                                   request.view.price_date,
                                                   leaf_only=True))


@viewapp.route('/income', name='income')
def income():
    "Income statement."
    return render_view(pagetitle="Income Statement",
                       contents=render_real_report(balance_reports.IncomeStatementReport,
                                                   request.view.real_accounts,
                                                   app.price_map,
                                                   request.view.price_date,
                                                   leaf_only=True))


@viewapp.route('/equity/holdings', name='holdings')
def holdings_():
    "Render a detailed table of all holdings."
    html_table = render_report(holdings_reports.HoldingsReport, request.view.entries,
                               [],
                               css_class='holdings detail-table sortable', center=True)
    return render_view(
        pagetitle="Holdings",
        contents=html_table,
        scripts='<script src="/third_party/sorttable.js"></script>')

@viewapp.route('/equity/holdings_byaccount', name='holdings_byaccount')
def holdings_byaccount():
    "Render a table of holdings by account."

    html_table = render_report(holdings_reports.HoldingsReport, request.view.entries,
                               ['--by', 'account'],
                               css_class='holdings detail-table sortable', center=True)
    return render_view(
        pagetitle="Holdings by Account",
        contents=html_table,
        scripts='<script src="/third_party/sorttable.js"></script>')

@viewapp.route('/equity/holdings_byrootaccount', name='holdings_byrootaccount')
def holdings_byrootaccount():
    "Render a table of holdings by account."

    html_table = render_report(holdings_reports.HoldingsReport, request.view.entries,
                               ['--by', 'root-account'],
                               css_class='holdings detail-table sortable', center=True)
    return render_view(
        pagetitle="Holdings by Account",
        contents=html_table,
        scripts='<script src="/third_party/sorttable.js"></script>')

@viewapp.route('/equity/holdings_bycommodity', name='holdings_bycommodity')
def holdings_bycommodity():
    "Render a table of holdings by commodity."

    html_table = render_report(holdings_reports.HoldingsReport, request.view.entries,
                               ['--by', 'commodity'],
                               css_class='holdings detail-table sortable', center=True)
    return render_view(
        pagetitle="Holdings by Commodity",
        contents=html_table,
        scripts='<script src="/third_party/sorttable.js"></script>')

@viewapp.route('/equity/holdings_bycurrency', name='holdings_bycurrency')
def holdings_bycurrency():
    "Render a table of holdings by currency."

    html_table = render_report(holdings_reports.HoldingsReport, request.view.entries,
                               ['--by', 'currency'],
                               css_class='holdings detail-table sortable', center=True)
    return render_view(
        pagetitle="Holdings by Currency",
        contents=html_table,
        scripts='<script src="/third_party/sorttable.js"></script>')

@viewapp.route('/equity/networth', name='networth')
def networth():
    "Render a table of the net worth for this filter."

    html_table = render_report(holdings_reports.NetWorthReport, request.view.entries)
    return render_view(
        pagetitle="Net Worth",
        contents=html_table)



@viewapp.route('/journal/all', name='journal_all')
def journal_all():
    "A list of all the entries in this realization."
    bottle.redirect(request.app.get_url('journal', account_name=''))


@viewapp.route('/journal/<account_name:re:.*>', name='journal')
def journal_(account_name=None):
    "A list of all the entries for this account realization."
    account_name = app.account_xform.parse(account_name)

    # Figure out which account to render this from.
    real_accounts = request.view.real_accounts
    if account_name:
        if account_name and account_types.is_balance_sheet_account(account_name,
                                                                   app.account_types):
            real_accounts = request.view.closing_real_accounts

    # Render the report.
    args = []
    if account_name:
        args.append('--account={}'.format(account_name))

    render_postings = request.params.get('postings', True)
    if isinstance(render_postings, str):
        render_postings = render_postings.lower() in ('1', 'true')
    if render_postings:
        args.append('--verbose')

    try:
        html_journal = render_real_report(journal_reports.JournalReport,
                                          real_accounts,
                                          app.price_map,
                                          request.view.price_date,
                                          args, leaf_only=False)
    except KeyError as e:
        raise bottle.HTTPError(404, '{}'.format(e))

    return render_view(pagetitle='{}'.format(account_name or
                                             'General Ledger (All Accounts)'),
                       contents=html_journal)


@viewapp.route('/conversions', name='conversions')
def conversions_():
    "Render the list of transactions with conversions."
    return render_view(
        pagetitle="Conversions",
        contents=render_report(journal_reports.ConversionsReport,
                               request.view.entries, leaf_only=False))


# Note: these redirects are necessary to let the view router create global links
# from routines that work from a single build_url() argument being passed in. We
# can just pass in the view's request.app.get_url() function and it can call
# this with 'doc' or 'link' and a redirect moves it to the corresponding global
# page.
@viewapp.route('/doc/<filename:re:.*>', name=doc_name)
def doc__view(filename=None):
    # Redirect to global page.
    bottle.redirect(app.router.build('doc', filename=filename))

@viewapp.route('/documents', name='documents')
def documents():
    "Render a tree with all the documents found."
    return render_view(
        pagetitle="Documents",
        contents=render_report(journal_reports.DocumentsReport,
                               request.view.entries, leaf_only=False))


@viewapp.route('/prices'
               r'/<base:re:[A-Z][A-Z0-9\'\.\_\-]{0,22}[A-Z0-9]>'
               r'/<quote:re:[A-Z][A-Z0-9\'\.\_\-]{0,22}[A-Z0-9]>', name='prices')
def prices_values(base=None, quote=None):
    "Render all the values for a particular price pair."
    html_table = render_report(price_reports.CommodityPricesReport, request.view.entries,
                               ['--commodity', '{}/{}'.format(base, quote)],
                               css_id='price-index')
    return render_view(
        pagetitle="Price: {} / {}".format(base, quote),
        contents=html_table)


@viewapp.route('/commodities', name='commodities')
def commodities():
    "Render a list commodities with list their prices page."
    html_table = render_report(price_reports.CommoditiesReport, request.view.entries,
                               [],
                               css_id='price-index')
    return render_view(
        pagetitle="Commodities",
        contents=html_table)


@viewapp.route(r'/event/<event:re:([A-Za-z0-9\-_/.]+)?>', name='event')
def event(event=None):
    "Render all values of a particular event."
    if not event:
        bottle.redirect(app.get_url('event_index'))
    return render_view(
        pagetitle="Event: {}".format(event),
        contents=render_report(misc_reports.EventsReport, app.entries,
                               ['--expr', event]))


@viewapp.route('/event', name='event_index')
def event_index():
    "Render the latest values of all events and an index."
    return render_view(
        pagetitle="Events Index",
        contents=render_report(misc_reports.CurrentEventsReport,
                               request.view.entries))


@viewapp.route('/activity', name='activity')
def activity():
    "Render the update activity."
    return render_global(
        pagetitle="Update Activity",
        contents=render_real_report(misc_reports.ActivityReport,
                                    request.view.real_accounts,
                                    app.price_map,
                                    request.view.price_date,
                                    leaf_only=False))

@viewapp.route('/stats_types', name='stats_types')
def stats_types():
    "Compute and render statistics about the input file."
    return render_view(
        pagetitle="Directives Statistics",
        contents=render_report(misc_reports.StatsDirectivesReport,
                               request.view.entries))

@viewapp.route('/stats_postings', name='stats_postings')
def stats_postings():
    "Compute and render statistics about the input file."
    return render_view(
        pagetitle="Postings Statistics",
        contents=render_report(misc_reports.StatsPostingsReport,
                               request.view.entries))


#--------------------------------------------------------------------------------
# Views.


# A cache for views that have been created (on access).
app.views = {}


def handle_view(path_depth):
    """A decorator for handlers which create views lazily.
    If you decorate a method with this, the wrapper does the redirect
    handling and your method is just a factory for a View instance,
    which is cached.

    Args:
      path_depth: An integer, the number of components that form the view id.
    Returns:
      A decorator function, used to wrap view handlers.
    """
    def view_populator(callback):
        def wrapper(*args, **kwargs):
            components = request.path.split('/')
            viewid = '/'.join(components[:path_depth+1])
            try:
                # Try fetching the view from the cache.
                view = app.views[viewid]
            except KeyError:
                # We need to create the view.
                view = app.views[viewid] = callback(*args, **kwargs)

            # Save the view for the subrequest and redirect. populate_view()
            # picks this up and saves it in request.view.
            request.environ['VIEW'] = view
            return bottle_utils.internal_redirect(viewapp, path_depth)
        return wrapper
    return view_populator


def populate_view(callback):
    """A plugin that will populate the request with the current view instance.

    Args:
      callback: A continuation function to call to handle the request.
    Returns:
      A function to call to install view-specific parameters on the request.
    """
    def wrapper(*args, **kwargs):
        request.view = request.environ['VIEW']
        return callback(*args, **kwargs)
    return wrapper

viewapp.install(populate_view)


def url_restrict_generator(url_prefix):
    """Restrict to only a single prefix.

    Args:
      url_prefix: A string, a URL prefix to restrict to.
      callback: The function to wrap.
    Returns:
      A handler decorator.
    """
    # A list of URLs that should always be accepted, even when restricted.
    allowed_regexps = [re.compile(regexp).match
                       for regexp in ['/resources',
                                      '/favicon.ico',
                                      '/index',
                                      '/errors',
                                      '/source',
                                      '/context',
                                      '/third_party']]

    def url_restrict_handler(callback):
        def wrapper(*args, **kwargs):
            if (any(match(request.path) for match in allowed_regexps) or
                request.path.startswith(url_prefix)):
                return callback(*args, **kwargs)
            if request.path == '/':
                bottle.redirect(url_prefix)

            # Note: we issue a "202 Accepted" status in order to satisfy bean-bake,
            # we want to distinguish between an actual 404 error and this case.
            raise bottle.HTTPError(202, "URLs restricted to '{}'".format(url_prefix))

        return wrapper
    return url_restrict_handler


def get_all_view(app):
    """Return a view of all transactions.

    Returns:
      An instance of AllView, that covers all transactions.
    """
    return views.AllView(app.entries, app.options, 'All Transactions')


@app.route(r'/view/all/<path:re:.*>', name='all')
@handle_view(2)
def all(path=None):
    return get_all_view(app)

@app.route(r'/view/year/<year:re:\d\d\d\d>/month/<month:re:\d\d>/<path:re:.*>',
           name='month')
@handle_view(5)
def month(year=None, month=None, path=None):
    year = int(year)
    month = int(month)
    date = datetime.date(year, month, 1)
    text = date.strftime('%B %Y')
    return views.MonthView(app.entries, app.options, text, year, month)

@app.route(r'/view/year/<year:re:\d\d\d\d>/<path:re:.*>', name='year')
@handle_view(3)
def year(year=None, path=None):
    year = int(year)
    first_month = app.args.first_month
    return views.YearView(app.entries, app.options, 'Year {:4d}'.format(year),
                          year, first_month)

@app.route(r'/view/tag/<tag:re:[^/]*>/<path:re:.*>', name='tag')
@handle_view(3)
def tag(tag=None, path=None):
    return views.TagView(app.entries, app.options, 'Tag {}'.format(tag), set([tag]))


@app.route(r'/view/payee/<payee:re:[^/]*>/<path:re:.*>', name='payee')
@handle_view(3)
def payee(payee=None, path=None):
    return views.PayeeView(app.entries, app.options, 'Payee {}'.format(payee), payee)

@app.route(r'/view/component/<component:re:[^/]*>/<path:re:.*>', name='component')
@handle_view(3)
def component(component=None, path=None):
    return views.ComponentView(app.entries, app.options,
                               'Component: {}'.format(component), component)


#--------------------------------------------------------------------------------
# Bootstrapping and main program.


def auto_reload_input_file(callback):
    """A plugin that automatically reloads the input file if it changed since the
    last page was loaded."""
    def wrapper(*posargs, **kwargs):
        filename = app.args.filename

        if loader.needs_refresh(app.options):
            logging.info('Reloading...')

            # Save the source for later, to render.
            with open(filename, encoding='utf8') as f:
                app.source = f.read()

            # Parse the beancount file.
            entries, errors, options_map = loader.load_file(filename)

            # Print out the list of errors.
            if errors:
                # pylint: disable=unsupported-assignment-operation
                request.params['render_overlay'] = True
                print(',----------------------------------------------------------------')
                printer.print_errors(errors, file=sys.stdout)
                print('`----------------------------------------------------------------')

            # Save globals in the global app.
            app.entries = entries
            app.errors = errors
            app.options = options_map
            app.account_types = options.get_account_types(options_map)

            # Pre-compute the price database.
            app.price_map = prices.build_price_map(entries)

            # Pre-compute the list of active years.
            app.active_years = list(getters.get_active_years(entries))

            # Reset the view cache.
            app.views.clear()

        else:
            # For now, the overlay is a link to the errors page. Always render
            # it on the right when there are errors.
            if app.errors:
                # pylint: disable=unsupported-assignment-operation
                request.params['render_overlay'] = True

        return callback(*posargs, **kwargs)
    return wrapper

app.install(auto_reload_input_file)


def incognito(callback):
    """A plugin that converts all numbers rendered into X's, in order
    to hide the actual values in the ledger. This is used for doing
    public demos using my real ledger, where I don't necessarily
    want to share the detail of my financial life with the viewers
    but when I still want an interesting ledger, with enough
    detail that looks realistic."""

    def wrapper(*posargs, **kwargs):
        contents = callback(*posargs, **kwargs)
        if (response.content_type in ('text/html', '') and
            isinstance(contents, str)):
            contents = text_utils.replace_numbers(contents)
        return contents

    return wrapper


# Global template.
template = None

def run_app(args, quiet=None):
    logging.basicConfig(level=logging.INFO,
                        format='%(levelname)-8s: %(message)s')

    app_installs = []
    view_installs = []

    # Hide the numbers in incognito mode. We do this on response text via a plug-in.
    if args.incognito:
        args.no_source = True
        app.install(incognito)
        app_installs.append(incognito)
        viewapp.install(incognito)
        view_installs.append(incognito)

    # Install code that will restrict all resources to a particular view.
    if args.view:
        view_url_prefix = '/view/{}/'.format(args.view)
        url_restrictor = url_restrict_generator(view_url_prefix)
        app.install(url_restrictor)
        app_installs.append(url_restrictor)

    app.options = None

    # Add an account transformer.
    app.account_xform = account.AccountTransformer('__' if args.no_colons else None)

    # Load templates.
    with open(path.join(path.dirname(__file__), 'web.html')) as f:
        global template
        template = bottle.SimpleTemplate(f)

    with open(path.join(path.dirname(__file__), 'web.css')) as f:
        global STYLE; STYLE = f.read()

    # Run the server.
    app.args = args
    bind_address = '0.0.0.0' if args.public else 'localhost'
    app.run(host=bind_address, port=args.port,
            debug=args.debug, reloader=False,
            quiet=args.quiet if hasattr(args, 'quiet') else quiet)

    # Uninstall applications.
    for function in app_installs:
        app.uninstall(function)
    for function in view_installs:
        viewapp.uninstall(function)


# The global server instance.
server = None

def setup_monkey_patch_for_server_shutdown():
    """Setup globals to steal access to the server reference.
    This is required to initiate shutdown, unfortunately.
    (Bottle could easily remedy that.)"""

    # Save the original function.
    # pylint: disable=import-outside-toplevel
    from wsgiref.simple_server import make_server

    # Create a decorator that will save the server upon start.
    def stealing_make_server(*args, **kw):
        global server
        server = make_server(*args, **kw)
        return server

    # Patch up wsgiref itself with the decorated function.
    import wsgiref.simple_server
    wsgiref.simple_server.make_server = stealing_make_server

setup_monkey_patch_for_server_shutdown()

def wait_ready():
    """Wait until the 'server' global has been set.
    This tells us the server is running.
    """
    while server is None:
        time.sleep(0.05)

def shutdown():
    """Request for the server to shutdown."""
    server.shutdown()


def add_web_arguments(argparser):
    group = argparser.add_argument_group("Web server arguments")

    group.add_argument('filename',
                       help="Beancount input filename to serve.")

    group.add_argument('--port', action='store', type=int, default=8080,
                       help="Which port to listen on.")

    group.add_argument('-q', '--quiet', action='store_true',
                       help="Don't even print out web server log")

    group.add_argument('--debug', action='store_true',
                       help="Enable debugging features (auto-reloading of css).")

    group.add_argument('--incognito', action='store_true',
                       help=("Filter the output in order to hide all the numbers. "
                             "This is great for demos using my real file."))

    group.add_argument('--no-source', action='store_true',
                       help=("Don't render the source."))

    group.add_argument('--no-colons', action='store_true',
                       help=("Don't render colons in filenames (for Windows)."))

    group.add_argument('--view', action='store',
                       help="Render only the specified view (identify by URL)")

    group.add_argument('--public', '--inaddr-any', action='store_true',
                       help="Bind server to listen to any address, not just localhost.")

    group.add_argument('--first-month', action='store', type=int, default=1,
                       help="The first month of the calendar year.")

    return group


def main():
    """Main web service runner. This runs the event loop and blocks indefinitely."""

    argparser = version.ArgumentParser(description=__doc__.strip())
    add_web_arguments(argparser)
    args = argparser.parse_args()

    run_app(args)


def scrape_webapp(webargs, callback, ignore_regexp):
    """Run a web server on a Beancount file and scrape it.

    This is the main entry point of this module.

    Args:
      webargs: An argparse.Namespace container of the arguments provided in
        web.add_web_arguments().
      callback: A callback function to invoke on each page to validate it.
        The function is called with the response and the url as arguments.
        This function should trigger an error on failure (via an exception).
      ignore_regexp: A regular expression string, the urls to ignore.
    Returns:
      A set of all the processed URLs and a set of all the skipped URLs.
    """
    url_format = 'http://localhost:{}{{}}'.format(webargs.port)

    thread = thread_server_start(webargs)

    # Skips:
    # - Docs cannot be read for external files.
    #
    # - Components views... well there are just too many, makes the tests
    #   impossibly slow. Just keep the A's so some are covered.
    url_lists = scrape.scrape_urls(url_format, callback, ignore_regexp)

    thread_server_shutdown(thread)

    return url_lists


def thread_server_start(web_args, **kwargs):
    """Start a server in a new thread.

    Args:
      argparse_args: An argparse parsed options object, with all the options
        from add_web_arguments().
    Returns:
      A new Thread instance.

    """
    thread = threading.Thread(
        target=run_app,
        args=(web_args,),
        kwargs=kwargs)
    thread.daemon = True # Automatically exit if the process comes dwn.
    thread.start()

    # Ensure the server has at least started before running the scraper.
    wait_ready()
    time.sleep(0.1)

    return thread


def thread_server_shutdown(thread):
    """Shutdown the server running in the given thread.

    Unfortunately, in the meantime this has a side-effect on all servers.
    This returns after waiting that the thread has stopped.

    Args:
      thread: A threading.Thread instance.
    """
    # Clean shutdown: request to stop, then join the thread.
    # Note that because we daemonize, we could forego this elegant detail.
    shutdown()
    thread.join()
