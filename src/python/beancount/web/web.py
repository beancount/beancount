"""
Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
import argparse
import datetime
from os import path
import io
import logging
import time
import threading

import bottle
from bottle import response, request

from beancount.core.data import Open, Close, Pad, Balance, Transaction, Note, Document, Event
from beancount.core.data import Posting
from beancount.core import data
from beancount.core import flags
from beancount.core import getters
from beancount.core import realization
from beancount.core import account
from beancount.core import account_types
from beancount.ops import basicops
from beancount.ops import summarize
from beancount.ops import prices
from beancount.ops import holdings
from beancount.utils import misc_utils
from beancount.utils.text_utils import replace_numbers
from beancount.web.bottle_utils import AttrMapper, internal_redirect
from beancount.parser import options
from beancount import loader
from beancount.web import views
from beancount.web import journal
from beancount.web import acctree
from beancount.web import gviz
from beancount.reports import table


#--------------------------------------------------------------------------------
# Global application pages.


NOT_IMPLEMENTED = '''
<p style="color:red; font-weight: bold; font-style: italic;">
  (Feature not implemented yet.)
</p>
'''

app = bottle.Bottle()
A = AttrMapper(app.router.build)


def render_errors(errors_list):
    if errors_list:
        errors_str = "<div id='errors'>{}</div>".format("\n".join(errors_list))
    else:
        errors_str = ""
    return errors_str

def render_global(*args, **kw):
    """Render the title and contents in our standard template."""
    response.content_type = 'text/html'
    kw['A'] = A # Application mapper
    kw['V'] = V # View mapper
    kw['title'] = app.options['title']
    kw['view_title'] = ''
    kw['navigation'] = GLOBAL_NAVIGATION
    kw['scripts'] = kw.get('scripts', '')
    kw['errors_str'] = render_errors(kw.pop('errors', None))
    return template.render(*args, **kw)


@app.route('/', name='root')
def root():
    "Redirect the root page to the home page."
    bottle.redirect(app.get_url('toc'))


@app.route('/toc', name='toc')
def toc():
    entries_no_open_close = [entry for entry in app.entries
                             if not isinstance(entry, (Open, Close))]
    if entries_no_open_close:
        mindate, maxdate = None, None
    else:
        mindate, maxdate = getters.get_min_max_dates(entries_no_open_close)

    def view_url(name, **kw):
        return app.router.build(name, path='', **kw)

    viewboxes = []

    # Global views.
    viewboxes.append(('global', None,
                      [(view_url('all'), 'All Transactions')]))

    # By year views.
    viewboxes.append(('year', 'By Year',
                      [(view_url('year', year=year), 'Year {}'.format(year))
                       for year in reversed(list(getters.get_active_years(app.entries)))]))

    # By tag views.
    viewboxes.append(('tag', 'Tags',
                      [(view_url('tag', tag=tag), '#{}'.format(tag))
                       for tag in getters.get_all_tags(app.entries)]))

    # FIXME: These are not implemented yet.
    if 0:
        # By level.
        all_accounts_names = getters.get_accounts(app.entries).keys()
        viewboxes.append(
            ('level1', 'Level 1',
             [(view_url('level1', level=level), '{}'.format(level))
              for level in getters.get_leveln_parent_accounts(all_accounts_names, 1, nrepeats=0)]))

        viewboxes.append(
            ('level2', 'Level 2',
             [(view_url('level2', level=level), '{}'.format(level))
              for level in getters.get_leveln_parent_accounts(all_accounts_names, 2, nrepeats=0)]))

    # FIXME: This deserves its own page, with options for cleanup (or a helper tool).
    if 0:
        # By payee views.
        viewboxes.append(('payee', 'Payees',
                          [(view_url('payee', payee=payee), '{}'.format(payee))
                           for payee in sorted(getters.get_all_payees(app.entries))]))

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
        pagetitle = "Table of Contents",
        contents = oss.getvalue())


@app.route('/errors', name='errors')
def errors():
    "Report error encountered during parsing, checking and realization."
    return render_global(
        pagetitle = "Errors",
        contents = NOT_IMPLEMENTED
        )


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
                '<pre id="{}">{}  {}</pre>\n'.format(
                    lineno, lineno, line.rstrip()))
        contents.write('</div>')

    return render_global(
        pagetitle = "Source",
        contents = contents.getvalue()
        )


@app.route('/update', name='update')
def update():
    "Render the update activity."

    errors = []
    cutoff = None
    try:
        if 'cutoff' in request.params:
            cutoff = datetime.datetime.strptime(request.params['cutoff'], '%Y-%m-%d').date()
    except ValueError:
        errors.append("Invalid cutoff date: {}".format(request.params['cutoff']))

    oss = io.StringIO()
    view = get_all_view(app)

    for root in (app.account_types.assets,
                 app.account_types.liabilities):
        table = acctree.tree_table(oss, view.real_accounts, root,
                                   ['Account', 'Last Entry'],
                                   leafonly=False)
        for real_account, cells, row_classes in table:
            if not isinstance(real_account, realization.RealAccount):
                continue
            last_posting = find_last_active_posting(real_account.postings)

            # Don't render updates to accounts that have been closed.
            # Note: this is O(N), maybe we can store this at realization.
            if last_posting is None or isinstance(last_posting, Close):
                continue

            last_date = data.get_entry(last_posting).date

            # Skip this posting if a cutoff date has been specified and the
            # account has been updated to at least the cutoff date.s
            if cutoff and cutoff <= last_date:
                continue

            # Okay, we need to render this. Append.
            cells.append(data.get_entry(last_posting).date
                         if real_account.postings
                         else '-')

    return render_global(
        pagetitle = "Update Activity",
        contents = oss.getvalue(),
        errors = errors
        )

def find_last_active_posting(postings):
    """Look at the end of the list of postings, and find the last
    posting or entry that is not an automatically added directive.
    Note that if the account is closed, the last posting is assumed
    to be a close directive (this is the case if the input is valid
    and checks without errors.

    Args:
      postings: a list of postings or entries.
    Returns:
      An entry, or None, if the input list was empty.
    """
    for posting in misc_utils.filter_type(reversed(postings),
                                          (Open, Close, Pad, Balance, Posting, Note)):
        if (isinstance(posting, Posting) and
            posting.entry.flag == flags.FLAG_UNREALIZED):
            continue
        return posting
    else:
        return None


@app.route('/events', name='events')
def events():
    "Render an index for the various kinds of events."

    events = misc_utils.filter_type(app.entries, Event)
    events_by_type = misc_utils.groupby(lambda event: event.type, events)

    contents = io.StringIO()
    for event_type, events in sorted(events_by_type.items()):
        contents.write(event_type)
        contents.write(str(len(events)))

    return render_global(
        pagetitle = "Events",
        ##contents = contents.getvalue() # FIXME: TODO
        contents = NOT_IMPLEMENTED
        )


@app.route('/prices', name='prices')
def prices_():
    "Render a list of links to instruments, to list their prices."

    oss = io.StringIO()
    for quote, baselist in sorted(misc_utils.groupby(lambda x: x[1], app.price_map.keys()).items(),
                                  key=lambda x: -len(x[1])):
        links = ['<a href="{link}">{0} ({1})</a>'.format(
            base_, quote_,
            link=request.app.get_url('prices_values', base=base_, quote=quote_)
        ) for base_, quote_ in sorted(baselist)]

        oss.write("""
          <td>
            <ul>
              {}
            </ul>
          </td>
        """.format('\n'.join(map('<li>{}</li>'.format, links))))

    return render_global(
        pagetitle = "Prices",
        contents = """
          <table id="price-index">
            <tr>
            {}
            </tr>
          </table>
        """.format(oss.getvalue()))


@app.route('/prices/<base:re:[A-Z0-9._\']+>_<quote:re:[A-Z0-9._\']+>', name='prices_values')
def prices_values(base=None, quote=None):
    date_rates = prices.get_all_prices(app.price_map, (base, quote))
    dates, rates = zip(*date_rates)

    scripts = gviz.gviz_timeline(dates, {'rates': rates, 'rates2': rates})

    return render_global(
        pagetitle = "Price: {} / {}".format(base, quote),
        scripts = """
          <script src="//www.google.com/jsapi" type="text/javascript"></script>
          <script type="text/javascript">
            {}
          </script>
        """.format(scripts),
        contents = """
           <div id="chart" style="height: 800px"></div>
           <div id="price-table">
             <table id="prices">
               <thead>
                 <tr><td>Date</td><td>Price</td></tr>
               </thead>
               {}
             </table>
           </div>
        """.format("\n".join("<tr><td>{}</td><td>{}</td></tr>".format(date, rate)
                             for (date, rate) in zip(dates, rates))))



@app.route('/link/<link:re:.*>', name='link')
def link(link=None):
    "Serve journals for links."

    linked_entries = basicops.filter_link(link, app.entries)

    oss = io.StringIO()
    journal.entries_table_with_balance(app, oss, linked_entries)
    return render_global(
        pagetitle = "Link: {}".format(link),
        contents = oss.getvalue())











GLOBAL_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{A.toc}}">Table of Contents</a></li>
  <li><a href="{{A.errors}}">Errors</a></li>
  <li><a href="{{A.source}}">Source</a></li>
  <li><a href="{{A.update}}">Update Activity</a></li>
  <li><a href="{{A.events}}">Events</a></li>
  <li><a href="{{A.prices}}">Prices</a></li>
</ul>
""").render(A=A)


@app.route('/web.css', name='style')
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
    for entry in misc_utils.filter_type(app.entries, Document):
        if entry.filename == filename:
            break
    else:
        raise bottle.HTTPError(404, "Not found.")

    # Just serve the file ourselves.
    return bottle.static_file(path.basename(filename),
                              path.dirname(filename))


#--------------------------------------------------------------------------------
# View application pages.

viewapp = bottle.Bottle()
V = AttrMapper(lambda *args, **kw: request.app.get_url(*args, **kw))


def render_view(*args, **kw):
    """Render the title and contents in our standard template."""
    response.content_type = 'text/html'
    kw['A'] = A # Application mapper
    kw['V'] = V # View mapper
    kw['title'] = app.options['title']
    kw['view_title'] = ' - ' + request.view.title
    kw['navigation'] = APP_NAVIGATION.render(A=A, V=V, view_title=request.view.title)
    kw['scripts'] = kw.get('scripts', '')
    kw['errors_str'] = render_errors(kw.pop('errors', None))
    return template.render(*args, **kw)

APP_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{A.toc}}">Table of Contents</a></li>
  <li><span class="ledger-name">{{view_title}}:</span></li>
  <li><a href="{{V.openbal}}">Opening Balances</a></li>
  <li><a href="{{V.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{V.income}}">Income Statement</a></li>
  <li><a href="{{V.equity}}">Shareholder's Equity</a></li>
  <li><a href="{{V.trial}}">Trial Balance</a></li>
  <li><a href="{{V.journal}}">Journal</a></li>
  <li><a href="{{V.holdings}}">Holdings</a></li>
  <li><a href="{{V.conversions}}">Conversions</a></li>
  <li><a href="{{V.documents}}">Documents</a></li>
  <li><a href="{{V.stats}}">Statistics</a></li>
</ul>
""")


@viewapp.route('/', name='approot')
def approot():
    bottle.redirect(request.app.get_url('balsheet'))


@viewapp.route('/trial', name='trial')
def trial():
    "Trial balance / Chart of Accounts."

    view = request.view
    real_accounts = view.real_accounts
    operating_currencies = view.options['operating_currency']
    table = acctree.table_of_balances(real_accounts, '', operating_currencies, classes=['trial'])


    ## FIXME: After conversions is fixed, this should always be zero.
    total_balance = summarize.compute_total_balance(view.entries)
    table += """
      Total Balance: <span class="num">{}</span>
    """.format(total_balance.get_cost())

    return render_view(
        pagetitle = "Trial Balance",
        contents = table
        )


def balance_sheet_table(real_accounts, options):
    """Render an HTML balance sheet of the real_accounts tree."""

    operating_currencies = options['operating_currency']
    assets      = acctree.table_of_balances(real_accounts, options['name_assets'],
                                            operating_currencies)
    liabilities = acctree.table_of_balances(real_accounts, options['name_liabilities'],
                                            operating_currencies)
    equity      = acctree.table_of_balances(real_accounts, options['name_equity'],
                                            operating_currencies)

    return """
           <div class="halfleft">

             <div id="assets">
              <h3>Assets</h3>
              {assets}
             </div>

           </div>
           <div class="halfright">

             <div id="liabilities">
              <h3>Liabilities</h3>
              {liabilities}
             </div>
             <div class="spacer">
             </div>
             <div id="equity">
              <h3>Equity</h3>
              {equity}
             </div>

           </div>
        """.format(**vars())


@viewapp.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."

    view = request.view
    real_accounts = request.view.closing_real_accounts
    contents = balance_sheet_table(real_accounts, view.options)

    return render_view(pagetitle = "Balance Sheet",
                      contents = contents)


@viewapp.route('/openbal', name='openbal')
def openbal():
    "Opening balances."

    view = request.view
    real_accounts = request.view.opening_real_accounts
    if real_accounts is None:
        contents = 'N/A'
    else:
        contents = balance_sheet_table(real_accounts, view.options)

    return render_view(pagetitle = "Opening Balances",
                      contents = contents)


@viewapp.route('/income', name='income')
def income():
    "Income statement."

    view = request.view
    real_accounts = request.view.real_accounts

    # Render the income statement tables.
    operating_currencies = view.options['operating_currency']
    income   = acctree.table_of_balances(real_accounts, view.options['name_income'],
                                         operating_currencies)
    expenses = acctree.table_of_balances(real_accounts, view.options['name_expenses'],
                                         operating_currencies)

    contents = """
       <div id="income" class="halfleft">

         <div id="income">
          <h3>Income</h3>
          {income}
         </div>

       </div>
       <div class="halfright">

         <div id="expenses">
          <h3>Expenses</h3>
          {expenses}
         </div>

       </div>
    """.format(**vars())

    return render_view(pagetitle = "Income Statement",
                      contents = contents)


@viewapp.route('/equity', name='equity')
def equity():
    "Render a table of the net worth at the beginning, end, and net income."

    if 0:
        view = request.view

        balance = summarize.compute_balance_for_prefix(view.closing_entries,
                                                       '{}:'.format(view.options['name_equity']))
        header = io.StringIO()
        header.write('<th>Currency</th>\n')
        header.write('<th>Amount</th>\n')
        operating_currencies = view.options['operating_currency']
        header.write('\n'.join('<th>{}</th>\n'.format(currency)
                               for currency in operating_currencies))

        body = io.StringIO()
        for position in balance.get_positions():
            body.write('<tr>')
            body.write('<td>{}</td>'.format(position.lot.currency))
            body.write('<td>{}</td>'.format(position.number))
            for currency in operating_currencies:
              date_, rate = prices.get_latest_price(app.price_map,
                                                    (position.lot.currency, currency))
              value = position.number * rate

              # FIXME: We may not have an appropriate conversion here, we may need
              # to get the cost and then convert the cost to the target currency. Do
              # this.
              body.write('<td>{}</td>'.format(value))
            body.write('</tr>')

        contents = """
           <div id="equity-table">
             <table>
               <thead>
                 {header}
               </thead>
               <tbody>
                 {body}
               </tbody>
             </table>
           </div>
        """.format(header=header.getvalue(), body=body.getvalue())

        ## FIXME: Render the equity at opening too.
        ## FIXME: Insert a summary of the net income.

    return render_view(pagetitle = "Shareholder's Equity",
                       contents = NOT_IMPLEMENTED)


@viewapp.route('/journal', name='journal')
def journal_():
    "A list of all the entries in this realization."
    bottle.redirect(request.app.get_url('account', slashed_account_name=''))


@viewapp.route('/account/<slashed_account_name:re:[^:]*>', name='account')
def account_(slashed_account_name=None):
    "A list of all the entries for this account realization."

    # Get the appropriate realization: if we're looking at the balance sheet, we
    # want to include the net-income transferred from the exercise period.
    account_name = slashed_account_name.strip('/').replace('/', account.sep)

    if account_name:
        options = app.options
        if account_name and account_types.is_balance_sheet_account(account_name, options):
            real_accounts = request.view.closing_real_accounts
        else:
            real_accounts = request.view.real_accounts
        if account_name not in real_accounts:
            raise bottle.HTTPError(404, "Not found.")
        real_account = real_accounts[account_name]
    else:
        real_account = request.view.real_accounts['']

    account_postings = realization.get_subpostings(real_account)

    oss = io.StringIO()
    journal.entries_table_with_balance(app, oss, account_postings)
    return render_view(
        pagetitle = '{}'.format(account_name or 'ROOT'),
        contents = oss.getvalue())


def get_conversion_entries(entries):
    """Return the subset of transaction entries which have a conversion."""
    return [entry
            for entry in misc_utils.filter_type(entries, Transaction)
            if data.transaction_has_conversion(entry)]


@viewapp.route('/conversions', name='conversions')
def conversions():
    "Render the list of transactions with conversions."

    view = request.view

    oss = io.StringIO()
    conversion_entries = get_conversion_entries(view.entries)
    journal.entries_table(app, oss, conversion_entries, render_postings=True)

    balance = summarize.compute_total_balance(conversion_entries)

    return render_view(
        pagetitle = "Conversions",
        contents = """
          <div id="table">
            {}
          </div>
          <h3>Conversion Total:<span class="num">{}</span></h3>
        """.format(oss.getvalue(), balance))


#--------------------------------------------------------------------------------


FORMATTERS = {
    'number': '{:.3f}'.format,
    'book_value': '{:.0f}'.format,
    'market_value': '{:.0f}'.format,
    'pnl': '{:.2f}'.format,
    'avg_cost': '{:.3f}'.format,
    'price_number': '{:.3f}'.format,
    }


@viewapp.route('/holdings', name='holdings')
def holdings_overview():
    "Render an index of the pages detailing holdings."
    return render_view(
        pagetitle = "Holdings",
        contents = """
          <ul>
            <li><a href="{V.holdings_detail}">Detailed Holdings List</a></li>
            <li><a href="{V.holdings_byinstrument}">By Instrument</a></li>
          </ul>
        """.format(V=V))


@viewapp.route('/holdings/detail', name='holdings_detail')
def holdings_detail():
    "Render a detailed table of all holdings."

    price_map = prices.build_price_map(request.view.entries)
    holdings_ = holdings.get_final_holdings(request.view.closing_entries, price_map)

    oss = io.StringIO()
    oss.write('<center>\n')
    if holdings_:
        table.render_tuples_to_html_table(
            holdings_,
            field_spec=[
                'account',
                ('number', None, '{:,.2f}'.format),
                'currency',
                'cost_currency',
                ('cost_number', 'Cost Price', '{:,.2f}'.format),
                ('price_number', 'Market Price', '{:,.2f}'.format),
                ('book_value', None, '{:,.2f}'.format),
                ('market_value', None, '{:,.2f}'.format),
                'price_date',
            ],
            classes=['holdings', 'detail-table', 'sortable'],
            file=oss)
    else:
        oss.write("(No holdings.)")
    oss.write('</center>\n')

    return render_view(
        pagetitle = "Holdings - Detailed List",
        scripts = '<script src="/third_party/sorttable.js"></script>',
        contents = oss.getvalue())


@viewapp.route('/holdings/byinstrument', name='holdings_byinstrument')
def holdings_byinstrument():
    "Render a table of holdings by instrument."

    price_map = prices.build_price_map(request.view.entries)
    holdings_ = holdings.get_final_holdings(request.view.closing_entries, price_map)
    aggregated_holdings = holdings.aggregate_by_base_quote(holdings_)

    oss = io.StringIO()
    oss.write('<center>\n')
    if holdings_:
        table.render_tuples_to_html_table(
            aggregated_holdings,
            field_spec=[
                ('number', None, '{:,.2f}'.format),
                'currency',
                'cost_currency',
                ('cost_number', 'Average Cost', '{:,.2f}'.format),
                ('price_number', 'Average Price', '{:,.2f}'.format),
                ('book_value', None, '{:,.2f}'.format),
                ('market_value', None, '{:,.2f}'.format)
            ],
            classes=['holdings', 'byinst-account-table', 'sortable'],
            file=oss)
    else:
        oss.write("(No holdings.)")
    oss.write('</center>\n')

    return render_view(
        pagetitle = "Holdings - By Instrument",
        scripts = '<script src="/third_party/sorttable.js"></script>',
        contents = oss.getvalue())


#--------------------------------------------------------------------------------


@viewapp.route('/trades', name='trades')
def trades():
    "Render a list of the transactions booked against inventory-at-cost."
    return render_view(
        pagetitle = "Trades",
        contents = NOT_IMPLEMENTED
        )


@viewapp.route('/documents', name='documents')
def documents():
    "Render a tree with all the documents found."
    document_entries = list(misc_utils.filter_type(request.view.entries, Document))
    oss = io.StringIO()
    if document_entries:
        journal.entries_table(app, oss, document_entries)
    else:
      oss.write("(No documents.)")
    return render_view(
        pagetitle = "Documents",
        contents = oss.getvalue())


def row_data_to_html_rows(items):
    return "\n".join(('<tr><td>{}</td>'
                      '<td style="text-align: right">{}</td></tr>').format(*item)
                     for item in items)


@viewapp.route('/stats', name='stats')
def stats():
    "Compute and render statistics about the input file."

    # Count the number of entries by type.
    entries_by_type = misc_utils.groupby(lambda entry: type(entry).__name__,
                                    request.view.entries)
    nb_entries_by_type = {name: len(entries)
                          for name, entries in entries_by_type.items()}
    rows_entries = sorted(nb_entries_by_type.items(), key=lambda x: -x[1])
    rows_entries.append(('Total', len(request.view.entries)))

    # Count the number of postings by account.
    all_postings = [posting
                    for entry in request.view.entries
                    if isinstance(entry, Transaction)
                    for posting in entry.postings]
    postings_by_account = misc_utils.groupby(lambda posting: posting.account,
                                        all_postings)
    nb_postings_by_account = {key: len(postings)
                              for key, postings in postings_by_account.items()}
    rows_postings = sorted(nb_postings_by_account.items(), key=lambda x: -x[1])

    if False:
        # Keep only enough postings to cover the top {topn}% of all entries.
        # This gets us rid of the long tail, which isn't interesting.
        topn = 0.90
        required = len(all_postings) * topn
        seen = 0
        for i, item in enumerate(rows_postings):
            _, count = item
            seen += count
            if seen > required:
                break
        del rows_postings[i:]

    rows_postings.append(('Total', len(all_postings)))

    contents = """
      <h2>Nb. Entries</h2>
      <table>
        <thead><tr><td>Description</td><td>Count</td></tr></thead>
        {rows_entries}
      </table>

      <h2>Top Nb. Postings</h2>
      <table>
        <thead><tr><td>Account</td><td>Count</td></tr></thead>
        {rows_postings}
      </table>
    """.format(rows_entries=row_data_to_html_rows(rows_entries),
               rows_postings=row_data_to_html_rows(rows_postings))

    return render_view(
        pagetitle = "View Statistics",
        contents = contents
        )



#--------------------------------------------------------------------------------
# Views.


# A cache for views that have been created (on access).
app.views = {}


def handle_view(path_depth):
    """A decorator for handlers which create views lazily.
    If you decorate a method with this, the wrapper does the redirect
    handling and your method is just a factory for a View instance,
    which is cached."""

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

            # Save for hte subrequest and redirect. populate_view() picks this
            # up and saves it in request.view.
            request.environ['VIEW'] = view
            return internal_redirect(viewapp, path_depth)
        return wrapper
    return view_populator


def populate_view(callback):
    "A plugin that will populate the request with the current view instance."
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
    allowed = ['/web.css',
               '/favicon.ico']

    def url_restrict_handler(callback):
        def wrapper(*args, **kwargs):
            if request.path in allowed or request.path.startswith(url_prefix):
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


@app.route(r'/view/year/<year:re:\d\d\d\d>/<path:re:.*>', name='year')
@handle_view(3)
def year(year=None, path=None):
    year = int(year)
    return views.YearView(app.entries, app.options, 'Year {:4d}'.format(year), year)


@app.route(r'/view/tag/<tag:re:[^/]*>/<path:re:.*>', name='tag')
@handle_view(3)
def tag(tag=None, path=None):
    return views.TagView(app.entries, app.options, 'Tag {}'.format(tag), set([tag]))


@app.route(r'/view/payee/<payee:re:[^/]*>/<path:re:.*>', name='payee')
@handle_view(3)
def payee(payee=None, path=None):
    return views.PayeeView(app.entries, app.options, 'Payee {}'.format(payee), payee)


# FIXME: Not implemented yet.
@app.route(r'/view/level1/<level:re:[^/]*>/<path:re:.*>', name='level1')
@handle_view(3)
def level1(level=None, path=None):
    return views.EmptyView(app.entries, app.options, 'Level 1: {}'.format(level), level)

# FIXME: Not implemented yet.
@app.route(r'/view/level2/<level:re:[^/]*>/<path:re:.*>', name='level2')
@handle_view(3)
def level2(level=None, path=None):
    return views.EmptyView(app.entries, app.options, 'Level 2: {}'.format(level), level)


# ## FIXME: We need to figure out how to deal with id-ification for paths.
# We need some sort of mapping from idified tag to "real" tag. Either of don't idify at all.
# Is the syntax compatible?
#     # Create views for all tags.
#     for tagid, tag in utils.compute_unique_clean_ids(get_all_tags(entries)):



#--------------------------------------------------------------------------------
# Bootstrapping and main program.


def auto_reload_input_file(callback):
    """A plugin that automatically reloads the input file if it changed since the
    last page was loaded."""
    def wrapper(*posargs, **kwargs):

        filename = app.args.filename
        mtime = path.getmtime(filename)
        if mtime > app.last_mtime:
            app.last_mtime = mtime

            logging.info('RELOADING')

            # Save the source for later, to render.
            with open(filename, encoding='utf8') as f:
                app.source = f.read()

            # Parse the beancount file.
            entries, errors, options_map = loader.load(
                filename, add_unrealized_gains=True, do_print_errors=True)

            # Save globals in the global app.
            app.entries = entries
            app.errors = errors
            app.options = options_map
            app.account_types = options.get_account_types(options_map)

            # Pre-compute the price database.
            app.price_map = prices.build_price_map(app.entries)

            # Reset the view cache.
            app.views.clear()

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
            contents = replace_numbers(contents)
        return contents

    return wrapper


def run_app(args, quiet=None):
    logging.basicConfig(level=logging.INFO,
                        format='%(levelname)-8s: %(message)s')

    loader.install_plugins(args.plugin)

    # Hide the numbers in incognito mode. We do this on response text via a plug-in.
    if args.incognito:
        args.no_source = True
        app.install(incognito)
        viewapp.install(incognito)

    # Install code that will restrict all resources to a particular view.
    if args.view:
        view_url_prefix = '/view/{}/'.format(args.view)
        url_restrictor = url_restrict_generator(view_url_prefix)
        app.install(url_restrictor)

    # Initialize to a small value in order to insure a reload on the first page.
    app.last_mtime = 0

    # Load templates.
    with open(path.join(path.dirname(__file__), 'web.html')) as f:
        global template
        template = bottle.SimpleTemplate(f)

    with open(path.join(path.dirname(__file__), 'web.css')) as f:
        global STYLE; STYLE = f.read()

    # Run the server.
    app.args = args
    app.run(host='localhost', port=args.port,
            debug=args.debug, reloader=False,
            quiet=args.quiet if hasattr(args, 'quiet') else quiet)


# The global server instance.
server = None

def setup_monkey_patch_for_server_shutdown():
    """Setup globals to steal access to the server reference.
    This is required to initiate shutdown, unfortunately.
    (Bottle could easily remedy that.)"""

    # Save the original function.
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
    while server is None:
        time.sleep(0.05)

def shutdown():
    """Request for the server to shutdown."""
    server.shutdown()


def add_web_arguments(argparser):
    group = argparser.add_argument_group("Web server arguments")

    group.add_argument('filename',
                       help="Beancount input filename to serve.")

    group.add_argument('--plugin', action='append', default=[],
                       help="The name of a plugin to import before running.")

    group.add_argument('--port', action='store', type=int, default=8080,
                       help="Which port to listen on.")

    group.add_argument('--debug', action='store_true',
                       help="Enable debugging features (auto-reloading of css).")

    group.add_argument('--incognito', action='store_true',
                       help=("Filter the output in order to hide all the numbers. "
                             "This is great for demos using my real file."))

    group.add_argument('--no-source', action='store_true',
                       help=("Don't render the source."))

    group.add_argument('--view', action='store',
                       help="Render only the specified view (identify by URL)")
    return group


def main():
    """Main web service runner. This runs the event loop and blocks indefinitely."""

    argparser = argparse.ArgumentParser(__doc__.strip())
    add_web_arguments(argparser)
    args = argparser.parse_args()

    run_app(args)


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

    Unfortauntely, in the meantime this has a side-effect on all servers.
    This returns after waiting that the thread has stopped.

    Args:
      thread: A threading.Thread instance.
    """
    # Clean shutdown: request to stop, then join the thread.
    # Note that because we daemonize, we could forego this elegant detail.
    shutdown()
    thread.join()
