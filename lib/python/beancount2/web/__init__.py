"""
Web server for Beancount ledgers.
"""
import argparse
from os import path
from textwrap import dedent

import bottle

from beancount2 import parser
from beancount2 import checks
from beancount2.data import Open, Close


def get_date_limits(entries):
    """REturn the minimum and maximum dates."""
    if entries:
        return entries[0].date, entries[-1].date
    else:
        return None, None

def render(title, contents, **kw):
    """Render the title and contents in our standard template."""
    output = bottle.template(template,
                             title=title)
    return output.format(navigation="", contents=dedent(contents))




@bottle.route('/')
@bottle.route('/home')
def home():
    mindate, maxdate = get_date_limits([entry for entry in contents.entries
                                        if not isinstance(entry, (Open, Close))])

    all = '<li><a href="{}">{}</a></li>'.format('', 'all')
    years = '\n'.join(['<li><a href="{}">{}</a></li>'.format(year, year)
                       for year in range(mindate.year, maxdate.year + 1)])
    return render("Overview",
                  """
      <h2>Ledgers</h2>

      <h2>All</h2>
      <h2>By-Year</h2>
      <ul>
        {all}
        {years}
      </ul>

    """.format(**vars()))




ledger_app = bottle.Bottle()

@ledger_app.route('/index/<no>')
def ledger_index(no):
    return str('TEST {}'.format(no))





#FIXME: I want to be able to render links with a global index of symbols, not as links, just like I did in my own thing.
# This makes everything nicer...
# def route(path, *args):
#     def decorator(fun):
#         fun = route(path, *args)


# CHeck this out, might be related:
    # def get_url(self, routename, **kargs):


        # bottle.route('/a/:b/c', name='named')(foo)
        # bottle.request.environ['SCRIPT_NAME'] = ''
        # self.assertEqual('/a/xxx/c', bottle.url('named', b='xxx'))
        # self.assertEqual('/a/xxx/c', bottle.app().get_url('named', b='xxx'))



def main():
    argparser = argparse.ArgumentParser(__doc__.strip())
    argparser.add_argument('filename', help="Beancount input filename to serve.")
    opts = argparser.parse_args()

    # Parse the beancount file.
    global contents
    contents = parser.parse(opts.filename)

    # Check for errors.
    errors = checks.check(contents.entries, contents.accounts)
## FIXME: Not sure what to do with errors yet.



    # Load templates.
    global template
    template = open(path.join(path.dirname(__file__), 'template.html')).read()

    bottle.mount('/ledger/byyear/2013', ledger_app)
    bottle.mount('/ledger/byyear/2012', ledger_app)
    bottle.mount('/ledger/byyear/2011', ledger_app)

    bottle.run(host='localhost', port=8080,
               # reloader=True,
               debug=True)
