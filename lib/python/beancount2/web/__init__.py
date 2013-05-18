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


@bottle.route('/ledger/byyear/<year>/<rest:path>')
def ledger_byyear(year, rest):
    print(year, repr(rest))
    return str(year)






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

    bottle.run(host='localhost', port=8080, debug=True, reloader=True)


#FIXME: I want to be able to render links with a global index of symbols, not as links, just like I did in my own thing.
k# This makes everything nicer...
