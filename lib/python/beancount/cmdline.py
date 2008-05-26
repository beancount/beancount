"""
Common cmdline interface for ledger scripts.
"""

# stdlib imports
import sys, os, logging, optparse, re, codecs
from datetime import date
import cPickle as pickle
from os.path import exists, getmtime

# other imports
from colorlog import ColorFormatter

# beancount imports
from beancount.ledger import Ledger
from beancount.timeparse import parse_time
from beancount import install_psyco


def main(parser):
    "Parse the cmdline as a list of ledger source files and return a Ledger."

    logging.basicConfig(level=logging.INFO,
                        format='%(levelname)-8s:%(message)s')

    parser.add_option('-p', '--enable-pickle', action='store_true',
                      help="Enable the pickling cache (create or use it).")

    parser.add_option('-e', '--encoding', '--input-encoding', action='store',
                      default='utf8',
                      help="Specify the encoding of the input files.")

    parser.add_option('-C', '--no-color',
                      dest='color', action='store_false', default=True,
                      help="Disable color terminal output.")

    parser.add_option('--no-psyco', action='store_true',
                      help="Disable psyco JIT optimizations.")

    opts, args = parser.parse_args()

    if sys.stderr.isatty() and opts.color:
        hndlr = logging.getLogger().handlers[-1]
        hndlr.setFormatter(ColorFormatter(hndlr.stream, hndlr.formatter._fmt))

    if not args:
        # Note: the support for env var input is only there to avoid putting off
        # existing ledger users. Remove when it makes sense.
        try:
            args.append(os.environ['LEDGER_FILE'])
        except KeyError:
            parser.error("You must provide some files or set the "
                         "environment variable LEDGER_FILE.")

    if not opts.no_psyco:
        install_psyco()

    fn, args = args[0], args[1:]

    # Parse the file.
    if not exists(fn):
        parser.error("No such file '%s'." % fn)

    # Rebuild the Ledger file if it needs it; otherwise load from the cache.
    fn_cache = '%s.pickle' % fn
    if not opts.enable_pickle:
        if exists(fn_cache):
            os.remove(fn_cache)

    if (not opts.enable_pickle or
        not exists(fn_cache) or
        getmtime(fn) > getmtime(fn_cache)):

        # logging.info("Parsing Ledger source file: %s" % fn)
        ledger = Ledger()

        f = open(fn)
        if opts.encoding:
            Reader = codecs.getreader(opts.encoding)
            f = Reader(f)
        ledger.parse_file(f, fn, opts.encoding)

        if opts.enable_pickle:
            f = open(fn_cache, 'wb')
            pickle.dump(ledger, f)
            f.close()

    else:
        f = open(fn_cache, 'rb')
        ledger = pickle.load(f)
        f.close()

    ledger.run_directives()

    return opts, ledger, args

def reload(ledger):
    """
    Parse the files again and create a new Ledger from them.
    """
    # Note: we ignore the pickling for reload.
    ledger2 = Ledger()

    for fn, encoding in ledger.parsed_files:
        f = open(fn)
        if encoding:
            Reader = codecs.getreader(encoding)
            f = Reader(f)
        ledger2.parse_file(f, fn, encoding)

    ledger2.run_directives()
    return ledger2




"""
Code to filter down specific postings.
"""

class SelectPostings(object):

    @staticmethod
    def mark(node):
        node.selected = 1

    def __init__(self):
        self.postings = []

    def __call__(self, acc):
        if getattr(acc, 'selected', False):
            self.postings.extend(acc.postings)

def select_addopts(parser):
    "Add options for selecting accounts/postings."

    parser.add_option('-a', '--account', action='append', metavar='REGEXP',
                      default=[],
                      help="Filter only the given accounts.")

    parser.add_option('-n', '--note', action='append', metavar='REGEXP',
                      help="Filter only the postings with the given notes.")

    parser.add_option('-t', '--time', action='store', metavar='REGEXP',
                      help="Filter only the postings within the given time range. "
                      "There are multiple valid time range formats. See source "
                      "for details.")

    parser.add_option('-g', '--tag', action='store', metavar='REGEXP',
                      help="Filter only the postings whose tag matches the "
                      "expression.")

    ## parser.add_option('-o', '--open-balances', action='store_true',
    ##                   help="Include opening balance entries when filtering "
    ##                   "according to time.")

def create_filter_pred(opts):
    """
    Synthesize and return a predicate that when applied to a Ledger's postings
    will filter only the transactions specified in 'opts'. If there is no filter
    to be applied, simply return None.
    """
    acc_funs = None
    if opts.account:
        try:
            acc_funs = [re.compile(regexp, re.I).search for regexp in opts.account]
        except re.error, e:
            raise SystemExit(e)

    note_funs = None
    if opts.note:
        try:
            note_funs = [re.compile(regexp, re.I).search for regexp in opts.note]
        except re.error, e:
            raise SystemExit(e)

    if opts.time:
        try:
            interval = parse_time(opts.time)
            if interval is not None:
                logging.info("Filtering by interval:  %s  ->  %s" % interval)
        except ValueError, e:
            raise SystemExit(e)
    else:
        interval = None

    if opts.tag:
        try:
            tagfun = re.compile(opts.tag, re.I).search
        except re.error, e:
            raise SystemExit(e)
    else:
        tagfun = None

    def pred(post):
        if acc_funs is not None:
            if all(not fun(post.account.fullname) for fun in acc_funs):
                return False
        if note_funs is not None:
            if all(not fun(post.note or '') for fun in note_funs):
                return False
        if interval is not None:
            dbegin, dend = interval
            if not (dbegin <= post.actual_date < dend):
                return False
        if tagfun is not None:
            if not (post.txn.tag and tagfun(post.txn.tag)):
                return False
        return True

    return pred


def select_postings(ledger, opts):
    "Select the postings given by the selection options in 'opts'"

    # Mark all the selected accounts.
    if opts.account:
        for regexp in opts.account:
            try:
                are = re.compile('.*%s.*' % regexp, re.I)
            except re.error, e:
                raise SystemExit(e)
            for acc in ledger.accounts.itervalues():
                if are.match(acc.fullname):
                    ledger.visit(acc, SelectPostings.mark)
    else:
        ledger.visit(ledger.get_root_account(), SelectPostings.mark)

    # Pick up the selected postings.
    vis = SelectPostings()
    ledger.visit(ledger.get_root_account(), vis)
    return vis.postings



