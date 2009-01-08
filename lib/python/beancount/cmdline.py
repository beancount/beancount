"""
Common cmdline interface for ledger scripts.
"""

# stdlib imports
import sys, os, logging, optparse, re, codecs
from datetime import date
import cPickle as pickle
from os.path import exists, getmtime

# other imports
from beancount.fallback.colorlog import ColorFormatter

# beancount imports
from beancount.ledger import Ledger
from beancount.timeparse import parse_time, parse_one_time
from beancount import install_psyco
from beancount import assetdef


MANY=-1

DATE_WAAAYBACK = date(1800, 1, 1)
DATE_WAAAYFWD = date(4000, 1, 1)

def main(parser, no=MANY):
    "Parse the cmdline as a list of ledger source files and return a Ledger."

    logging.basicConfig(level=logging.INFO,
                        format='%(levelname)-8s:%(message)s')

    parser.add_option('-v', '--verbose', action='store_true',
                      help="Display warnings and non-essential information.")

    parser.add_option('-e', '--encoding', '--input-encoding', action='store',
                      default='utf8',
                      help="Specify the encoding of the input files.")

    parser.add_option('-C', '--no-color',
                      dest='color', action='store_false', default=True,
                      help="Disable color terminal output.")

    parser.add_option('--no-psyco', action='store_true',
                      help="Disable psyco JIT optimizations.")

    parser.add_option('--assets', action='append', default=[],
                      help="Root directories for asset definition files.")

    parser.add_option('--unsafe', '--with-source',
                      action='store_false', default=True,
                      help="Allow serving some possibly sensitive personal informations,"
                      " access to source file, for example.")

    opts, args = parser.parse_args()

    logging.getLogger().setLevel(logging.INFO if opts.verbose else logging.ERROR)
                                 
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

    for assfn in opts.assets:
        assetdef.add_asset_path(assfn)

    if opts.begin:
        opts.begin, _ = parse_one_time(opts.begin)
    if opts.end:
        opts.end, _ = parse_one_time(opts.end)

    if no == 0:
        return opts, None, args
    elif no == 1:
        ledger = load_ledger(parser, args[0:1], opts)
        args = args[1:]
        return opts, ledger, args
    elif no == MANY:
        ledger = load_ledger(parser, args, opts)
        return opts, ledger, args

def load_ledger(parser, filenames, opts):
    # logging.info("Parsing Ledger source file: %s" % fn)
    ledger = Ledger()
    for fn in filenames:
        if fn == '-':
            f = sys.stdin
        else:
            f = open(fn)
        if opts.encoding:
            Reader = codecs.getreader(opts.encoding)
            f = Reader(f)
        ledger.parse_file(f, fn, opts.encoding)

    run_postprocesses(ledger, opts)

    return ledger

def reload(ledger, opts):
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

    run_postprocesses(ledger2, opts)

    return ledger2

def run_postprocesses(ledger, opts):
    ledger.run_directives()
    
    if hasattr(opts, 'begin') and opts.begin:
        ledger.close_books(opts.begin)

    filter_opts = 'account', 'transaction_account', 'note', 'begin', 'end', 'tag'

    if all(hasattr(opts, x) for x in filter_opts):
        pred = create_filter_pred(opts)
        ledger.filter_postings(pred)
    
    ledger.compute_balsheet('local_balance', 'balance')




"""
Code to filter down specific postings.
"""

def addopts(parser):
    "Add options for selecting accounts/postings."

    parser.add_option('--begin', '--start', '--close', dest='begin', 
                      action='store', metavar='TIME_EXPR',
                      help="Begin time in the interval in use for the flow "
                      "statements (e.g. Income Statement). If specified, "
                      "synthetic entries are inserted to close the books at "
                      "that time, so that level amounts are calculated properly.")

    parser.add_option('--end', '--stop', dest='end',
                      action='store', metavar='TIME_EXPR',
                      help="Ignore postings with a date after the given date. "
                      "This allows the flow statements (e.g. Income Statement) "
                      "to focus on a finite period.")

    group = optparse.OptionGroup(parser, "Options for filtering postings.")

    group.add_option('-a', '--account', action='append', metavar='REGEXP',
                     default=[],
                     help="Filter only the postings whose account matches "
                     "the given regexp.")

    group.add_option('-A', '--transaction-account',
                     action='append', metavar='REGEXP', default=[],
                     help="Filter only the transactions which have at least "
                     "one account which matches the given regexp.")

    group.add_option('-n', '--note', action='append', metavar='REGEXP',
                     help="Filter only the postings with the given notes.")

    group.add_option('-g', '-t', '--tag', action='store', metavar='REGEXP',
                     help="Filter only the postings whose tag matches the "
                     "expression.")
    
    parser.add_option_group(group)


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

    txnacc_funs = None
    if opts.transaction_account:
        try:
            txnacc_funs = [re.compile(regexp, re.I).search for regexp in opts.transaction_account]
        except re.error, e:
            raise SystemExit(e)

    note_funs = None
    if opts.note:
        try:
            note_funs = [re.compile(regexp, re.I).search for regexp in opts.note]
        except re.error, e:
            raise SystemExit(e)

        
    if opts.begin or opts.end:
        begin = opts.begin or DATE_WAAAYBACK
        end = opts.end or DATE_WAAAYFWD
        interval = (begin, end)
        logging.info("Filtering by interval:  %s  ->  %s" % interval)
    else:
        interval = None

    if opts.tag:
        try:
            tagfun = lambda tags: opts.tag in tags
        except re.error, e:
            raise SystemExit(e)
    else:
        tagfun = None

    if all((x is None) for x in
           (acc_funs, txnacc_funs, note_funs, interval, tagfun)):
        # Simpler predicate for speed optimization.
        def pred(post):
            return True
    else:
        def pred(post):
            if acc_funs is not None:
                if all(not fun(post.get_account_name()) for fun in acc_funs):
                    return False
            if txnacc_funs is not None:
                if all(all(not fun(p.account.fullname) for fun in txnacc_funs)
                       for p in post.get_txn_postings()):
                    return False
            if note_funs is not None:
                if all(not fun(post.get_note() or '') for fun in note_funs):
                    return False
            if interval is not None:
                dbegin, dend = interval
                if not (dbegin <= post.get_date() < dend):
                    return False
            if tagfun is not None:
                tags = post.get_tags()
                if not (tags and tagfun(tags)):
                    return False
            return True

    return pred


