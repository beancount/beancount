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
from beancount.timeparse import parse_time
from beancount import install_psyco


MANY=-1

def main(parser, no=1):
    "Parse the cmdline as a list of ledger source files and return a Ledger."

    logging.basicConfig(level=logging.INFO,
                        format='%(levelname)-8s:%(message)s')

    parser.add_option('-v', '--verbose', action='store_true',
                      help="Display warnings and non-essential information.")

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

    if no == 1:
        fn, args = args[0], args[1:]
        ledger = load_ledger(parser, fn, opts)
        return opts, ledger, args
    elif no == MANY:
        ledgers = [load_ledger(parser, fn, opts) for fn in args]
        return opts, ledgers, args
    elif no == 0:
        return opts, None, args

def load_ledger(parser, fn, opts):
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
    
    if hasattr(opts, 'close') and opts.close:
        closedate, _ = parse_time(opts.close)
        ledger.close_books(closedate)

    filter_opts = 'account', 'transaction_account', 'note', 'time', 'tag'

    if all(hasattr(opts, x) for x in filter_opts):
        pred = create_filter_pred(opts)
        ledger.filter_postings(pred)
    




"""
Code to filter down specific postings.
"""

def addopts(parser):
    "Add options for selecting accounts/postings."

    parser.add_option('-c', '--close', '--close-books',
                      action='store', metavar='TIME_EXPR',
                      help="Close the books at the given time.")

    group = optparse.OptionGroup(parser, "Options for filtering postings.")

    group.add_option('-a', '--account', action='append', metavar='REGEXP',
                     default=[],
                     help="Filter only the postings whose account matches the given regexp.")

    group.add_option('-A', '--transaction-account', action='append', metavar='REGEXP',
                     default=[],
                     help="Filter only the transactions which have at least one account which matches the given regexp.")

    group.add_option('-n', '--note', action='append', metavar='REGEXP',
                     help="Filter only the postings with the given notes.")

    group.add_option('-t', '--time', action='store', metavar='TIME_EXPR',
                     help="Filter only the postings within the given time range. "
                     "There are multiple valid time range formats. See source "
                     "for details.")

    group.add_option('-g', '--tag', action='store', metavar='REGEXP',
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

    if all((x is None) for x in
           (acc_funs, txnacc_funs, note_funs, interval, tagfun)):
        # Simpler predicate for speed optimization.
        def pred(post):
            return True
    else:
        def pred(post):
            if acc_funs is not None:
                if all(not fun(post.account.fullname) for fun in acc_funs):
                    return False
            if txnacc_funs is not None:
                if all(all(not fun(p.account.fullname) for fun in txnacc_funs)
                       for p in post.txn.postings):
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


