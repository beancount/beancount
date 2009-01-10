# -*- coding: utf-8 -*-
"""
Main file and data models for the Python version of Ledger.
"""

# stdlib imports
import sys, os, logging, re, codecs, string
from copy import copy, deepcopy
import cPickle as pickle
from decimal import Decimal, getcontext
from datetime import date, timedelta
from os.path import *
from operator import attrgetter
from itertools import count, izip, chain, repeat
from StringIO import StringIO
from bisect import bisect_left, bisect_right
from collections import defaultdict

# beancount imports
from beancount.wallet import Wallet
from beancount.utils import SimpleDummy, iter_pairs
from beancount.inventory import FIFOInventory

# fallback imports
from beancount.fallback.collections2 import namedtuple


__all__ = ('Account', 'Transaction', 'Posting', 'Ledger',
           'CheckDirective')

oneday = timedelta(days=1)


def init_wallets(py_wallets):
    """
    Initialize globals for precision calculations.
    """
    # Set the representational precision of Decimal objects.
    ctx = getcontext()
    ctx.prec = 16

init_wallets(1)



# The error messages that we use.
INFO     = logging.INFO      # Normal message, not an error.

WARNING  = logging.WARNING   # Benign problem, we continue.

ERROR    = logging.ERROR     # Serious error, but the parsed file is still valid,
                             # for example, a failed assert.

CRITICAL = logging.CRITICAL  # Error that we can't recover from,
                             # the Ledger is invalid.










class Message(object):
    "The encapsulation for a single message."

    def __init__(self, level, message, filename, lineno):
        self.level = level
        self.message = message
        self.filename = filename
        self.lineno = lineno

    def __str__(self):
        return '%s:%s:%d %s' % (self.level, self.filename, self.lineno, self.message)



class Account(object):
    """
    An account object.
    """
    __slots__ = ('sep', 'fullname', 'name', 'ordering', 'postings',
                 'parent', 'children', 'usedcount', 'isdebit', 'commodities',
                 'balances',
                 'checked', 'check_min', 'check_max')

    # Account path separator.
    sep = ':'

    def __init__(self, fullname, ordering):

        # The full name of the account.
        self.fullname = fullname

        # The full name of the account.
        self.name = fullname.split(Account.sep)[-1]
        self.ordering = ordering

        # A list of the postings contained in this account.
        self.postings = []

        # The parent and children accounts.
        self.parent = None
        self.children = []

        # The number of times an account has been requested for use.
        self.usedcount = 0

        # Flag: True if a debit account, False if a credit account, None is unknown.
        self.isdebit = None

        # A list of valid commodities that can be deposited in this account.
        self.commodities = []

        # An attribute that tells if this account contains a check.
        # (Set by the 'check' directive.)
        self.checked = None
        self.check_min = None
        self.check_max = None

        # A dict of available balances on an account object. All the wallet
        # amounts calculated per-account can be stored here under unique names.
        # Some names are reserved: 'local' is the main balance for this account.
        # 'cumul' for is the cumulative equivalent.
        self.balances = {} 

    def __str__(self):
        return "<Account '%s'>" % self.fullname
    __repr__ = __str__

    def __len__(self):
        "Return the number of subpostings for the account."
        n = len(self.postings)
        n += sum(len(child) for child in self.children)
        return n

    atypemap = {True: 'Debit',
                False: 'Credit',
                None: ''}
    def getatype(self):
        return self.atypemap[self.isdebit]

    def isroot(self):
        return self.fullname == ''

    def isused(self):
        "Return true if the account is used."
        return self.usedcount > 0

    def subpostings(self):
        """
        Iterator that yields all of the postings in this account and in its
        children accounts.
        """
        for post in self.postings:
            yield post
        for child in self.children:
            for post in child.subpostings():
                yield post

    def get_fullname(self):
        "Compute the full name of the account from its hierarchy."
        if self.parent is None:
            return self.name
        else:
            return Account.sep.join(
                (self.parent.get_fullname(), self.name)).lstrip(':')

    def ischildof(self, cparent):
        """ Return true if the 'cparent' account is a parent of this account (or
        is that account itself)."""

        if self is cparent:
            return True
        elif self.parent is None:
            return False
        else:
            return self.parent.ischildof(cparent)

    def clear_field(self, aname):
        """ Rename the attribute 'aname' from the account and all its
        subaccounts."""
        if hasattr(self, aname):
            self.balances.pop(aname)
        for child in self.children:
            child.clear_field(aname)






class Dated(object):
    "Base class for dates and ordered objects."

    actual_date = None
    effective_date = None
    ordering = None

    def __cmp__(self, other):
        """ A comparison function that takes into account the ordering of the
        transactions."""
        c = cmp(self.actual_date, other.actual_date)
        if c != 0:
            return c
        else:
            return cmp(self.ordering, other.ordering)

    def rdate(self):
        return self.actual_date.strftime('%Y-%m-%d')

    def fulldate(self):
        l = [self.actual_date.strftime('%Y-%m-%d')]
        if self.effective_date != self.actual_date:
            l.append(self.effective_date.strftime('=%Y-%m-%d'))
        return ''.join(l)


class Filtrable(object):
    """
    Base class for filtering using the predicate created by the cmdline module.
    """
    def get_date(self):
        pass

    def get_account_name(self):
        pass

    def get_note(self):
        pass

    def get_tags(self):
        pass

    def get_txn_postings(self):
        pass


class Transaction(Dated):
    "A transaction, that contains postings."

    # Parse origin.
    filename = None
    lineno = None

    flag = None
    code = None

    # 'payee' and 'narration' compose the description field. Usually, we want to
    # be able to split the payee apart, because it can be used to further
    # automate the entry of transactions, or to filter down the results by
    # payee. If there is separator, we simply leave the 'payee' field empty.
    payee = None
    narration = None

    # The tags is a (shared) list that is assigned to a set of transactions,
    # using the @begintag and @endtag directives. This can be used to mark
    # transactions during a trip, for example.
    tags = None

    # The vector sum of the various postings here-in contained.
    wallet = None

    def __init__(self):
        # The list of contained postings.
        self.postings = []

    def description(self):
        return ''.join(['%s | ' % self.payee if self.payee else '',
                        self.narration or ''])

    def topline(self):
        "Render the top line of the transaction, without the date."
        # Compute the transaction declaration line.
        l = []
        append = l.append
        append(' ')
        append('%s ' % (self.flag or ' '))
        if self.code:
            append('(%s) ' % self.code)
        desc = self.description()
        if desc:
            append(desc)
        return ''.join(l)

    def __str__(self):
        "Produce a basic rendering for debugging."
        lines = [self.fulldate() + ' ' + self.topline()]
        lines.extend(str(post) for post in self.postings)
        return os.linesep.join(lines)

    def pretty(self):
        "Produce a pretty rendering."
        lines = [self.topline()]
        lines.extend(post.pretty() for post in self.postings)
        return os.linesep.join(lines)

    def get_booking_post(self):
        """Find a booking entry in its posts and return it or None if there
        aren't any."""
        for post in self.postings:
            if post.booking is not None:
                return post


VIRT_NORMAL, VIRT_BALANCED, VIRT_UNBALANCED = 0, 1, 2

class Posting(Dated):
    """
    A posting or entry, that lives within a transaction.
    """
    # Parse origin.
    filename = None
    lineno = None

    # The transaction that this posting belongs to.
    txn = None

    flag = None
    account = None
    account_name = None
    virtual = VIRT_NORMAL
    amount = None

    price = None         # Price-per-commodity.
    cost = None          # The cost of this commodity.

    note = None
    booking = False  # Whether this is a booking entry to be filled in.

    def __init__(self, txn):
        self.txn = txn

    def __str__(self):
        s = '  %-70s %s' % (self.account_name or self.account.name, self.amount)
        if self.note:
            s += ' ; %s' % self.note
        return s
    __repr__ = __str__

    def __key__(self):
        return (self.actual_date, self.ordering)

    def pretty(self):
        "Produce a pretty rendering."
        return '  %-50s %10s (Cost: %10s) %s' % (
            #self.actual_date,
            self.account.fullname,
            self.amount.round(),
            self.cost.round(),
            '; %s' % self.note if self.note else '')

    #
    # Filtrable.
    #
    def get_date(self):
        return self.actual_date

    def get_account_name(self):
        self.account.fullname

    def get_note(self):
        return self.note

    def get_tags(self):
        return self.txn.tags

    def get_txn_postings(self):
        return self.txn.postings


class BookedTrade(object):
    """
    An object that represents all the information that is present in a trade
    (turn-around).
    """
    Leg = namedtuple("Leg",
                     ('post amount_book '
                      'price comm_price '
                      'amount_price xrate amount_target').split())
    # comm_book: the commodity being booked
    # comm_price: the commodity in which the booked commodity is priced
    # comm_target: the target commodity for the gain/loss
    # amount_book: number of units that we booked
    # amount_price: amount in the natural pricing commodity
    # amount_target: amount in the final target commodity

    def __init__(self, account, comm_book, comm_target, post_book):

        # The account and booking commodity.
        self.account = account

        # The commodity being booked.
        self.comm_book = comm_book

        # The target pricing commodity for PnL calculation.
        self.comm_target = comm_target

        # The post that is adjusted for booking.
        self.post_book = post_book

        # The list of (posting, amount, price, amount_in, xrate) that
        # participated in the booked trade.
        self.legs = []

    def close_date(self):
        return self.legs[-1][0].actual_date

    __key__ = close_date
    def __cmp__(self, other):
        return cmp(self.close_date(), other.close_date())

    def add_leg(self, *args):
        "See Leg nested class for required members."
        leg = self.Leg(*args)
        assert leg.post.amount.tocomm() == self.comm_book
        self.legs.append(leg)

    #
    # Implement the Filtrable interface.
    #
    def get_date(self):
        return self.close_date()

    def get_account_name(self):
        return self.account.fullname

    def get_note(self):
        return None

    def get_tags(self):
        return []

    def get_txn_postings(self):
        return [x.post for x in self.legs]



class Ledger(object):
    """
    A ledger object, that contains transactions and directives and all data
    related to the construction of a single ledger, its list of commodities,
    etc.
    """
    def __init__(self):

        # A list of (filename, encoding) parsed.
        self.parsed_files = []

        # A dict of all the accounts.
        self.accounts = {}

        # A list of all the transactions in the file.
        self.transactions = []

        # A list of the commodities names.
        self.commodities = set()

        # A list of all the postings.
        self.postings = []

        # A list of the messages accumulated during the parsing of the ledger.
        self.messages = []

        # The source lines from which the Ledger was built.
        self.source = []

        # A map of commodity to a list of price commodities.
        self.pricedmap = None

        # A map of directive-name to contents.
        self.directives = {}
        add_directive = lambda x: self.directives.__setitem__(x.name, x)
        check = CheckDirective(self)
        add_directive(check)
        add_directive(DefineAccountDirective(self))
        add_directive(DefineCommodityDirective(self))
        add_directive(AutoPadDirective(self, check))
        add_directive(DefvarDirective(self))
        add_directive(BeginTagDirective(self))
        add_directive(EndTagDirective(self))
        add_directive(LocationDirective(self))
        add_directive(PriceDirective(self))

        # Current tags that are being assigned to transactions during parsing.
        self.tags = []

        # List of booked trades.
        self.booked_trades = []

    def isvalid(self):
        "Return true if the ledger has not had critical errors."
        return all(self.messages.level != CRITICAL)

    def dump_info(self):
        payees = set(txn.payee for txn in self.transactions)
        lines = [
            'Nb accounts: %d' % len(self.accounts),
            'Nb transactions: %d' % len(self.transactions),
            'Nb commodities: %d' % len(self.commodities),
            'Nb postings: %d' % len(self.postings),
            'Nb unique payees: %d' % len(payees),
            ]
        return lines

    def log(self, level, message, obj):
        "Log a message for later, and display to stderr."
        assert level in (logging.INFO,
                         logging.WARNING,
                         logging.ERROR,
                         logging.CRITICAL), level

        filename, lineno = None, None
        if isinstance(obj, tuple):
            filename, lineno = obj
        if hasattr(obj, 'filename'):
            filename = obj.filename
        if hasattr(obj, 'lineno'):
            lineno = obj.lineno

        if filename is not None:
            filename = abspath(filename)

        msg = Message(level, message, filename, lineno)
        self.messages.append(msg)
        if filename is not None and lineno is not None:
            s = ' %s:%-4d : %s' % (filename, lineno, message)
        else:
            s = ' %s' % message
        logging.log(level, s)


    # Account ordering integer.
    acc_ordering = count().next

    def get_account(self, name, create=False, incrcount=True):
        """
        Return or create an account by name, creating all the intermediate
        account tree nodes as well. 'incrcount' increases the account's 'used'
        count by that much (this is used to figure out which accounts are in
        use).
        """
        accounts = self.accounts
        try:
            acc = accounts[name]
        except KeyError:
            if not create:
                raise
            acc = accounts[name] = Account(name, self.acc_ordering())
            if name:
                # Set and make sure the parent exists.
                parent_name = Account.sep.join(name.split(Account.sep)[:-1])
                acc.parent = self.get_account(parent_name, create)
                children = acc.parent.children
                if acc not in children:
                    children.append(acc)
        if incrcount:
            acc.usedcount += 1
        return acc

    def get_root_account(self):
        "Return the root account."
        return self.get_account('', True)




    # Patterns for comments and empty lines.
    comment_re = re.compile('^\s*;(.*)$')
    empty_re = re.compile('^\s*$')

    # Pattern for date.
    date_re = re.compile('(\d\d\d\d)[/-](\d\d)[/-](\d\d)')

    # A date within a note.
    notedate_re = re.compile('\[(?:%(date)s)?(?:=%(date)s)?\]' %
                             {'date': date_re.pattern})

    # Pattern for a transaction line.
    txn_re = re.compile('^%(date)s(=%(date)s)?\s+(?:(.)\s+)?(\(.*?\))?(.*)$' %
                        {'date': date_re.pattern})
    payee_sep = ' | '
    desc_re = re.compile('(?:\s*([^|]+?)\s*\|)?\s*([^|]*?)\s*$')

    # Pattern for an amount.
    commodity_re = re.compile('"?([A-Za-z][A-Za-z0-9.~\']*)"?')
    amount_re = re.compile('([-+]?\d*(?:\.\d*)?)\s+%(comm)s' %
                           {'comm': commodity_re.pattern})

    # Pattern for an account (note: we don't allow spaces in this version).
    account_re = re.compile('[:A-Za-z0-9-_]+')
    postaccount_re = re.compile('(?:%(accname)s|\[%(accname)s\]|\(%(accname)s\))' %
                                {'accname': account_re.pattern})

    # Pattern for a posting line (part of a transaction).
    posting_re = re.compile(
        ('\s+([*!]\s+)?(%(account)s)' # account name
         '(?:'
         '(?:\s+%(amount)s)?'  # main
         '(?:\s+(?:({)\s*%(amount)s\s*}|({{)\s*%(amount)s\s*}}))?' # declared cost
         '(?:\s+@(@?)(?:\s+%(amount)s))?\s*(?:;(.*))?\s*$'
         '|'
         '\s+(BOOK)\s+%(commodity)s(?:\s+(IN)\s+%(commodity)s)?\s*$'  # booking entry
         ')') %  # price/note
        {'amount': amount_re.pattern,
         'account': postaccount_re.pattern,
         'commodity': commodity_re.pattern})

    # Pattern for the directives, and the special commands.
    directive_re = re.compile('^@([a-z_]+)\s+([^;]*)(;.*)?')
    special_re = re.compile('([YPNDCiobh])\s+')
    command_re = re.compile('!([a-z]+)')

    def parse_string(self, text, name='<string>', encoding='ascii'):
        f = StringIO(text)
        Reader = codecs.getreader(encoding)
        return self.parse_file(Reader(f), name, encoding)

    def parse_file(self, f, fn, encoding='ascii'):
        """
        Parse the file 'fn' in Ledger file format, into this Ledger object.

        return raw, unnormalized lists of objects that were seen in the file.
        (Those objects need to have completions and some conversions done on
        them, and more.)
        """
        self.parsed_files.append((fn, encoding))
        source = self.source = []

        # Cache some attribetus for speed.
        match_comment = self.comment_re.match
        match_empty = self.empty_re.match
        search_notedate = self.notedate_re.search
        match_txn = self.txn_re.match
        match_posting = self.posting_re.match
        match_special = self.special_re.match
        match_command = self.command_re.match
        match_directive = self.directive_re.match

        accounts = self.accounts

        xread = f.readline
        lineno = [0]
        def nextline():
            lineno[0] += 1
            line = xread()
            source.append(line)
            assert isinstance(line, unicode), line
            if not line:
                raise StopIteration
            return line

        add_commodity = self.commodities.add
        next_ordering = count(1).next
        try:
            line = nextline()
            while 1:
                # Skip comments.
                if match_empty(line) or match_comment(line):
                    line = nextline()
                    continue

                # Parse a transaction.
                mo = match_txn(line)
                if mo:
                    txn = Transaction()
                    txn.filename = fn
                    txn.lineno = lineno[0]
                    txn.ordering = next_ordering()
                    txn.tags = self.tags
                    self.transactions.append(txn)

                    try:
                        actual_date = date(*map(int, mo.group(1, 2, 3)))
                        if mo.group(4):
                            effective_date = date(*map(int, mo.group(5, 6, 7)))
                        else:
                            effective_date = actual_date
                    except ValueError, e:
                        self.log(CRITICAL, "Date component is out of range: %s" % e,
                                 (fn, lineno[0]))
                        line = nextline()
                        continue

                    txn.actual_date = actual_date
                    txn.effective_date = effective_date

                    txn.flag = mo.group(8)
                    txn.code = mo.group(9)

                    desc_line = mo.group(10)
                    mod = Ledger.desc_re.match(desc_line)
                    if not mod:
                        self.log(ERROR, "Invalid description: %s" % desc_line,
                                 (fn, lineno[0]))
                        line = nextline()
                        continue
                    txn.payee, txn.narration = mod.groups()

                    # Parse the postings.
                    while 1:
                        line = nextline()

                        # Allow comments in between postings, but not empty lines.
                        if match_comment(line):
                            continue

                        mo = match_posting(line)
                        if mo:
                            post = Posting(txn)
                            post.filename, post.lineno = fn, lineno[0]
                            post.ordering = next_ordering()
                            txn.postings.append(post)

                            (post.flag,
                             post.account_name,
                             post.note) = mo.group(1,2,14)

                            booking_comm = (mo.group(16) if mo.group(15) == 'BOOK'
                                            else None)
                            if booking_comm is not None:
                                booking_quote = (
                                    mo.group(18) if mo.group(17) == 'IN' else None)
                                post.booking = (booking_comm, booking_quote)
                            else:
                                post.booking = None

                            # Remove the modifications to the account name.
                            accname = post.account_name
                            fchar = accname[0]
                            if fchar in '[(':
                                accname = accname.strip()[1:-1]
                                post.virtual = (VIRT_BALANCED if fchar == '['
                                                else VIRT_UNBALANCED)
                            post.account = acc = self.get_account(accname, create=1)

                            # Fetch the amount.
                            anum, acom = mo.group(3,4)
                            if anum is not None:
                                anum = Decimal(anum)
                                post.amount = Wallet(acom, anum)
                                add_commodity(acom)
                            else:
                                post.amount = None


                            # Fetch the price.
                            pnum, pcom = mo.group(12,13)
                            if pnum is not None:
                                pnum = Decimal(pnum)
                                add_commodity(pcom)
                                if bool(mo.group(11) == '@'):
                                    pnum /= anum
                                post.price = Wallet(pcom, pnum)
                            else:
                                post.price = None


                            # Fetch the cost.
                            if mo.group(5) == '{':
                                assert mo.group(8) == None
                                cnum, ccom = mo.group(6,7)
                                cnum = anum*Decimal(cnum)
                                post.cost = Wallet(ccom, cnum)
                                add_commodity(ccom)

                            elif mo.group(8) == '{{':
                                assert mo.group(5) == None
                                cnum, ccom = mo.group(9,10)
                                cnum = Decimal(cnum)
                                post.cost = Wallet(ccom, cnum)
                                add_commodity(ccom)

                            else:
                                assert mo.group(5) is None, mo.groups()
                                assert mo.group(8) is None, mo.groups()


                            # Compute the price from the explicit cost.
                            if post.cost is not None:
                                if post.price is None:
                                    post.price = Wallet(ccom, cnum/anum)

                            # Compute the cost from the explicit price.
                            elif post.price is not None:
                                    post.cost = Wallet(pcom, anum*pnum)

                            # Compute the cost directly from the amount.
                            else:
                                post.cost = post.amount
                                if post.cost is not None:
                                    post.cost = Wallet(post.cost) # copy


                            # Look for date overrides in the note field.
                            if post.note:
                                mo = search_notedate(post.note)
                                if mo:
                                    # Set the posting's date according to the
                                    # dates in the note.
                                    actual = mo.group(1,2,3)
                                    if actual[0]:
                                        post.actual_date = date(*map(int, actual))
                                    effective = mo.group(4,5,6)
                                    if effective[0]:
                                        post.effective_date = \
                                            date(*map(int, effective))

                                    # Remove the date spec from the note itself.
                                    post.note = self.notedate_re.sub(post.note, '')

                            # Default values for dates should be those of the
                            # transaction.
                            if post.actual_date is None:
                                post.actual_date = txn.actual_date
                            if post.effective_date is None:
                                post.effective_date = txn.effective_date

                        else:
                            txn = None
                            break
                    continue

                # Parse a directive.
                mo = match_directive(line)
                if mo:
                    direc, direc_line = mo.group(1,2)
                    try:
                        parser = self.directives[direc]
                        parser.parse(direc_line, fn, lineno[0])
                    except ValueError, e:
                        self.log(CRITICAL, "Unknown directive %s: %s" % (direc, e),
                                 (fn, lineno[0]))
                    line = nextline()
                    continue

                # Parse a directive.
                mo = match_special(line)
                if mo:
                    self.log(WARNING, "Directive %s not supported." % mo.group(1),
                             (fn, lineno[0]))
                    line = nextline()
                    continue

                # Parse a directive.
                mo = match_command(line)
                if mo:
                    self.log(CRITICAL, "Command %s not supported." % mo.group(1),
                             (fn, lineno[0]))
                    line = nextline()
                    continue

                self.log(CRITICAL, "Cannot recognize syntax:\n %s" % line.strip(),
                         (fn, lineno[0]))
                line = nextline()

        except StopIteration:
            pass

        self.build_postings_lists()

        # Set the precision map according to some rules about the commodities.
        roundmap = Wallet.roundmap
        for com in self.commodities:
            prec = 2 if (len(com) == 3) else 3
            roundmap[com] = Decimal(str(10**-prec))

        self.complete_balances()
        self.compute_priced_map()
        self.complete_bookings()
        self.build_payee_lists()

    def build_postings_lists(self):
        """ (Re)Builds internal lists of postings from the list of transactions."""

        self.postings = []
        for acc in self.accounts.itervalues():
            acc.postings = []

        for txn in self.transactions:
            for post in txn.postings:
                post.account.postings.append(post)
                self.postings.append(post)

        self.postings.sort()
        for acc in self.accounts.itervalues():
            acc.postings.sort()

    def compute_balsheet(self, aname_bal, aname_total, atcost=False):
        """
        Compute a balance sheet stored in the given attribute on each account
        node.
        """
        vis = BalanceVisitor(aname_bal, aname_total, atcost)
        self.visit(self.get_root_account(), vis)

    def complete_balances(self):
        """
        Fill in missing numbers in each transactions and check if the
        transactions can be made to balance this way.
        """
        # Note: if a price is mentioned, we store the wallet in real terms of the
        # commodity specified in the amount, but we always try to convert to the
        # commodity specified in the price in order to balance.

        for txn in self.transactions:

            # Split postings between normal, virtual and virtual unbalanced.
            postsets = defaultdict(list)
            for post in txn.postings:
                postsets[post.virtual].append(post)

            # Process normal postings.
            self.check_postings_balance(postsets[VIRT_NORMAL])
            self.check_postings_balance(postsets[VIRT_BALANCED])

            # Process virtual balanced postings.

            # Process non-balanced virtual postings.
            for post in postsets[VIRT_UNBALANCED]:
                if post.cost is None:
                    if post.booking is None:
                        self.log(
                            WARNING,
                            "Virtual posting without amount has no effect.", post)
                    post.amount = post.cost = Wallet()

    def get_price_comm(self, comm):
        """ Return the pricing commodity of the given commodity 'comm'. Note
        that there must be a single pricing commodity for this to work. """
        pcomms = self.pricedmap[comm]
        assert len(pcomms) == 1, "Looking in %s for %s." % (pcomms, comm)
        return iter(pcomms).next()

    def complete_bookings(self):
        """
        Complete entries that are automatic bookings in each account.

        Important note: this does *NOT* automatically take into account the
        commissions.
        """

        # Build a list of booking jobs to be done, by looking at all
        # (unresolved) booking posts and finding out which accounts we need to
        # apply booking for which commodity. For example:
        #
        #    2004-11-29 * Sell QQQ
        #      Assets:Investments:RBCDirect:Taxable-CA:QQQ   -50.00 QQQ @ 39.02 USD
        #      Assets:Investments:RBCDirect:Taxable-CA      2233.52 CAD @ 0.8638 USD
        #      Expenses:Financial:Commissions                 25.00 CAD
        #      (Income:Investments:Capital-Gains)           BOOK QQQ IN CAD
        #
        # The 3rd posting tells us that we need to book QQQ, in the account
        # specified in the first posting. We would create a booking job to book
        # QQQ in account 'Assets:Investments:RBCDirect:Taxable-CA:QQQ'. The
        # second (or other) line(s) do(es) not affect the booking. Note that
        # this means that commissions are not included in the booked gain/loss.
        booking_posts = (post for post in self.postings if post.booking)
        booking_jobs = defaultdict(set)
        for post in booking_posts:
            comm_book, _ = post.booking
            n = 0
            for tpost in post.txn.postings:
                if comm_book in tpost.amount:
                    booking_jobs[tpost.account].add(comm_book)
                    n += 1
            if n == 0:
                logging.error("Invalid booking for %s in transaction at %s:%d" %
                              (comm_book, post.txn.filename, post.txn.lineno))

        # Apply the booking jobs to each required account.
        pricedir = self.directives['price']
        for acc, bcomms in sorted(booking_jobs.iteritems()):

            # For each booking commodity.
            for comm_book in bcomms:
                logging.info("Booking  %s  in %s" % (comm_book, acc.fullname))

                # Figure out the pricing commodity of the commodity being
                # booked.
                comm_price = self.get_price_comm(comm_book)

                # Process all of the account's postings in order.
                inv = FIFOInventory()
                booked = []
                for post in acc.postings:

                    # Apply trades to our inventory.
                    if comm_book in post.amount:
                        assert post.price is not None, post
                        assert post.price.tocomm() == comm_price
                        price = post.price.tonum()
                        _booked, _ = inv.trade(price, post.amount[comm_book], post)

                        booked.extend(_booked)

                    # If there is a booking posting in the current transaction,
                    # set its amount to the P+L at this point.
                    #
                    # FIXME: we should preprocess this to avoid the loop in
                    # processing each transaction.
                    txn = post.txn
                    post_book = txn.get_booking_post()
                    if post_book is None:
                        logging.warning("Unbooked %s in transaction at %s:%d" %
                                        (comm_book, txn.filename, txn.lineno))

                    elif not booked:
                        post_book.flag = 'B'
                        post_book.note = 'BOOKED'
                        logging.warning(
                            "Useless booking entry in transaction at %s:%d" %
                            (txn.filename, txn.lineno))

                    else:
                        _comm_book, comm_target = post_book.booking
                        assert _comm_book == comm_book, (_comm_book, comm_book)

                        # Create the trade's legs and compute the PnL.
                        btrade = BookedTrade(acc, comm_book, comm_target, post_book)
                        pnl_price, pnl_target = Decimal(), Decimal()
                        for post, amount_book in booked:
                            price, comm_price = post.price.single()

                            amount_price = -amount_book * price
                            if comm_target is None:
                                xrate = 1
                                amount_target = amount_price
                            else:
                                assert comm_target != comm_price, \
                                       (comm_target, comm_price)
                                xrate = pricedir.getrate(
                                    comm_price, comm_target, post.actual_date)
                                amount_target = amount_price * xrate

                            btrade.add_leg(post, amount_book,
                                           price, comm_price,
                                           amount_price, xrate, amount_target)

                            pnl_price += amount_price
                            pnl_target += amount_target

                        rpnl = inv.reset_pnl()
                        assert pnl_price == rpnl, (pnl_price, rpnl) # Sanity check.

                        w_price = Wallet(comm_price, pnl_price)
                        w_target = Wallet(comm_target or comm_price, pnl_target)

                        if not hasattr(post_book, 'amount_orig'):
                            post_book.amount_orig = Wallet()
                        post_book.amount_orig += w_price
                        post_book.amount += w_target
                        post_book.flag = 'B'
                        post_book.note = 'BOOKED'

                        booked[:] = []
                        self.booked_trades.append(btrade)

        self.booked_trades.sort()

        # Note: Maybe we need to include the booking posting if it is not part
        # of the list of transations that makes up a trade.

    def compute_priced_map(self):
        """
        Compute the priced map, that is, the set of commodities that each
        commodity is priced in.
        """
        pmap = defaultdict(set)
        for post in self.postings:
            if post.price is not None:
                assert len(post.amount) == 1
                assert len(post.price) == 1
                pmap[post.amount.keys()[0]].add(post.price.keys()[0])
        self.pricedmap = dict(pmap)

    def check_postings_balance(self, postings):
        """
        Check that the given list of postings balance and automatically fill-in
        for missing ones.
        """
        if not postings:
            return

        # Note: we assume that we've already set the cost to the amount if
        # there was no price defined, so we can just use the cost here (in
        # convert_wallets()).
        cost = Wallet()
        noamount = None
        for post in postings:
            if post.cost is not None:
                cost += post.cost
            else:
                if noamount is None:
                    noamount = post
                else:
                    self.log(CRITICAL, "More than one missing amounts.", post)
                    post.cost = Wallet() # patch it up.

        if noamount:
            # Fill in the missing amount.
            diff = -cost
            noamount.amount = noamount.cost = diff
            cost += diff

        elif bool(cost):
            # If there are only two non-zero commodities, we can simply infer a
            # price between the two and balance automatically. We also store an
            # implicit measure of price.
            if len(cost) == 2:
                it = cost.iteritems()
                com1, amt1 = it.next()
                com2, amt2 = it.next()
                price1 = -amt1/amt2
                price2 = -amt2/amt1
                txn = postings[0].txn
                self.log(WARNING,
                         "Implied price: %s %s/%s  or  %s %s/%s" %
                         (price1, com1, com2, price2, com2, com1), txn)

        # For each commodity, round the cost to a desired precision.
        cost = cost.round()

        if bool(cost):
            txn = postings[0].txn
            self.log(ERROR,
                     "Transaction does not balance: remaining=%s\n%s\n" %
                     (cost.round(), txn),
                     txn)

        ## # Double-check to make sure that all postings in this transaction
        ## # has been normalized.
        ## for post in postings:
        ##     assert post.amount is not None
        ##     assert post.cost is not None

    def visit_preorder(self, node, visitor):
        """
        Visit pre-order all the nodes of the given accounts tree.
        """
        for child in node.children:
            self.visit_preorder(child, visitor)
        visitor(node)

    def visit_postorder(self, node, visitor):
        if visitor(node) is False:
            return
        for child in node.children:
            self.visit_postorder(child, visitor)

    visit = visit_preorder

    def run_directives(self):
        "Run all the directives on the ledger."

        directives = sorted(self.directives.itervalues(),
                            key=attrgetter('prio'))
        for direct in directives:
            direct.apply()

        # We need to re-sort the postings because the directives may have added
        # some out-of-order postings.
        self.build_postings_lists()



    close_flag = 'A'

    def close_books(self, closedate):
        """ Close the books at the specified date 'closedate', replacing all
        entries before that date by opening balances, and resetting
        Income/Revenues and Expenses categories to zero via entries in Equity.
        We also remove trades whose date is before the closed date.
        """
        other_account = self.get_account('Equity:Opening-Balances', create=1)

        # Select all the transactions with date on or after the closing date.
        # This is the set of transactions that we will keep.
        keep_txns = list(txn
                         for txn in self.transactions
                         if txn.actual_date >= closedate)

        # Compute the set of all postings we will keep, the 'in' set (vs.
        # 'out').
        inset = set(post
                    for txn in keep_txns
                    for post in txn.postings)

        # Figure out some accounts to ignore for closing the books (the income
        # statement accounts, mainly).
        income_acc = self.find_account(('Income', 'Revenue', 'Revenues'))
        expenses_acc = self.find_account(('Expenses', 'Expense'))
        imb1_acc = self.find_account(('Imbalance',))
        imb2_acc = self.find_account(('Imbalances',))
        ignore_accounts = filter(None, [income_acc, expenses_acc,
                                        imb1_acc, imb2_acc, other_account])

        # Create automated transactions to replace balances from all the
        # transactions that came before the closing date, transactions which
        # will be removed.
        open_txns = []
        next_ordering = count(1).next
        for acc in self.accounts.itervalues():
            # Ignore income and expenses accounts.
            if any(acc.ischildof(x) for x in ignore_accounts):
                continue

            bal = Wallet()
            for post in acc.postings:
                if post not in inset:
                    bal += post.amount
            if not bal:
                continue

            # Create a transaction to replace the removed postings.
            txn = Transaction()
            txn.ordering = next_ordering()
            txn.actual_date = txn.effective_date = closedate
            txn.flag = self.close_flag
            txn.narration = ("Opening books for account: '%s'" %
                             acc.fullname)

            post = Posting(txn)
            post.ordering = next_ordering()
            txn.postings.append(post)
            post.flag, post.account_name, post.note = txn.flag, acc.fullname, None
            post.account = acc
            post.amount = bal
            post.actual_date = closedate

            # Other side.
            post = Posting(txn)
            post.ordering = next_ordering()
            txn.postings.append(post)
            post.flag, post.account_name, post.note = txn.flag, acc.fullname, None
            post.account = other_account
            post.amount = -bal
            post.actual_date = closedate

            open_txns.append(txn)
            self.log(INFO, " Closing books at %s in %s for %s" %
                     (closedate, acc.fullname, bal), (None, None))

        self.booked_trades = filter(lambda t: t.close_date() >= closedate,
                                    self.booked_trades)

        self.transactions = sorted(open_txns + keep_txns)
        self.build_postings_lists()

    def find_account(self, namelist):
        """ Returns the first account found matching the given name."""
        candidates = []
        for accname in namelist:
            try:
                candidates.append(self.get_account(accname))
            except KeyError:
                pass
        if not candidates:
            return None
        elif len(candidates) > 1:
            raise KeyError("Ambiguous accounts for %s: %s" %
                           (', '.join(namelist),
                            ', '.join(acc.fullname for acc in candidates)))
        else:
            return candidates[0]

    def filter_postings(self, pred):
        """
        Apply the given predicate on all the postings and filter out those for
        which the predicate returns false.

        Important note: as a side-effect, the 'selected' attribute is set to
        true for the nodes that the predicate matches.
        """
        inset = frozenset(filter(pred, self.postings))
        if len(inset) == 0:
            logging.error("No postings selected by predicates.")

        for post in self.postings:
            post.selected = (post in inset)

        if pred is None:
            return

        self.postings[:] = [post for post in self.postings if post in inset]

        for acc in self.accounts.itervalues():
            acc.postings[:] = [post for post in acc.postings if post in inset]

        self.transactions = [txn for txn in self.transactions
                             if any(post in inset for post in txn.postings)]

        self.booked_trades = filter(pred, self.booked_trades)

        self.build_payee_lists()

    def build_payee_lists(self):
        paydict = defaultdict(list)

        for txn in self.transactions:
            if txn.payee:
                payee = re.sub('[^A-Za-z0-9]', '_',
                               ' '.join(txn.payee.lower().split()).encode('ascii',
                                                                          'replace'))
                paydict[payee].append(txn)

        self.payees = {}
        for key, txns in paydict.iteritems():

            # Find the payee name variant with the highest complexity score.
            snames = [(accent_score(txn.payee), txn.payee) for txn in txns]
            snames.sort()
            payee = snames[-1][1]

            self.payees[key] = (payee, txns)





_accents = u'áâàäãéêèëíîìïóôòöõúûùüçøñÁÂÀÄÉÊÈËÍÎÌÏÓÔÒÖÚÛÙÜÇØÑ¡¿'

def accent_score(s):
    """ Return a score for a string that ranks strings in importance whevener
    they have capital letters and accents."""
    acc_upper = sum(1 for c in s if c in string.uppercase)
    acc_score = sum(1 for c in s if c in _accents)
    return acc_upper + acc_score






""" Accounts tree visitors.
"""

class BalanceVisitor(object):
    """
    A visitor that computes the balance of the given account node.
    """
    def __init__(self, aname_bal, aname_total, atcost):
        self.aname_bal = aname_bal
        self.aname_total = aname_total
        self.atcost = atcost

    def __call__(self, node):
        bal = Wallet()
        for post in node.postings:
            assert post.account is node
            bal += post.cost if self.atcost else post.amount
        node.balances[self.aname_bal] = bal
        total = Wallet(bal)
        for child in node.children:
            total += child.balances[self.aname_total]
        node.balances[self.aname_total] = total





""" Directive parsers.
"""

class Check(SimpleDummy):
    attrs = 'cdate account expected commodity filename lineno flag balance'.split()

    def __cmp__(self, other):
        return cmp(self.cdate, other.cdate)

    def passed(self):
        return self.flag != '!'   # '*' or 'A'


class CheckDirective(object):
    """
    Assert that an account has a specific balance at a specific date in a single
    commodity.
    """

    name = 'check'
    prio = 1000

    mre = re.compile("\s*%(date)s\s+(%(account)s)\s+%(amount)s\s*$" %
                     {'date': Ledger.date_re.pattern,
                      'account': Ledger.account_re.pattern,
                      'amount': Ledger.amount_re.pattern})

    def __init__(self, ledger):
        self.checks = []
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid check directive:\n %s" % line.strip(),
                            (filename, lineno))
            return
        cdate = date(*map(int, mo.group(1, 2, 3)))
        account = self.ledger.get_account(mo.group(4), create=1)
        com = mo.group(6)
        amount = (com, Decimal(mo.group(5)))
        expected = Wallet(*amount)
        self.checks.append(Check(cdate, account, expected, com, filename, lineno,
                                 None, expected))

    def apply(self):
        ledger = self.ledger

        for acc in ledger.accounts.itervalues():
            acc.checked = False
            acc.check_min = acc.check_max = None

        for chk in self.checks:
            cdate = chk.cdate
            acc = chk.account
            expected = chk.expected

            acc.checked = True

            balance = Wallet()
            for post in acc.subpostings():
                if post.actual_date <= cdate:  ## Note: shouldn't this be "<" ?
                    balance += post.amount

            # Remove the amounts that we're not supposed to be checking from the
            # actual balance.
            balance = balance.mask_commodity(chk.commodity)

            if chk.flag is None:
                chk.flag = '*' if (balance == expected) else '!'
            chk.balance = balance

            # Note: it is contentious whether we should also round the number
            # specified in the check before making the comparison.
            chk.diff = (balance - expected).round()
            if not chk.passed():
                se = expected or 'nothing'
                sb = balance or 'nothing'
                diff = chk.diff or 'nothing'
                ledger.log(ERROR,
                           ("Check failed at  %s  %s :\n  Got:       %s\n"
                           "  Expecting: %s  \n  Diff: %s\n") %
                           (cdate, acc.fullname, sb, se, diff), chk)

            # Update ranges (no matter what).
            acc.check_min = min(acc.check_min, cdate) if acc.check_min else cdate
            acc.check_max = max(acc.check_max, cdate) if acc.check_max else cdate

    def account_checks(self, acc):
        "Return the list of checks for the given account."
        return sorted(chk for chk in self.checks if chk.account is acc)



class DefineAccountDirective(object):
    """
    Declare a valid account and check that all the postings only use declared
    valid acccounts.
    """

    name = 'defaccount'
    prio = 1

    mre = re.compile(("\s*(D[re]|Cr)\s+(%(account)s)\s+"
                      "((?:%(commodity)s(?:,\s*)?)*)\s*$") %
                     {'account': Ledger.account_re.pattern,
                      'commodity': Ledger.commodity_re.pattern})

    def __init__(self, ledger):
        self.definitions = {}
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid defaccount directive: %s" % line,
                            (filename, lineno))
            return

        isdebit = (mo.group(1) in ('Dr', 'De'))
        account = self.ledger.get_account(mo.group(2), create=1, incrcount=False)
        commodities = mo.group(3).split(',') if mo.group(3) else None
        if account in self.definitions:
            self.ledger.log(CRITICAL,
                            "Duplicate account definition: %s" % account.fullname,
                            (filename, lineno))
        account.commodities = commodities
        account.isdebit = isdebit
        self.definitions[account] = (filename, lineno)

    def apply(self):
        ledger = self.ledger

        # Compute a set of valid account fullnames.
        valid_accounts = set(x.fullname for x in self.definitions)

        # Check that all the postings have a valid account name.
        for post in ledger.postings:
            accname = post.account.fullname
            if accname not in valid_accounts:
                ledger.log(ERROR, "Invalid account name '%s'." % accname, post)

        # Check for unused accounts.
        for acc, (filename, lineno) in sorted(self.definitions.iteritems()):
            if not acc.isused():
                ledger.log(WARNING, "Account %s is unused." % acc.fullname,
                           (filename, lineno))

        # Check that none of the account's postings are in an invalid commodity.
        for accname in valid_accounts:
            acc = self.ledger.get_account(accname)
            if not acc.commodities:
                continue
            for post in acc.postings:
                comms = post.amount.keys()
                if not comms:
                    continue # Empty amount for posting, ignore it.
                comm = comms[0]
                if comm not in acc.commodities:
                    ledger.log(ERROR, "Invalid commodity '%s' for account '%s'." %
                               (comm, accname), post)



class DefineCommodityDirective(object):
    """
    Define a commodity name.
    """

    name = 'defcomm'
    prio = 1

    market_re = re.compile('"?"?')

    mre = re.compile("\s*%(commodity)s\s+([A-Za-z-][A-Za-z0-9:.-]*)\s+(.+)\s*$" %
                     {'commodity': Ledger.commodity_re.pattern})

    def __init__(self, ledger):
        self.commnames = {}
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid defcomm directive: %s" % line,
                            (filename, lineno))
            return

        market, name = mo.group(2, 3)
        if market == '-':
            market = None
        self.commnames[mo.group(1)] = (market, name)

    def apply(self):
        pass



class BeginTagDirective(object):
    """
    Set a page attribute to the transactions between beginpage and endpage
    directives.
    """
    name = 'begintag'
    prio = 1

    def __init__(self, ledger):
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        ledger = self.ledger

        tag = line.strip()
        ledger.tags = list(ledger.tags) + [tag]
        # Note: it is important to make a copy of 'ledger.tags' here because
        # this is getting reference by postings many many times.

    def apply(self):
        # Nothing to do: the tag has been set on the transaction objects during
        # parsing.
        pass

class EndTagDirective(BeginTagDirective):
    name = 'endtag'

    def parse(self, line, filename, lineno):
        ledger = self.ledger
        tag = line.strip()
        assert tag in ledger.tags, (tag, ledger.tags)
        ledger.tags = list(ledger.tags) # copy
        ledger.tags.remove(tag)





class AutoPad(object):
    "Representation of a @pad directive (and its temporary data)."
    def __init__(self, pdate, acc_target, acc_offset, filename, lineno):
        self.pdate = pdate
        self.acc_target = acc_target
        self.acc_offset = acc_offset
        self.filename, self.lineno = filename, lineno

        # The set of commodities that have been adjusted for this pad. (This is
        # used to make only one check per commodity affect each pad.)
        self.adjusted = dict() # commodity -> check
        self.wallet = Wallet()



class AutoPadDirective(object):
    """
    Automatically insert an opening balance before any of the transactions
    before an existing account, to make the first check work. Insert a directive
    like this to automatically insert an entry to balance an account::

      @openbal  Assets:Current:RBC:Checking  Equity:Opening-Balances

    This inserts a transaction before the first transaction in the checking
    account and offsets it with the transaction in the opening balances.
    """

    name = 'pad'
    prio = 2

    flag = 'A'

    mre = re.compile("\s*(?:%(date)s)\s+(%(account)s)\s+(%(account)s)\s*$" %
                     {'date': Ledger.date_re.pattern,
                      'account': Ledger.account_re.pattern})

    def __init__(self, ledger, checkdir):
        self.pads = []
        self.ledger = ledger
        self.checkdir = checkdir

        # A record of the transactions we added.
        self.transactions = []

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid pad directive:\n %s" % line.strip(),
                            (filename, lineno))
            return

        pdate = date(*map(int, mo.group(1, 2, 3)))
        try:
            acc_target = self.ledger.get_account(mo.group(4))
            acc_offset = self.ledger.get_account(mo.group(5))
        except KeyError, e:
            self.ledger.log(CRITICAL, "Invalid account: %s" % e,
                            (filename, lineno))
            return

        pad = AutoPad(pdate, acc_target, acc_offset, filename, lineno)
        self.pads.append(pad)

    def apply(self):
        ledger = self.ledger

        # Arrange pads by target account, then sort them, and deal with them
        # thereafter as such.
        padacc = defaultdict(list)
        for x in self.pads:
            padacc[x.acc_target].append(x)

        for acc_target, pads in padacc.iteritems():

            # Get the list of checks for this account.
            checks = self.checkdir.account_checks(acc_target)
            if not checks:
                continue

            # Merge the account's postings, pads and check and sort them
            # together using a Schwartzian transform with appropriate priorities
            # to disambiguate cases where the date is equal.
            slist = ([((pad.pdate, 0), pad) for pad in pads] +
                     [((chk.cdate, 1), chk) for chk in checks] +
                     [((post.actual_date, 2), post)
                      for post in acc_target.subpostings()])
            slist.sort()

            # The current pad, and a set of the commodities that have already
            # been adjusted for it.
            pad = None
            balance = Wallet()
            for sortkey, x in slist:

                if isinstance(x, AutoPad):
                    # Make this pad the current pad.
                    pad = x

                elif isinstance(x, Check):
                    # Ajust the current pad to reflect this check, if it has not
                    # already been adjusted.
                    chk = x
                    if (pad is not None and
                        chk.commodity not in pad.adjusted):

                        pad.adjusted[chk.commodity] = chk
                        diff = chk.expected - balance
                        balamount = diff.get(chk.commodity)
                        if balamount:
                            pad.wallet[chk.commodity] = balamount

                            # Mark check as having been padded.
                            chk.flag = self.flag

                elif isinstance(x, Posting):
                    post = x
                    balance += post.amount

                else:
                    raise ValueError("Invalid type in list.")

            for pad in pads:
                if not pad.wallet:
                    logging.warning("Ununsed pad at %s:%d" %
                                    (pad.filename, pad.lineno))
                    continue

                txn = Transaction()
                ledger.transactions.append(txn)
                txn.actual_date = txn.effective_date = pad.pdate
                txn.filename = pad.filename
                txn.lineno = 0
                txn.ordering = 0
                txn.flag = self.flag
                txn.payee = self.__class__.__name__

                chkstr = ', '.join('%s:%d' % (chk.filename, chk.lineno)
                                   for chk in pad.adjusted.itervalues())
                txn.narration = u'Automatic opening balance for checks: %s' % chkstr


                for com, num in pad.wallet.iteritems():
                    for acc, anum in ((pad.acc_target, num),
                                      (pad.acc_offset, -num)):

                        # Let's install one posting per commodity, because the input
                        # format does not allow more than that either (it would
                        # work, but we just don't want to break the
                        # 1-posting/1-commodity hypothesis).
                        post = Posting(txn)
                        post.filename, post.lineno = pad.filename, pad.lineno
                        post.actual_date = post.effective_date = txn.actual_date
                        post.ordering = 0
                        post.flag = self.flag
                        post.account_name = acc.fullname
                        post.account = acc
                        txn.postings.append(post)
                        ledger.postings.append(post)
                        acc.postings.append(post)
                        post.amount = post.cost = Wallet(com, anum)

                ledger.log(INFO, "Inserting automatic padding for %s at %s for %s" %
                           (acc_target.fullname, pad.pdate.isoformat(), pad.wallet),
                           (pad.filename, pad.lineno))



class DefvarDirective(object):
    """
    A directive that can be used to define generic parameters for specialized
    applications. For example, the import scripts make use of this directive in
    order to fetch some custom information from the ledger file. The format of
    the variables is generic::

        @defvar MODULE VARNAME VALUE

    VALUE is only interpreted as a string. There can be multiple definitions of
    the same variable (they are accumulated as a list).
    """

    name = 'var'
    prio = 2000

    mre = re.compile("\s*(?P<module>[a-zA-Z0-9]+)"
                     "\s+(?P<varname>[a-zA-Z0-9]+)"
                     "\s+(?P<value>.+)\s*$")

    def __init__(self, ledger):

        self.modules = defaultdict(lambda: defaultdict(list))
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid check directive: %s" % line,
                            (filename, lineno))
            return

        self.modules[mo.group('module')][mo.group('varname')].append(
            mo.group('value'))

    def apply(self):
        pass # Nothing to do--the modules do the parsing themselves.

def read_ofx_accounts_map(ledger):
    """
    Process account mapping declarations from the ledger file and return a
    mapping.
    """
    m = {}
    vardir = ledger.directives['var']
    accids = vardir.modules['ofx']['accid']
    for decl in accids:
        accid, accname = [x.strip() for x in decl.split()]
        try:
            acc = ledger.get_account(accname)
        except KeyError:
            raise SystemExit(
                "Could not find account %s\n  @var declaration: %s\n" %
                (accname, decl))
        m[accid] = acc
    return m



class LocationDirective(object):
    """
    A directive that can be used to provide an update on the physical location
    of the main spender in a personal account. This is used to calculate the
    number of days spent in each country for each civil year, in order to

    - declare it to customs if needed (with precision)
    - declare it to the Canadian medicare plans.
    """

    name = 'location'
    prio = 2000

    mre = re.compile(("\s*(?:%(date)s)"
                      "\s+(?P<city>[^,]+)\s*,"
                      "\s+(?P<country>.+)\s*$") %
                     {'date': Ledger.date_re.pattern})

    def __init__(self, ledger):
        self.locations = []
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid location directive: %s" % line,
                            (filename, lineno))
            return

        ldate = date(*map(int, mo.group(1, 2, 3)))
        self.locations.append( (ldate, mo.group(4), mo.group(5)) )

    def apply(self):
        pass

        ## # If the ledger appears to be current, append a date for today in order
        ## # to count those days as well.
        ## if not self.locations:
        ##     return
        ## lastloc = sorted(self.locations)[-1]
        ## today = date.today()
        ## if lastloc[0] < today:
        ##     self.locations.append( (today, lastloc[1], lastloc[2]) )


class PriceDirective(object):
    """
    Define prices and exchange rates between two commodities. This is used to
    build an internal price history database that can be used for other
    purposes, for example, converting all values from one commodity to another
    in the reports, and for computing capital gains in a currency different from
    the underlying's rice currency.

    Note that we don't include this feature with the intention of providing
    detailed time-series of prices. Instead, we expect that the user will simply
    enter some of the prices, where relevant for his application.
    """

    name = 'price'
    prio = 100

    mre = re.compile(("\s*(?:%(date)s)"
                      "\s+(?:%(commodity)s)"
                      "\s+%(amount)s"
                      ) %
                     {'date': Ledger.date_re.pattern,
                      'commodity': Ledger.commodity_re.pattern,
                      'amount': Ledger.amount_re.pattern})

    def __init__(self, ledger):
        self.ledger = ledger

        # A map of (from, to) commodity pairs to PriceHistory objects.
        self.prices = defaultdict(PriceHistory)

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            self.ledger.log(CRITICAL, "Invalid price directive: %s" % line,
                            (filename, lineno))
            return

        ldate = date(*map(int, mo.group(1, 2, 3)))
        base, quote = mo.group(4,6)
        rate = Decimal(mo.group(5))
        phist = self.prices[(base, quote)]
        if phist.finddate(ldate) is not None:
            self.ledger.log(ERROR, "Duplicate price: %s" % line,
                            (filename, lineno))
            return
        else:
            phist.append( (ldate, rate) )
            phist.sort()

    def apply(self):
        # FIXME: This ends up happening too late, we need it beforehand.
        # Sort all the price histories only once.
        for phist in self.prices.itervalues():
            phist.sort()

    def getrate(self, base, quote, date_):
        return self.prices[(base, quote)].interpolate(date_)


class PriceHistory(list):
    """ An object that can accumulate samples of price history and calculate
    interpolated values."""

    def __init__(self):
        list.__init__(self)
        self.m = {}

    def append(self, el):
        assert isinstance(el, tuple)
        assert isinstance(el[0], date)
        assert isinstance(el[1], Decimal)
        self.m[el[0]] = el[1]
        return list.append(self, el)

    def finddate(self, d):
        "Return the rate at precisely date 'd'."
        return self.m.get(d, None)

    def interpolate(self, date_):
        "Linear interpolation of the rate, given our samples."
        if len(self) == 0:
            raise IndexError("Cannot interpolate empty price history.")
        idx = bisect_left(self, (date_, None))

        # Edge cases.
        res = None
        if idx == len(self):
            d, r = self[idx-1]
            res = r
        elif idx == 0:
            d, r = self[0]
            res = r

        # Interpolate between the two closest dates.
        if res is None:
            idx -= 1
            d1, r1 = self[idx]
            d2, r2 = self[idx+1]
            assert d1 < date_, (d1, date_)
            assert d2 >= date_, (d2, date_)
            alpha = Decimal((date_ - d1).days) / Decimal((d2 - d1).days)
            res = (1-alpha)*r1 + (alpha)*r2

        return res



