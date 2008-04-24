"""
Main file and data models for the Python version of Ledger.
"""

# stdlib imports
import sys, os, logging, re, codecs
import cPickle as pickle
from decimal import Decimal, getcontext
from datetime import date, timedelta
from os.path import *
from operator import attrgetter
from itertools import count, izip, chain, repeat
from StringIO import StringIO
from bisect import bisect_left
from collections import defaultdict

# other imports
from namedtuple import namedtuple

# beancount imports
from beancount.wallet import Wallet
from beancount.utils import SimpleDummy


__all__ = ('Account', 'Transaction', 'Posting', 'Ledger',
           'CheckDirective',
           'compute_balsheet')

oneday = timedelta(days=1)


def init_wallets(py_wallets):
    """
    Initialize globals for precision calculations.
    """
    # Set the representational precision of Decimal objects.
    ctx = getcontext()
    ctx.prec = 16

init_wallets(1)



class Account(object):
    """
    An account object.
    """
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

    def __str__(self):
        return "<Account '%s'>" % self.fullname
    __repr__ = __str__

    def isroot(self):
        return self.fullname == ''

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
            return Account.sep.join((self.parent.get_fullname(), self.name)).lstrip(':')

    def issubaccount(self, other):
        "Return true if the 'other' account is a subaccount of this one."
        # We do this by name for now.
        return other.name.startswith(self.name)



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

    # The vector sum of the various postings here-in contained.
    wallet = None

    def __init__(self):
        # The list of contained postings.
        self.postings = []

    def description(self):
        return Ledger.payee_sep.join(filter(None, (self.payee, self.narration)))

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
    price = None
    lotprice = None
    note = None

    def __init__(self, txn):
        self.txn = txn

    def __str__(self):
        return '  %-70s %s' % (self.account_name or self.account.name, self.amount)
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



class Ledger(object):
    """
    A ledger object, that contains transactions and directives and all data
    related to the construction of a single ledger, its list of commodities,
    etc.
    """
    def __init__(self):

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
        
        # A map of directive-name to contents.
        self.directives = {}
        add_directive = lambda x: self.directives.__setitem__(x.name, x)
        check = CheckDirective(self)
        add_directive(check)
        add_directive(DefineAccountDirective(self))
        add_directive(AutoPad(self, check))

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

    def log_message(self, level, message, obj):
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

        msg = LedgerMessage(level, message, filename, lineno)
        self.messages.append(msg)
        logging.log(level, '%s:%d: %s' % (filename, lineno, message))


    # Account ordering integer.
    acc_ordering = count().next

    def get_account(self, name, create=False):
        """
        Return or create an account by name, creating all the intermediate account
        tree nodes as well.
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
    notedate_re = re.compile('\[(?:%(date)s)?(?:=%(date)s)?\]' % {'date': date_re.pattern})

    # Pattern for a transaction line.
    payee_sep = ' | '
    txn_re = re.compile('^%(date)s(=%(date)s)?\s+(?:(.)\s+)?(\(.*?\))?(.*)$' %
                        {'date': date_re.pattern})

    # Pattern for an amount.
    commodity_re = re.compile('"?([A-Za-z][A-Za-z0-9]*)"?')
    amount_re = re.compile('([-+]?\d*(?:\.\d*)?)\s+%(comm)s' %
                           {'comm': commodity_re.pattern})

    # Pattern for an account (note: we don't allow spaces in this version).
    account_re = re.compile('[:A-Za-z0-9-_]+')
    postaccount_re = re.compile('(?:%(accname)s|\[%(accname)s\]|\(%(accname)s\))' %
                                {'accname': account_re.pattern})

    # Pattern for a posting line (part of a transaction).
    posting_re = re.compile(
        ('\s+([*!]\s+)?(%(account)s)(?:\s+%(amount)s)?(?:\s+{\s*%(amount)s\s*})?'
         '(?:\s+@(@?)(?:\s+%(amount)s))?\s*(?:;(.*))?\s*$') %
        {'amount': amount_re.pattern, 'account': postaccount_re.pattern})

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
        append_txn = self.transactions.append
        all_postings = self.postings

        xread = f.readline
        lineno = [0]
        def nextline():
            lineno[0] += 1
            line = xread()
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
                    append_txn(txn)

                    actual_date = date(*map(int, mo.group(1, 2, 3)))

                    if mo.group(4):
                        effective_date = date(*map(int, mo.group(5, 6, 7)))
                    else:
                        effective_date = actual_date

                    txn.actual_date = actual_date
                    txn.effective_date = effective_date

                    txn.flag = mo.group(8)
                    txn.code = mo.group(9)

                    g = mo.group(10).split(Ledger.payee_sep, 1)
                    if len(g) == 1:
                        txn.narration = g[0]
                    else:
                        assert len(g) == 2
                        txn.payee, txn.narration = g

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
                            all_postings.append(post)

                            post.flag, post.account_name, post.note = mo.group(1,2,10)

                            # Remove the modifications to the account name.
                            accname = post.account_name
                            if accname[0] in '[(':
                                accname = accname.strip()[1:-1]
                                post.virtual = VIRT_BALANCED if accname[0] else VIRT_UNBALANCED
                            post.account = acc = self.get_account(accname, create=1)

                            acc.postings.append(post)

                            amount = mo.group(3,4)
                            price = mo.group(8,9)
                            lotprice = mo.group(5,6)
                            post.price_complete = bool(mo.group(7))

                            if amount[0] is not None:
                                anum = Decimal(amount[0])
                                acom = amount[1]
                                post.amount = Wallet(acom, anum)
                                add_commodity(acom)
                            else:
                                post.amount = None

                            if price[0] is not None:
                                pcom = price[1]
                                pnum = Decimal(price[0])
                                post.price = Wallet(pcom, pnum)
                                add_commodity(pcom)
                            else:
                                post.price = None

                            if lotprice[0] is not None:
                                lcom = lotprice[1]
                                lnum = Decimal(lotprice[0])
                                post.lotprice = Wallet(lcom, lnum)
                                add_commodity(lcom)
                            else:
                                post.lotprice = None

                            # Compute the cost, if possible.
                            if post.lotprice is not None:
                                post.cost = Wallet(lotprice[1], anum * lnum)
                            elif post.price is not None:
                                post.cost = Wallet(price[1], anum * pnum)
                            else:
                                post.cost = post.amount
                                if post.cost is not None:
                                    post.cost = Wallet(post.cost)

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
                                        post.effective_date = date(*map(int, effective))

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
                    except KeyError, e:
                        logging.warning("Unknown directive %s: %s" % (direc, str(e)))
                    line = nextline()
                    continue

                # Parse a directive.
                mo = match_special(line)
                if mo:
                    logging.warning("Directive %s not supported." % mo.group(1))
                    line = nextline()
                    continue

                # Parse a directive.
                mo = match_command(line)
                if mo:
                    logging.warning("Command %s not supported." % mo.group(1))
                    line = nextline()
                    continue

                self.log_msg("%s:%d : Cannot recognize syntax: %s" % (fn, lineno[0], line))

                logging.error("%s:%d : Cannot recognize syntax: %s" % (fn, lineno[0], line))
                line = nextline()

        except StopIteration:
            pass

        # Set the precision map according to some rules about the commodities.
        roundmap = Wallet.roundmap
        for com in self.commodities:
            prec = 2 if (len(com) == 3) else 3
            roundmap[com] = Decimal(str(10**-prec))

        self.complete_balances()

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
                    logging.warning("%s:%d: Virtual posting without amount has no effect." %
                                    (post.filename, post.lineno))
                    post.amount = post.cost = Wallet()

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
                    logging.error("%s:%d: more than one missing amounts." %
                                  (post.filename, post.lineno))
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
                logging.warning("%s:%d: Implied price: %s %s/%s  or  %s %s/%s" %
                                (txn.filename, txn.lineno,
                                 price1, com1, com2, price2, com2, com1))

        # For each commodity, round the cost to a desired precision.
        cost = cost.round()

        if bool(cost):
            txn = postings[0].txn
            logging.error("%s:%d: Transaction does not balance: Cost: %s" %
                          (txn.filename, txn.lineno, cost.round()))

        # Double-check to make sure that all postings in this transaction
        # has been normalized.
        for post in postings:
            assert post.amount is not None
            assert post.cost is not None


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




""" Accounts tree visitors.
"""

def compute_balsheet(ledger, aname_bal, aname_total, atcost=False):
    """
    Compute a balance sheet stored in the given attribute on each account
    node.
    """
    vis = BalanceVisitor(aname_bal, aname_total, atcost)
    ledger.visit(ledger.get_root_account(), vis)

class BalanceVisitor(object):
    """
    A visitor that computes the balance of the given node.
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
        setattr(node, self.aname_bal, bal)
        total = Wallet(bal)
        for child in node.children:
            total += getattr(child, self.aname_total)
        setattr(node, self.aname_total, total)





""" Directive parsers.
"""

class Check(SimpleDummy):
    attrs = 'cdate account expected filename lineno flag balance'.split()

    def __cmp__(self, other):
        return cmp(self.cdate, other.cdate)

    def passed(self):
        return self.flag != '!'   # '*' or 'A'


class CheckDirective(object):
    """
    Assert that an account has a specific balance at a specific date in a single
    commodity.
    """

    name = 'assert'
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
            raise ValueError("%s:%d : Invalid check directive: %s" %
                             (filename, lineno, line))
        cdate = date(*map(int, mo.group(1, 2, 3)))
        account = self.ledger.get_account(mo.group(4), create=1)
        com = mo.group(6)
        amount = (com, Decimal(mo.group(5)))
        expected = Wallet(*amount)
        self.checks.append(Check(cdate, account, expected, filename, lineno,
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
                if post.actual_date <= cdate:
                    balance += post.amount

            # Remove the amounts that we're not supposed to be checking from the
            # actual balance.
            balance = balance.mask(expected)

            if chk.flag is None:
                chk.flag = '*' if (balance == expected) else '!'
            chk.balance = balance

            # Note: it is contentious whether we should also round the number
            # specified in the check before making the comparison.
            if not chk.passed():
                se = expected or 'nothing'
                sb = balance or 'nothing'
                diff = (balance - expected).round() or 'nothing'
                logging.error(
                    "%s:%d : Balance check failed at  %s  %s :\n  Got:       %s\n  Expecting: %s  \n  Diff: %s\n" %
                    (chk.filename, chk.lineno, cdate, acc.fullname, sb, se, diff))

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

    mre = re.compile("\s*(De|Cr)\s+(%(account)s)\s+(?:%(commodity)s,?)*\s*$" %
                     {'account': Ledger.account_re.pattern,
                      'commodity': Ledger.commodity_re.pattern})

    def __init__(self, ledger):
        self.definitions = []
        self.ledger = ledger

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            raise ValueError("Invalid defaccount directive: %s" % line)

        isdebit = (mo.group(1) == 'De')
        account = self.ledger.get_account(mo.group(2), create=1)
        commodities = mo.group(3).split(',') if mo.group(3) else None
        self.definitions.append((account, commodities, isdebit))

    def apply(self):
        ledger = self.ledger

        # Compute a set of valid account fullnames.
        valid_accounts = set(x[0].fullname for x in self.definitions)

        # Check that all the postings have a valid account name.
        for post in ledger.postings:
            accname = post.account.fullname
            if accname not in valid_accounts:
                logging.error("%s:%d : Invalid account name '%s'." %
                              (post.filename, post.lineno, accname))



class AutoPad(object):
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
        self.openings = []
        self.ledger = ledger
        self.checkdir = checkdir

        # A record of the transactions we added.
        self.transactions = []

    def parse(self, line, filename, lineno):
        mo = self.mre.match(line)
        if not mo:
            raise ValueError("Invalid openbal directive: %s" % line)

        pad_date = date(*map(int, mo.group(1, 2, 3)))
        acc_target = self.ledger.get_account(mo.group(4))
        acc_offset = self.ledger.get_account(mo.group(5))

        self.openings.append((pad_date, acc_target, acc_offset, filename, lineno))

    def apply(self):
        ledger = self.ledger

        for pad_date, acc_target, acc_offset, fn, lineno in self.openings:
            checks = self.checkdir.account_checks(acc_target)
            if not checks:
                logging.error("%s:%d: cannot automatically open a balance "
                              "if there is no check for account %s." %
                              (fn, lineno, acc_target.fullname))
                continue

            # Find the checks that come before and after the pad date.
            chk_before = chk_after = None
            for chk in checks:
                if chk.cdate < pad_date:
                    if chk_before is None or chk.cdate > chk_before.cdate:
                        chk_before = chk
                else:
                    if chk_after is None or chk.cdate < chk_after.cdate:
                        chk_after = chk

            if chk_after is None:
                logging.error("%s:%d: cannot pad beyond the last check in account %s." %
                              (fn, lineno, acc_target.fullname))
                continue

            # Sum the balance between the checks.
            balance = chk_before.expected if chk_before is not None else Wallet()
            postings = acc_target.postings
            for post in postings:
                if chk_before is not None and post.actual_date < chk_before.cdate:
                    continue
                if post.actual_date >= chk_after.cdate:
                    continue
                balance += post.amount

            missing = chk_after.expected - balance
            if missing.isempty():
                if chk_before is not None:
                    logging.warning("%s:%d: unnecessary padding for account %s." %
                                    (fn, lineno, acc_target.fullname))
                continue

            # Actually do insert a new transaction!
            if chk_before is not None:
                chk_before.flag = self.flag
            chk_after.flag = self.flag

            txn = Transaction()
            ledger.transactions.append(txn)
            txn.actual_date = txn.effective_date = pad_date
            txn.filename = '<openbal>'
            txn.lineno = 0
            txn.ordering = 0
            txn.flag = self.flag
            txn.payee = self.__class__.__name__

            if chk_before is not None:
                txn.narration = (
                    u'Automatic opening balance for checks %s:%d and %s:%d' %
                    (chk_before.filename, chk_before.lineno,
                     chk_after.filename, chk_after.lineno))
            else:
                txn.narration = (
                    u'Automatic opening balance for check %s:%d' %
                    (chk_after.filename, chk_after.lineno))

            for acc, amount in ((acc_target, missing),
                                (acc_offset, -missing)):
                post = Posting(txn)
                post.filename, post.lineno = '<openbal>', 0
                post.actual_date = post.effective_date = txn.actual_date
                post.ordering = 0
                post.flag = self.flag
                post.account_name = acc.fullname
                post.account = acc
                txn.postings.append(post)
                ledger.postings.append(post)
                acc.postings.append(post)
                post.amount = post.cost = amount

            logging.info("%s: inserting opening balance at %s for %s" %
                         (self.__class__.__name__, pad_date.isoformat(), missing))




def filter_inout(tlist, pred):
    "Split the list in two according to the given predicate."
    list_in, list_out = [], []
    [(list_in if pred(el) else list_out).append(el) for el in tlist]
    return list_in, list_out




LedgerMessage = namedtuple('Message', ('level', 'message', 'filename', 'lineno'))
