# Copyright (C) 2009-2012  Martin Blais, Inc. All Rights Reserved.
"""
Protocol parser.
"""
import sys
import logging
from os.path import dirname
from collections import namedtuple, defaultdict
from operator import attrgetter
from itertools import chain

import ply.yacc as yacc
import ply.lex as lex

__all__ = ('create_parser',)



#--------------------------------------------------------------------------------
# Lexer


tokens = """
  ERROR INDENT EOL EOF
  COMMENT
  PIPE ATAT AT LCURL RCURL EQUAL COMMA SLASH TXNFLAG
  TXN CHECK OPEN CLOSE PAD EVENT PRICE LOCATION NOTE
  BEGINTAG ENDTAG
  DATE ACCOUNT CURRENCY STRING NUMBER
""".split()


# Whitespace counts as an 'INDENT' token, but only if it is found at the
# beginning of a line.
def t_INDENT(t):
    r'[ \t\r]+'
    if find_column(t) == 0:
        return t

def t_EOL(t):
    r'\n'
    t.lexer.lineno += 1
    return t

t_PIPE = r'\|'
t_ATAT = r'@@'
t_AT = r'@'
t_LCURL = r'{'
t_RCURL = r'}'
t_EQUAL = r'='
t_COMMA = r','
t_SLASH = r'/'

def t_TXNFLAG(t):
    r'[*!&#?%]'
    if find_column(t) != 0:
        return t
    else:
        # If not at the beginning of a line, skip the rest of the line (to support org-mode).
        pos = t.lexer.lexpos
        while t.lexer.lexdata[pos] != '\n':
            pos += 1
        t.lexer.lexpos = pos + 1

t_TXN = r'txn'
t_CHECK = r'check'
t_OPEN = r'open'
t_CLOSE = r'close'
t_PAD = r'pad'
t_EVENT = r'event'
t_PRICE = r'price'
t_LOCATION = r'location'
t_NOTE = r'note'

t_BEGINTAG = r'begintag'
t_ENDTAG = r'endtag'

# Note: the following are declared in order to establish an ordering; this is needed here.

def t_DATE(t):
    r'(\d\d\d\d)[-/](\d\d)[-/](\d\d)'
    return t

def t_ACCOUNT(t):
    r'(([A-Z][A-Za-z0-9\-]+):)+([A-Z][A-Za-z0-9\-]+)'
    assert t.value.split(':')[0] in ('Assets', 'Liabilities', 'Equity', 'Income', 'Expenses', 'Misc')
    return t

def t_CURRENCY(t):
    r"[A-Z][A-Z0-9'\.]{1,10}"
    return t

def t_STRING(t):
    r'(".*?")'
    return t

def t_NUMBER(t):
    r'[-+]?([0-9\.])+'
    return t

def t_comment(t):
    r';+(.*)'

def t_error(t):
    print("Illegal character %s" % repr(t.value[0]))
    t.lexer.skip(1)



def find_column(t):
    """Compute column.
        input is the input text string
        token is a token instance
    """
    last_cr = t.lexer.lexdata.rfind('\n', 0, t.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (t.lexpos - last_cr) - 1
    return column



#--------------------------------------------------------------------------------
# Grammar

def p_empty(t):
    'empty : '

def p_txn(p):
    """txn : TXN
           | TXNFLAG
    """
    #varlen(p)

def p_date(p):
    """date : DATE
    """
    p[0] = p[1] # FIXME: parse the date here

def p_date_pair(p):
    """date_pair : date
                 | date EQUAL date
    """
    p[0] = p[1] # Ignore the other date for now.

def p_transaction(p):
    """transaction : date_pair txn STRING EOL posting_list
                   | date_pair txn STRING PIPE STRING EOL posting_list
    """
    #varlen(p)

def p_optflag(p):
    """optflag : empty
               | TXNFLAG
    """
    p[0] = p[1] if len(p) > 1 else None

def p_account(p):
    """account : ACCOUNT
    """
    p[0] = p[1]

Posting = namedtuple('Posting', 'account lot')

def p_posting(p):
    """posting : INDENT optflag account amount_lot EOL
               | INDENT optflag account amount_lot AT amount EOL
               | INDENT optflag account amount_lot ATAT amount EOL
               | INDENT optflag account EOL
               | INDENT EOL
    """
    # Ignore indented empty lines. Note: we cannot do this in a separate rules
    # because of the possible shift/reduce conflict.
    if len(p) == 3:
        return
    p[0] = Posting(p[2], p[3])

def p_posting_list(p):
    """posting_list : empty
                    | posting_list posting
    """
    handle_varlen(p)

def p_currency_list(p):
    """currency_list : empty
                     | CURRENCY
                     | currency_list COMMA CURRENCY
    """
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]] if (p[1] is not None) else []

def p_begintag(p):
    """begintag : BEGINTAG STRING
    """

def p_endtag(p):
    """endtag : ENDTAG STRING
    """

# FIXME: Add account id.
Open = namedtuple('Open', 'date account currency_list accountid')
Close = namedtuple('Close', 'date account')

def p_open(p):
    """open : date OPEN ACCOUNT currency_list
            | date OPEN ACCOUNT STRING currency_list
    """
    if len(p) == 6:
        accountid, currencies = p[4], p[5]
    else:
        accountid, currencies = None, p[4]
    p[0] = Open(p[1], p[3], currencies, accountid)

def p_close(p):
    """close : date CLOSE ACCOUNT
    """
    p[0] = Close(p[1], p[3])

Pad = namedtuple('Pad', 'date account_to account_from')

def p_pad(p):
    """pad : date PAD ACCOUNT ACCOUNT
    """
    p[0] = Pad(*p[2:])

Check = namedtuple('Check', 'date account amount')

def p_check(p):
    """check : date CHECK ACCOUNT amount
    """
    p[0] = Check(*p[2:])

Amount = namedtuple('Amount', 'number currency')

def p_amount(p):
    """amount : NUMBER CURRENCY
    """
    p[0] = Amount(p[1], p[2])

Lot = namedtuple('Lot', 'amount cost lotdate')

def p_amount_lot(p):
    """amount_lot : amount
                  | amount lot
    """
    if len(p) == 2:
        p[0] = Lot(p[1], None, None)
    elif len(p) == 3:
        cost, lotdate = p[2]
        p[0] = Lot(p[1], cost, lotdate)

def p_lot(p):
    """lot : LCURL amount RCURL
           | LCURL amount SLASH date RCURL
    """
    p[0] = (p[2], (p[4] if len(p) > 4 else None))

Price = namedtuple('Price', 'price date currency amount')

def p_price(p):
    """price : date PRICE CURRENCY amount
    """
    p[0] = Price(*p[1:])

Location = namedtuple('Location', 'date city')

def p_location(p):
    """location : date LOCATION STRING
    """
    p[0] = Location(p[1], p[3])

Event = namedtuple('Event', 'date event_type event_description')

def p_event(p):
    """event : date EVENT STRING STRING
    """
    p[0] = Event(p[1], p[3], p[4])

Note = namedtuple('Note', 'date note')

def p_note(p):
    """note : date NOTE STRING
    """
    p[0] = Note(p[1], p[2])

def p_entry(p):
    """entry : EOL
             | transaction
             | check
             | open
             | close
             | pad
             | event
             | note
             | price
             | location
    """
    p[0] = p[1]

def p_directive(p):
    """directive : entry
                 | begintag
                 | endtag
    """
    p[0] = p[1]

def p_directives(p):
    """directives : empty
                  | directives directive
    """
    handle_varlen(p)

def p_error(p):
    if p is None:
        return logging.error("%s:%d: Error in input." % ('', 0))
    logging.error("%s:%d: Syntax error" % (p.lexer.filename, p.lineno))

start = 'directives'



def handle_varlen(p):
    "Do what needs to be done to handle a variable length sequence."
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]] if p[1] else []



#-------------------------------------------------------------------------------
# Create/Run Code

def create_parser(debug=None, optimize=None):
    lexer = lex.lex(debug=debug, optimize=optimize)
    parser = yacc.yacc(method='LALR', debug=debug, outputdir=dirname(__file__))
    parser.__lexer = lexer
    return lexer, parser
