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
    INDENT
    DATE NUMBER STRING
    ACCOUNT DRCR CURRENCY
    DEFACCOUNT VAR PAD CHECK BEGINTAG ENDTAG PRICE LOCATION
    TXNFLAG TXN OFX ACCID
    AT ATAT PIPE EQUAL EOL LCURL RCURL COMMA
""".split()


# Whitespace counts as an 'INDENT' token, but only if it is found at the
# beginning of a line.
def t_INDENT(t):
    r'[ \t\r]+'
    if find_column(t) == 0:
        return t

t_TXN = r'txn'
t_OFX = r'ofx'
t_ACCID = r'accid'

t_PIPE = r'\|'
t_ATAT = r'@@'
t_AT = r'@'
t_LCURL = r'{'
t_RCURL = r'}'
t_EQUAL = r'='
t_COMMA = r','
t_TXNFLAG = r'[*\!&R#c\?SP]'

def t_EOL(t):
    r'\n'
    t.lexer.lineno += 1
    return t

t_DEFACCOUNT = r'@defaccount'
t_VAR = r'@var'
t_PAD = r'@pad'
t_CHECK = r'@check'
t_BEGINTAG = r'@begintag'
t_ENDTAG = r'@endtag'
t_PRICE = r'@price'
t_LOCATION = r'@location'

def t_DATE(t):
    r'(\d\d\d\d)[-/](\d\d)[-/](\d\d)'
    return t

# Note: define these as functions in order to establish an ordering; this is
# needed here.
def t_ACCOUNT(t):
    r'(([A-Z][A-Za-z0-9\-]+):)+([A-Z][A-Za-z0-9\-]+)'
    assert t.value.split(':')[0] in ('Assets', 'Liabilities', 'Equity', 'Income', 'Expenses', 'Misc')
    return t

def t_CURRENCY(t):
    r"[A-Z][A-Z0-9'\.]{1,10}"
    return t

def t_DRCR(t):
    r'(Dr|Cr)'
    return t

def t_STRING(t):
    r'(".*?")'
    return t

def t_NUMBER(t):
    r'[-+]?([0-9\.])+'
    return t

def t_COMMENT(t):
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

def p_dates(p):
    """dates : DATE
             | DATE EQUAL DATE
    """
    p[0] = p[1] # Ignore the other date for now.

def p_transaction(p):
    """transaction : dates txn STRING EOL posting_list
                   | dates txn STRING PIPE STRING EOL posting_list
    """
    #varlen(p)


def p_optflag(p):
    """optflag : empty
               | TXNFLAG
    """
    p[0] = p[1] if len(p) > 1 else None

Posting = namedtuple('Posting', 'account lot')

def p_posting(p):
    """posting : INDENT optflag ACCOUNT lot EOL
               | INDENT optflag ACCOUNT lot AT amount EOL
               | INDENT optflag ACCOUNT lot ATAT amount EOL
               | INDENT optflag ACCOUNT EOL
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
    handle_varlen(p)

def p_begintag(p):
    """begintag : BEGINTAG STRING
    """

def p_endtag(p):
    """endtag : ENDTAG STRING
    """

VarAccid = namedtuple('VarAccid', 'account accountid')

def p_var_accid(p):
    """var_accid : VAR OFX ACCID STRING ACCOUNT
    """
    p[0] = VarAccid(*p[4:])

DefAccount = namedtuple('DefAccount', 'account drcr currency_list')

def p_defaccount(p):
    """defaccount : DEFACCOUNT DRCR ACCOUNT currency_list
    """
    p[0] = DefAccount(*p[2:])

Pad = namedtuple('Pad', 'date account_to account_from')

def p_pad(p):
    """pad : PAD DATE ACCOUNT ACCOUNT
    """
    p[0] = Pad(*p[2:])

Check = namedtuple('Check', 'date account amount')

def p_check(p):
    """check : CHECK DATE ACCOUNT amount
    """
    p[0] = Check(*p[2:])

Amount = namedtuple('Amount', 'number currency')

def p_amount(p):
    """amount : NUMBER CURRENCY
    """
    p[0] = Amount(p[1], p[2])

Lot = namedtuple('Lot', 'amount cost')

def p_lot(p):
    """lot : amount
           | amount LCURL amount RCURL
    """
    cost = p[3] if len(p) > 2 else None
    p[0] = Lot(p[1], cost)

Price = namedtuple('Price', 'price date currency amount')

def p_price(p):
    """price : PRICE DATE CURRENCY amount
    """
    p[0] = Price(*p[1:])

Location = namedtuple('Location', 'date city')

def p_location(p):
    """location : LOCATION DATE STRING
    """
    p[0] = Location(*p[2:])

def p_entry(p):
    """entry : EOL
             | transaction
             | defaccount
             | var_accid
             | pad
             | check
             | begintag
             | endtag
             | price
             | location
    """
    p[0] = p[1]

def p_entry_list(p):
    """entry_list : empty
                  | entry_list entry
    """
    handle_varlen(p)

def p_error(p):
    if p is None:
        return logging.error("%s:%d: Error in input." % ('', 0))
    logging.error("%s:%d: Syntax error" % (p.lexer.filename, p.lineno))

start = 'entry_list'



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
